namespace Jacques.NetworkSimulator

open Akka.Actor
open System.Collections.Generic

[<AutoOpen>]
module Actors = 
    type Hosts() =
        inherit UntypedActor()
        member val HostsFile = List<HostsEntry>() with get, set

        override m.OnReceive msg =
            match msg with
            | :? HostsEntry as entry -> m.HostsFile.Add entry
            | :? HostsAsk as ask ->
                match ask.Information with
                | "name" ->
                    let name = fst (List.find (fun ((_,y):HostsEntry) -> y = ask.Value) (List.ofSeq m.HostsFile))
                    UntypedActor.Context.Sender.Tell(name)
                | "ip" ->
                    let ip = snd (List.find (fun ((x,_):HostsEntry) -> x = ask.Value) (List.ofSeq m.HostsFile))
                    UntypedActor.Context.Sender.Tell(ip)
                | _ -> failwith ("Incorrect message: " + msg.ToString())
            | _ -> failwith ("Incorrect message: " + msg.ToString())

    type Node(mac, ip : string, gateway, hosts : IActorRef) = 
        inherit UntypedActor()
        member val ARPTable = List<ARPTableEntry>() with get, set
        member m.MAC = mac
        member m.IP = (ip.Split '/').[0]
        member m.Mask = (ip.Split '/').[1]
        member m.Gateway = gateway
        member m.HostsFile = hosts
        
        override m.OnReceive msg = 
            match msg with
            | :? InputCommand as input -> 
                let comm, _, destination = input
                
                let originalDestIP = hosts.Ask({ Information = "ip"; Value = destination }).Result :?> string

                let sendTo, destIP =
                    match sameSubnet m.IP originalDestIP m.Mask with
                    | true ->
                        UntypedActor.Context.ActorSelection("../" + destination), originalDestIP
                    | false ->
                        let name = hosts.Ask({ Information = "name"; Value = m.Gateway }).Result :?> string
                        UntypedActor.Context.ActorSelection("../" + name), m.Gateway

                let destMAC, packetType, packetCode = "FF:FF:FF:FF:FF:FF", PacketType.ARP, PacketCode.ARPRequest
                
                let arpPacket = Packet(m.MAC, destMAC, m.IP, destIP, 0, packetType, packetCode)
                
                match comm with
                | "ping" ->
                    let arpResponse = sendTo.Ask(arpPacket).Result :?> Packet
                    arpResponse.Print()
                    m.ARPTable.Add (arpResponse.IPSource, arpResponse.MACSource)
                    
                    let icmpPacket = Packet(m.MAC, arpResponse.MACSource, m.IP, originalDestIP, 8, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                    let icmpResponse = sendTo.Ask(icmpPacket).Result :?> Packet
                    icmpResponse.Print()

                | "traceroute" ->
                    let arpResponse = sendTo.Ask(arpPacket).Result :?> Packet
                    arpResponse.Print()
                    m.ARPTable.Add (arpResponse.IPSource, arpResponse.MACSource)

                    let mutable ttl = 1
                    let mutable finish = false

                    while (ttl < 9 && not finish) do
                        let icmpPacket = Packet(m.MAC, arpResponse.MACSource, m.IP, originalDestIP, ttl, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                        let icmpResponse = sendTo.Ask(icmpPacket).Result :?> Packet
                        icmpResponse.Print()
                        
                        if icmpResponse.IPSource = originalDestIP then
                            finish <- true
                        else
                            ttl <- ttl + 1

                | _ -> failwith ("Invalid command: " + comm)
            
            | :? Packet as packet ->
                packet.Print()

                match packet.Type with
                | PacketType.ARP ->
                    match packet.Code with
                    | PacketCode.ARPRequest ->
                        let response = Packet(m.MAC, packet.MACSource, m.IP, packet.IPSource, 0, PacketType.ARP, PacketCode.ARPReply)
                        m.ARPTable.Add (packet.IPSource, packet.MACSource)
                        UntypedActor.Context.Sender.Tell(response)
                    | _ -> failwith ("Unexpected packet type:" + packet.Type.ToString())

                | PacketType.ICMP ->
                    match packet.Code with
                    | PacketCode.ICMPEchoRequest ->
                        let response = Packet(m.MAC, packet.MACSource, m.IP, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPEchoReply)
                        UntypedActor.Context.Sender.Tell(response)
                    | _ -> failwith ("Unexpected packet type:" + packet.Type.ToString())

            | _ -> failwith ("Incorrect message: " + msg.ToString())
    
    type Router(addresses : (string * string) list, hosts : IActorRef) = 
        inherit UntypedActor()
        member val RouterTable = List<RouterTableEntry>() with get, set
        member val ARPTable = List<ARPTableEntry>() with get, set
        member m.Ports = addresses
        member m.HostsFile = hosts

        override m.OnReceive msg = 
            match msg with
            | :? RouterTableEntry as entry -> m.RouterTable.Add entry

            | :? Packet as packet ->
                packet.Print()

                match packet.Type with
                | PacketType.ARP ->
                    match packet.Code with
                    | PacketCode.ARPRequest ->
                        let macResponse = fst (List.find (fun (_,y) -> y = packet.IPDestination) m.Ports)
                        let response = Packet(macResponse, packet.MACSource, packet.IPDestination, packet.IPSource, 0, PacketType.ARP, PacketCode.ARPReply)
                        m.ARPTable.Add (packet.IPSource, packet.MACSource)
                        UntypedActor.Context.Sender.Tell(response)
                    | _ -> failwith "???"

                | PacketType.ICMP ->
                    if packet.TTL-1 <= 0 then
                        let _, _, port = List.find (fun (x:string,_,_) -> sameSubnet packet.IPSource ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable)
                        let _, ipSource = m.Ports.[int(port)]
                        let timeExceeded = Packet(packet.MACDestination, packet.MACSource, ipSource, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPTimeExceeded)
                        UntypedActor.Context.Sender.Tell(timeExceeded)
                    else
                        match packet.Code with
                        | PacketCode.ICMPEchoRequest ->
                            match List.tryFind (fun (_,y) -> y = packet.IPDestination) m.Ports with
                            | Some (mac,ip) -> // ping/traceroute to router - return imcp echo reply
                                let response = Packet(mac, packet.MACSource, ip, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPEchoReply)
                                UntypedActor.Context.Sender.Tell(response)
                            | None -> // forward ping/traceroute somewhere else
                                match List.tryFind (fun (x, y) -> x = packet.IPDestination) (List.ofSeq m.ARPTable) with
                                | Some (mac, ip) -> // known mac address - forward icmp echo request
                                    let _, _, port = List.find (fun (x:string,_,_) -> sameSubnet packet.IPDestination ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable)
                                    let macSource, ipSource = m.Ports.[int(port)]

                                    let forward = Packet(macSource, mac, ipSource, ip, packet.TTL-1, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                                    let forwardName = m.HostsFile.Ask({ Information = "name"; Value = ip }).Result :?> string
                                    let forwardResponse = UntypedActor.Context.ActorSelection("../" + forwardName).Ask(forward).Result :?> Packet
                                    forwardResponse.Print()

                                    let response =
                                        match forwardResponse.Code with
                                        | PacketCode.ICMPEchoReply ->
                                            Packet(macSource, packet.MACSource, ipSource, packet.IPSource, packet.TTL-1, PacketType.ICMP, PacketCode.ICMPEchoReply)
                                        | PacketCode.ICMPTimeExceeded ->
                                            Packet(packet.MACDestination, packet.MACSource, forwardResponse.IPSource, forwardResponse.IPDestination, forwardResponse.TTL-1, PacketType.ICMP, PacketCode.ICMPTimeExceeded)
                                        | _ -> failwith ("Unexpected packet type:" + packet.Type.ToString())

                                    UntypedActor.Context.Sender.Tell(response)

                                | None -> // unknown mac, arp request and then forward icmp
                                    let network, nextHop, port = 
                                        match List.tryFind (fun (x:string,_,_) -> sameSubnet packet.IPDestination ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable) with
                                        | Some (x,y,z) -> x,y,z // found next hop in router table
                                        | None -> match List.tryFind (fun (x:string,_,_) -> x = "0.0.0.0/0") (List.ofSeq m.RouterTable) with
                                            | Some (x,y,z) -> x,y,z // found default gateway
                                            | None -> failwith (UntypedActor.Context.Self.Path.Name + " has nowhere to send packet. Router table is incomplete.")

                                    let dest =
                                        match (nextHop.Split '/').[0] with
                                        | "0.0.0.0" -> packet.IPDestination
                                        | ip -> ip

                                    let macSource, ipSource = m.Ports.[int(port)]

                                    let arpPacket = Packet(macSource, "FF:FF:FF:FF:FF:FF", ipSource, dest, 0, PacketType.ARP, PacketCode.ARPRequest)
                                    let arpName = m.HostsFile.Ask({ Information = "name"; Value = dest }).Result :?> string
                                    let arpResponse = UntypedActor.Context.ActorSelection("../" + arpName).Ask(arpPacket).Result :?> Packet
                                    arpResponse.Print()
                                    m.ARPTable.Add (arpResponse.IPSource, arpResponse.MACSource)

                                    let icmpPacket = Packet(macSource, arpResponse.MACSource, packet.IPSource, packet.IPDestination, packet.TTL-1, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                                    let icmpResponse = UntypedActor.Context.ActorSelection("../" + arpName).Ask(icmpPacket).Result :?> Packet
                                    icmpResponse.Print()

                                    let response =
                                        match icmpResponse.Code with
                                        | PacketCode.ICMPEchoReply ->
                                            Packet(packet.MACDestination, packet.MACSource, packet.IPDestination, packet.IPSource, icmpResponse.TTL-1, PacketType.ICMP, PacketCode.ICMPEchoReply)
                                        | PacketCode.ICMPTimeExceeded ->
                                            Packet(packet.MACDestination, packet.MACSource, icmpResponse.IPSource, icmpResponse.IPDestination, icmpResponse.TTL-1, PacketType.ICMP, PacketCode.ICMPTimeExceeded)
                                        | _ -> failwith ("Unexpected packet type:" + packet.Type.ToString())
                                    UntypedActor.Context.Sender.Tell(response)
                                    
                        | _ -> failwith ("Unexpected packet type:" + packet.Type.ToString())

            | _ -> failwith ("Incorrect message: " + msg.ToString())
    
    let private createNode (node : string) (system : ActorSystem) (hosts : IActorRef) = 
        let split = node.Split ','
        let name = split.[0]
        let mac = split.[1]
        let ip = split.[2]
        let gateway = split.[3]

        system.ActorOf(Props.CreateFun(fun () -> Node(mac, ip, gateway, hosts)), name) |> ignore
        hosts.Tell(name, (ip.Split '/').[0]:HostsEntry)
    
    let private createRouter (router : string) (system : ActorSystem) (hosts : IActorRef)= 
        let split = router.Split ','
        let name = split.[0]
        let portsList = new List<string * string>()

        for i in 2..2..split.Length-2 do
            portsList.Add(split.[i], ((split.[i + 1]).Split '/').[0])
            hosts.Tell(name, ((split.[i+1]).Split '/').[0]:HostsEntry)

        system.ActorOf(Props.CreateFun(fun () -> Router(portsList |> List.ofSeq, hosts)), name) |> ignore
    
    let private createRouterTable (routerTable : string) (system : ActorSystem) = 
        let split = routerTable.Split ','
        let router = system.ActorSelection("user/" + split.[0])
        router.Tell(split.[1], split.[2], split.[3] : RouterTableEntry)
    
    let createActors (topology : string list) (system : ActorSystem) = 
        let hosts = system.ActorOf(Props.CreateFun(fun () -> Hosts()), "HostsFile")
        let nodeStart = List.findIndex (fun x -> obj.Equals(x, "#NODE")) topology
        let routerStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTER")) topology
        let routerTableStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTERTABLE")) topology
        let nodeList = topology.GetSlice(Some(nodeStart + 1), Some(routerStart - 1))
        let routerList = topology.GetSlice(Some(routerStart + 1), Some(routerTableStart - 1))
        let routerTableList = topology.GetSlice(Some(routerTableStart + 1), Some(topology.Length - 1))
        
        for node in nodeList do
            createNode node system hosts
        for router in routerList do
            createRouter router system hosts
        for routerTable in routerTableList do
            createRouterTable routerTable system
