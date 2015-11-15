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

        member val TracerouteMode = false with get, set
        member val TracerouteDestinationIP = "" with get, set
        member val TracerouteTTL = 1 with get, set
        member val TracerouteActorPath = Unchecked.defaultof<ActorSelection> with get, set
        member val TracerouteFistHopMac = "" with get, set
        
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
                    sendTo.Tell(icmpPacket)

                | "traceroute" ->
                    let arpResponse = sendTo.Ask(arpPacket).Result :?> Packet
                    arpResponse.Print()
                    m.ARPTable.Add(arpResponse.IPSource, arpResponse.MACSource)

                    m.TracerouteFistHopMac <- arpResponse.MACSource
                    m.TracerouteMode <- true
                    m.TracerouteDestinationIP <- originalDestIP
                    m.TracerouteActorPath <- sendTo

                    let icmpPacket = Packet(m.MAC, arpResponse.MACSource, m.IP, originalDestIP, 1, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                    sendTo.Tell(icmpPacket)
                        
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
                        if packet.IPDestination = m.IP then
                            let response = Packet(m.MAC, packet.MACSource, m.IP, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPEchoReply)
                            UntypedActor.Context.Sender.Tell(response)
                        else
                            failwith (UntypedActor.Context.Self.Path.Name + " received a ICMP_ECHOREQUEST not addressed to itself.")
                    | _ ->
                        if packet.IPSource = m.TracerouteDestinationIP then
                            m.TracerouteMode <- false
                        else if m.TracerouteMode && m.TracerouteTTL < 8 then
                            m.TracerouteTTL <- m.TracerouteTTL + 1
                            let icmpPacket = Packet(m.MAC, m.TracerouteFistHopMac, m.IP, m.TracerouteDestinationIP, m.TracerouteTTL, PacketType.ICMP, PacketCode.ICMPEchoRequest)
                            m.TracerouteActorPath.Tell(icmpPacket)

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
                    | _ -> failwith ("Unexpected packet code:" + packet.Code.ToString())

                | PacketType.ICMP ->
                    if packet.TTL-1 <= 0 then
                        let _, _, port =
                            match List.tryFind (fun (x:string,_,_) -> sameSubnet packet.IPDestination ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable) with
                            | Some (x,y,z) -> x,y,z // found next hop in router table
                            | None ->
                                match List.tryFind (fun (x:string,_,_) -> x = "0.0.0.0/0") (List.ofSeq m.RouterTable) with
                                | Some (x,y,z) -> x,y,z // found default gateway
                                | None -> failwith (UntypedActor.Context.Self.Path.Name + " has nowhere to send packet. Router table is incomplete.")
                        let _, ipSource = m.Ports.[int(port)]
                        let timeExceeded = Packet(packet.MACDestination, packet.MACSource, ipSource, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPTimeExceeded)
                        UntypedActor.Context.Sender.Tell(timeExceeded)
                    else
                        match List.tryFind (fun (_,y) -> y = packet.IPDestination) m.Ports with
                        | Some (mac,ip) -> // ping/traceroute to router - return imcp echo reply
                            match packet.Code with
                            | PacketCode.ICMPEchoRequest ->
                                let response = Packet(mac, packet.MACSource, ip, packet.IPSource, 8, PacketType.ICMP, PacketCode.ICMPEchoReply)
                                UntypedActor.Context.Sender.Tell(response)
                            | _ -> ()
                        | None -> // forward ping/traceroute somewhere else
                            match List.tryFind (fun (x, y) -> x = packet.IPDestination) (List.ofSeq m.ARPTable) with
                            | Some (ip, mac) -> // known mac address - forward icmp echo request
                                let _, _, port = List.find (fun (x:string,_,_) -> sameSubnet packet.IPDestination ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable)
                                let macSource, ipSource = m.Ports.[int(port)]

                                let forward = Packet(macSource, mac, packet.IPSource, packet.IPDestination, packet.TTL-1, PacketType.ICMP, packet.Code)
                                let forwardName = m.HostsFile.Ask({ Information = "name"; Value = ip }).Result :?> string
                                    
                                UntypedActor.Context.ActorSelection("../" + forwardName).Tell(forward)

                            | None -> // unknown mac, arp request and then forward icmp
                                let network, nextHop, port = 
                                    match List.tryFind (fun (x:string,_,_) -> sameSubnet packet.IPDestination ((x.Split '/').[0]) ((x.Split '/').[1])) (List.ofSeq m.RouterTable) with
                                    | Some (x,y,z) -> x,y,z // found next hop in router table
                                    | None ->
                                        match List.tryFind (fun (x:string,_,_) -> x = "0.0.0.0/0") (List.ofSeq m.RouterTable) with
                                        | Some (x,y,z) -> x,y,z // found default gateway
                                        | None -> failwith (UntypedActor.Context.Self.Path.Name + " has nowhere to send packet. Router table is incomplete.")

                                let dest =
                                    match (nextHop.Split '/').[0] with
                                    | "0.0.0.0" -> packet.IPDestination
                                    | ip -> ip

                                let macSource, ipSource = m.Ports.[int(port)]

                                // check arp again if message is being sent to the default gateway
                                match List.tryFind (fun (x, y) -> x = dest) (List.ofSeq m.ARPTable) with
                                | Some (_, mac) ->
                                    let arpName = m.HostsFile.Ask({ Information = "name"; Value = dest }).Result :?> string
                                    let icmpPacket = Packet(macSource, mac, packet.IPSource, packet.IPDestination, packet.TTL-1, PacketType.ICMP, packet.Code)
                                    UntypedActor.Context.ActorSelection("../" + arpName).Tell(icmpPacket)
                                | None ->
                                    let arpPacket = Packet(macSource, "FF:FF:FF:FF:FF:FF", ipSource, dest, 0, PacketType.ARP, PacketCode.ARPRequest)
                                    let arpName = m.HostsFile.Ask({ Information = "name"; Value = dest }).Result :?> string
                                    let arpResponse = UntypedActor.Context.ActorSelection("../" + arpName).Ask(arpPacket).Result :?> Packet
                                    arpResponse.Print()
                                    m.ARPTable.Add(arpResponse.IPSource, arpResponse.MACSource)

                                    let icmpPacket = Packet(macSource, arpResponse.MACSource, packet.IPSource, packet.IPDestination, packet.TTL-1, PacketType.ICMP, packet.Code)
                                    UntypedActor.Context.ActorSelection("../" + arpName).Tell(icmpPacket)
                                    
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
