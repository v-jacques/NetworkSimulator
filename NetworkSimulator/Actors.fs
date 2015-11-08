namespace Jacques.NetworkSimulator

open Akka
open Akka.Actor
open Akka.FSharp
open System
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
                
                let destIP = hosts.Ask({ Information = "ip"; Value = destination }).Result :?> string

                let sendTo =
                    match sameSubnet m.IP destIP m.Mask with
                    | true -> UntypedActor.Context.ActorSelection("../" + destination)
                    | false ->
                        let name = hosts.Ask({ Information = "name"; Value = m.Gateway }).Result :?> string
                        UntypedActor.Context.ActorSelection("../" + name)

                let destMAC, packetType, packetCode =
                    match List.tryFind (fun ((_,y):ARPTableEntry) -> y = destIP) (List.ofSeq m.ARPTable) with
                    | Some (x,_) -> x, PacketType.ICMP, PacketCode.ICMPEchoRequest
                    | None -> "FF:FF:FF:FF:FF:FF", PacketType.ARP, PacketCode.ARPRequest
                
                let packet = Packet(m.MAC, destMAC, m.IP, destIP, 0, packetType, packetCode)
                
                match comm with
                | "ping" ->
                    sendTo.Tell(packet)
                | "traceroute" -> failwith "Not implemented."
                | _ -> failwith ("Invalid command: " + comm)
            | :? Packet as packet ->
                packet.Print()
                match packet.Type with
                | PacketType.ARP -> ()
                    //match code
                | PacketType.ICMP -> ()
                    //match code
            | _ -> failwith ("Incorrect message: " + msg.ToString())
    
    type Router(addresses : (string * string) list) = 
        inherit UntypedActor()
        member val RouterTable = List<RouterTableEntry>() with get, set
        member val ARPTable = List<ARPTableEntry>() with get, set
        member m.Ports = addresses
        
        override m.OnReceive msg = 
            match msg with
            | :? RouterTableEntry as entry -> m.RouterTable.Add entry
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
        let portsNumber = Int32.Parse split.[1]
        let portsList = new List<string * string>()

        for i = 0 to portsNumber do
            portsList.Add(split.[2 + i], split.[3 + i])
            hosts.Tell(name, split.[3 + i]:HostsEntry)

        system.ActorOf(Props.CreateFun(fun () -> Router(portsList |> List.ofSeq)), name) |> ignore
    
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
