namespace Jacques.NetworkSimulator

open Akka
open Akka.Actor
open Akka.FSharp
open System
open System.Collections.Generic

// mensagems: arp request, arp reply, icmp echo request, ping, icmp echo reply

// icmp echo request: mac destino, mac origem, tipo, ip origem, ip destino, ttl, tipo pacote, codigo
// ex: 05, 01, ip, 192.168.0.2, 192.168.1.2, 8, icmp, echo req
// ex2: 03, 06, ip, 192.168.0.2, 192.168.1.2, 7, icmp, echo req

// icmp echo reply: 01, 05, 192.168.1.2, 192.168.0.2, 7, icmp, echo reply

[<AutoOpen>]
module Actors =
    type internal FunProps<'T when 'T :> ActorBase>(fn: unit -> 'T, ?supervisorStrategy: SupervisorStrategy) =
        inherit Props(typeof<'T>, defaultArg supervisorStrategy null, null)
        override m.NewActor() = upcast fn()

    type Props with
        static member CreateFun<'T when 'T :> ActorBase>(fn: unit -> 'T, ?supervisorStrategy: SupervisorStrategy) = 
            FunProps(fn, ?supervisorStrategy = supervisorStrategy) :> Props
    
    type Node(mac, ip, gateway) =
        inherit UntypedActor()

        // nome, mac, ip, subrede, gateway padrao, arp table
        // arp table: mac, ip
        // ex: 05, 192.168.0.1

        override m.OnReceive msg =
            match msg with
            | :? InputCommand as input -> ()
            | _ -> failwith("Incorrect message: " + msg.ToString())

    type Router(addresses: (string * string) list) =
        inherit UntypedActor()

        member val RouterTable = List<RouterTableEntry>() with get, set
        member m.Ports = addresses

        // interfaces, router table, arp table
        // arp table: mac, ip
        // ex: 01, 192.168.0.2
        //router table: destino, next ho, int
        // ex1: 192.168.0.0/24, 0.0.0.0, 0
        // ex2: 192.168.1.0/24, 0.0.0.0, 1

        override m.OnReceive msg =
            match msg with
            | :? RouterTableEntry as entry -> m.RouterTable.Add entry
            | _ -> failwith("Incorrect message: " + msg.ToString())

    let private CreateNode (node:string) (system:ActorSystem) =
        let split = node.Split ','
        system.ActorOf(Props.CreateFun(fun () -> Node(split.[1], split.[2], split.[3])), split.[0]) |> ignore

    let private CreateRouter (router:string) (system:ActorSystem) =
        let split = router.Split ','
        let portsList = new List<string * string>()
        
        for i = 0 to Int32.Parse split.[1] do
            portsList.Add(split.[2+i], split.[3+i])

        system.ActorOf(Props.CreateFun(fun () -> Router(portsList |> List.ofSeq)), split.[0]) |> ignore

    let private CreateRouterTable (routerTable:string) (system:ActorSystem) =
        let split = routerTable.Split ','
        let router = system.ActorSelection("user/"+split.[0])
        router.Tell(split.[1], split.[2], split.[3] : RouterTableEntry)

    let CreateActors (topology:string list) (system:ActorSystem) =
        let nodeStart = List.findIndex (fun x -> obj.Equals(x, "#NODE")) topology
        let routerStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTER")) topology
        let routerTableStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTERTABLE")) topology
        
        let nodeList = topology.GetSlice(Some (nodeStart+1), Some (routerStart-1))
        let routerList = topology.GetSlice(Some (routerStart+1), Some (routerTableStart-1))
        let routerTableList = topology.GetSlice(Some (routerTableStart+1), Some (topology.Length-1))

        for node in nodeList do
            CreateNode node system
        for router in routerList do
            CreateRouter router system
        for routerTable in routerTableList do
            CreateRouterTable routerTable system
    