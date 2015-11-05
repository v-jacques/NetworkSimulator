namespace Jacques.NetworkSimulator

open Akka
open Akka.Actor
open Akka.FSharp
open System
open System.Collections.Generic

[<AutoOpen>]
module Actors = 
    type Node(mac, ip : string, gateway) = 
        inherit UntypedActor()
        member m.MAC = mac
        member m.IP = (ip.Split '/').[0]
        member m.Mask = (ip.Split '/').[1]
        member m.Gateway = gateway
        // nome, mac, ip, subrede, gateway padrao, arp table
        // arp table: mac, ip
        // ex: 05, 192.168.0.1
        override m.OnReceive msg = 
            match msg with
            | :? InputCommand as input -> 
                let comm, source, destination = input
                //                if SameSubNetwork source destination m.Mask then ()
                //                else ()
                if comm = "ping" then ()
                else if comm = "traceroute" then ()
                else failwith ("Invalid command: " + comm)
            | _ -> failwith ("Incorrect message: " + msg.ToString())
    
    type Router(addresses : (string * string) list) = 
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
            | _ -> failwith ("Incorrect message: " + msg.ToString())
    
    let private createNode (node : string) (system : ActorSystem) = 
        let split = node.Split ','
        system.ActorOf(Props.CreateFun(fun () -> Node(split.[1], split.[2], split.[3])), split.[0]) |> ignore
    
    let private createRouter (router : string) (system : ActorSystem) = 
        let split = router.Split ','
        let portsList = new List<string * string>()
        for i = 0 to Int32.Parse split.[1] do
            portsList.Add(split.[2 + i], split.[3 + i])
        system.ActorOf(Props.CreateFun(fun () -> Router(portsList |> List.ofSeq)), split.[0]) |> ignore
    
    let private createRouterTable (routerTable : string) (system : ActorSystem) = 
        let split = routerTable.Split ','
        let router = system.ActorSelection("user/" + split.[0])
        router.Tell(split.[1], split.[2], split.[3] : RouterTableEntry)
    
    let createActors (topology : string list) (system : ActorSystem) = 
        let nodeStart = List.findIndex (fun x -> obj.Equals(x, "#NODE")) topology
        let routerStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTER")) topology
        let routerTableStart = List.findIndex (fun x -> obj.Equals(x, "#ROUTERTABLE")) topology
        let nodeList = topology.GetSlice(Some(nodeStart + 1), Some(routerStart - 1))
        let routerList = topology.GetSlice(Some(routerStart + 1), Some(routerTableStart - 1))
        let routerTableList = topology.GetSlice(Some(routerTableStart + 1), Some(topology.Length - 1))
        for node in nodeList do
            createNode node system
        for router in routerList do
            createRouter router system
        for routerTable in routerTableList do
            createRouterTable routerTable system
