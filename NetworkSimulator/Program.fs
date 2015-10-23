﻿namespace Jacques.NetworkSimulator

open Akka
open Akka.Actor
open System
open System.IO

module Main =
    [<EntryPoint>]
    let main argv = 
        let topology = File.ReadAllLines argv.[0] |> Array.toList
        let system = ActorSystem.Create "NetworkSimulator"
        CreateActors topology system

        match argv.[1] with
        | "ping" -> system.ActorSelection("user/"+argv.[2]).Tell("ping", argv.[2], argv.[3] : InputCommand)
        | "traceroute" -> system.ActorSelection("user/"+argv.[2]).Tell("traceroute", argv.[2], argv.[3] : InputCommand)
        | _ -> failwith("Invalid command: " + argv.[1])

        Console.ReadKey() |> ignore
        0
