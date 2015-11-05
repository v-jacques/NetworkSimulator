namespace Jacques.NetworkSimulator

open Akka.Actor

[<AutoOpen>]
module Util = 
    let rec binaryToBinaryOctet (i : string) = 
        match i.Length with
        | a when a < 8 -> binaryToBinaryOctet ("0" + i)
        | a when a = 8 -> i
        | _ -> failwith ("Can't parse binary number into an octet: " + i)
    
    let rec intToBinary i = 
        match i with
        | 0 | 1 -> string i
        | _ -> 
            let bit = string (i % 2)
            (intToBinary (i / 2)) + bit
    
    let rec stringToBinaryOctets (i : string) = 
        i.Split('.')
        |> Array.map (int
                      >> intToBinary
                      >> binaryToBinaryOctet)
        |> String.concat ""
    
    let getIPClass (ip : string) = 
        match int (ip.Split('.').[0]) with
        | a when a < 128 -> "A"
        | b when b < 192 -> "B"
        | c when c < 224 -> "C"
        | d when d < 240 -> "D"
        | _ -> "E"
    
    let getDefaultMask ipClass = 
        match ipClass with
        | "A" -> "255.0.0.0"
        | "B" -> "255.255.0.0"
        | "C" -> "255.255.255.0"
        | _ -> failwith "Error getting default subnet mask."
    
    let getSubnetRange ip mask = 
        let ipClass = getIPClass ip
        let defaultMask = getDefaultMask ipClass
        ()
    
    let sameSubnet ip1 ip2 mask = ()
    
    type internal FunProps<'T when 'T :> ActorBase>(fn : unit -> 'T, ?supervisorStrategy : SupervisorStrategy) = 
        inherit Props(typeof<'T>, defaultArg supervisorStrategy null, null)
        override m.NewActor() = upcast fn()
    
    type Props with
        static member CreateFun<'T when 'T :> ActorBase>(fn : unit -> 'T, ?supervisorStrategy : SupervisorStrategy) = 
            FunProps(fn, ?supervisorStrategy = supervisorStrategy) :> Props
