namespace Jacques.NetworkSimulator.Actors

open Akka
open Akka.Actor

// mensagems: arp request, arp reply, icmp echo request, ping, icmp echo reply

// icmp echo request: mac destino, mac origem, tipo, ip origem, ip destino, ttl, tipo pacote, codigo
// ex: 05, 01, ip, 192.168.0.2, 192.168.1.2, 8, icmp, echo req
// ex2: 03, 06, ip, 192.168.0.2, 192.168.1.2, 7, icmp, echo req

// icmp echo reply: 01, 05, 192.168.1.2, 192.168.0.2, 7, icmp, echo reply

type Node() =
    inherit UntypedActor()

    // nome, mac, ip, subrede, gateway padrao, arp table
    // arp table: mac, ip
    // ex: 05, 192.168.0.1

    override m.OnReceive msg =
        ()

type Router() =
    inherit UntypedActor()

    // interfaces, router table, arp table
    // arp table: mac, ip
    // ex: 01, 192.168.0.2
    //router table: destino, next ho, int
    // ex1: 192.168.0.0/24, 0.0.0.0, 0
    // ex2: 192.168.1.0/24, 0.0.0.0, 1

    override m.OnReceive msg =
        ()
