namespace Jacques.NetworkSimulator

type HostsEntry = string * string

type HostsAsk = 
    { Information : string
      Value : string }

type ARPTableEntry = string * string

type RouterTableEntry = string * string * string

type InputCommand = string * string * string

type PacketType = 
    | ICMP
    | ARP

type PacketCode = 
    | ICMPEchoRequest
    | ICMPEchoReply
    | ICMPTimeExceeded
    | ARPRequest
    | ARPReply

type Packet(macSource, macDest, ipSource, ipDest, ttl, typ, code) = 
    member m.MACSource = macSource
    member m.MACDestination = macDest
    member m.IPSource = ipSource
    member m.IPDestination = ipDest
    member m.TTL = ttl
    member m.Type = typ
    member m.Code = code
    member m.Print() = 
        match m.Type with
        | PacketType.ARP -> 
            let code = 
                match m.Code with
                | PacketCode.ARPRequest -> "ARP_REQUEST"
                | PacketCode.ARPReply -> "ARP_REPLY"
                | _ -> failwith "Incorrect packet code: " + m.Code.ToString()
            printfn "%s|%s,%s|%s,%s" code m.MACSource m.MACDestination m.IPSource m.IPDestination
        | PacketType.ICMP -> 
            let code = 
                match m.Code with
                | PacketCode.ICMPEchoRequest -> "ICMP_ECHOREQUEST"
                | PacketCode.ICMPEchoReply -> "ICMP_ECHOREPLY"
                | PacketCode.ICMPTimeExceeded -> "ICMP_TIMEEXCEEDED"
                | _ -> failwith "Incorrect packet code: " + m.Code.ToString()
            printfn "%s|%s,%s|%s,%s|%i" code m.MACSource m.MACDestination m.IPSource m.IPDestination m.TTL
