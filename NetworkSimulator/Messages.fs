namespace Jacques.NetworkSimulator

type RouterTableEntry = string * string * string

type InputCommand = string * string * string

type PingAsk = unit

// icmp echo request: mac destino, mac origem, tipo, ip origem, ip destino, ttl, tipo pacote, codigo
// ex: 05, 01, ip, 192.168.0.2, 192.168.1.2, 8, icmp, echo req
// ex2: 03, 06, ip, 192.168.0.2, 192.168.1.2, 7, icmp, echo req
type ICMPEchoRequest = 
    { MACSource : string
      MACDestination : string
      Type : string
      IPSource : string
      IPDestination : string
      TTL : int
      PackageType : string
      Code : string }

// icmp echo reply: 01, 05, 192.168.1.2, 192.168.0.2, 7, icmp, echo reply
type ICMPEchoReply = 
    { MACSource : string
      MACDestination : string
      Type : string
      IPSource : string
      IPDestination : string
      TTL : int
      PackageType : string
      Code : string }

// mac origem, mac destino, ip origem, ip destino, ttl???
type ICMPTimeExceeded = 
    { MACSource : string
      MACDestination : string
      Type : string
      IPSource : string
      IPDestination : string
      TTL : int
      PackageType : string
      Code : string }

// mac origem, mac destino, ip origem, ip destino
type ARPRequest = 
    { MACSource : string
      MACDestination : string
      IPSource : string
      IPDestination : string }

// mac origem, mac destino, ip origem, ip destino
type ARPReply = 
    { MACSource : string
      MACDestination : string
      IPSource : string
      IPDestination : string }
