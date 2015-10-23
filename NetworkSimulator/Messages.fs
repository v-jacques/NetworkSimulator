namespace Jacques.NetworkSimulator

// arp request, arp reply, icmp echo req, ping

type RouterTableEntry = string * string * string
type AddRouterTableEntry = RouterTableEntry
type InputCommand = string * string * string