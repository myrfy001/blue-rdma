# coding: utf-8

import sys
import scapy.all
from scapy.layers.l2 import Ether
from scapy.layers.inet import IP, UDP
import scapy

input_file = sys.argv[1]
output_file = sys.argv[2]

total_bytes = []
with open(input_file) as fi:
    for line in fi:
        line = line.strip()

        raw_bytes = [int(line[idx: idx+2], 16)
                     for idx in range(0, len(line), 2)]
        print(raw_bytes)
        raw_bytes.reverse()
        total_bytes.extend(raw_bytes)

eth_header = Ether(bytes(total_bytes))

print(eth_header)

# Handle IP header
ip_header = IP(bytes(eth_header.payload))
print(ip_header)
print("IP Header:")
print("Source IP:", ip_header.src)
print("Destination IP:", ip_header.dst)
print("Protocol:", ip_header.proto)

# Handle UDP header
udp_header = UDP(bytes(ip_header.payload))
print("UDP Header:")
print("Source Port:", udp_header.sport)
print("Destination Port:", udp_header.dport)
print("Length:", udp_header.len)

scapy.all.wrpcap(output_file, eth_header)
