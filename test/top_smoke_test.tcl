package require Bluesim
namespace import Bluesim::*

sim load build/mkTestTop.sh.so mkTestTop
sim run
set mem_handle [sim lookup fakeXdmaA_hostMem_memory]

puts "///MEMORY DUMP BEGIN///"
for {set idx 0} {$idx<1048575} {incr idx} {
    puts [sim getrange $mem_handle $idx ]
}