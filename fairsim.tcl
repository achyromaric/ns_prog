# Copyright (c) 1997 Regents of the University of California.
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. All advertising materials mentioning features or use of this software
#    must display the following acknowledgement:
#      This product includes software developed by the Computer Systems
#      Engineering Group at Lawrence Berkeley Laboratory.
# 4. Neither the name of the University nor of the Laboratory may be used
#    to endorse or promote products derived from this software without
#    specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED.  IN NO EVENT SHALL THE REGENTS OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
# OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
# HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
# LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
# OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
# SUCH DAMAGE.
#


# Script to Study Unfairness/Starvation in IEEE 802.11 
# V.B., March 2005  

# ======================================================================
# Define options
# ======================================================================
set val(chan)           Channel/WirelessChannel    ;# channel type
set val(prop)           Propagation/TwoRayGround   ;# radio-propagation model
set val(netif)          Phy/WirelessPhy            ;# network interface type
set val(mac)            Mac/802_11                 ;# MAC type
set val(ifq)            Queue/DropTail/PriQueue    ;# interface queue type
set val(ll)             LL                         ;# link layer type
set val(ant)            Antenna/OmniAntenna        ;# antenna model
set val(ifqlen)         50                         ;# max packet in ifq
set val(nn)             4                          ;# number of mobilenodes
set val(rp)           AODV                    ;# routing protocol AODV
set val(sc)		"./scenario"		   ;# scenario file
set val(x)		3000.0			   ;
set val(y)		400.0			   ;
set val(simtime)	10.0			   ; #sim time
set val(drate)		2.0e6			   ; #default datarate
set val(dist)		150.0			   ; #default datarate
# ======================================================================
#DEFINITION DE LA COMMANDE POUR LANCER LA SIMULATION
if { $argc < 2} {
        puts "Wrong no. of cmdline args."
        puts "Usage: ns sim.tcl -dist <dist>"
        exit 0
}


proc getopt {argc argv} {
        global val
        lappend optlist dist

        for {set i 0} {$i < $argc} {incr i} {
                set arg [lindex $argv $i]
                if {[string range $arg 0 0] != "-"} continue

                set name [string range $arg 1 end]
                set val($name) [lindex $argv [expr $i+1]]
        }

}
#=======================================================================
# Main Program
# ======================================================================
#//////////////////////////Parametres de la simulation///////////////////

getopt $argc $argv
#
# Initialize Global Variables
#
set ns_		[new Simulator] 
set tracefd     [open fairsim.tr w] 
for {set i 1} {$i < 3 } {incr i} {
set out_($i) [open debit_$val(dist)_Flux$i.tr w]
set recievepck_($i) [open pckrecu_$val(dist)_$i.tr w]
set droppack_($i) [open drop_$val(dist)_$i.tr w]
set delay_($i) [open delay_$val(dist)_$i.tr w]
}
$ns_ trace-all $tracefd
set namtrace [open fairsim.nam w]           ;# for nam tracing
$ns_ namtrace-all-wireless $namtrace $val(x) $val(y)
$ns_ use-newtrace
# set up topography object
set topo       [new Topography]
$topo load_flatgrid $val(x) $val(y)
#
# Create God
#
set god_ [ create-god $val(nn) ]
$val(mac) set bandwidth_ 22.0e6 
#$val(prop) set pathlossExp_ 3.0 

#
#  Create the specified number of mobilenodes [$val(nn)] and "attach" them
#  to the channel. 

# configure node


        $ns_ node-config -adhocRouting $val(rp) \
			 -llType $val(ll) \
			 -macType $val(mac) \
			 -ifqType $val(ifq) \
			 -ifqLen $val(ifqlen) \
			 -antType $val(ant) \
			 -propType $val(prop) \
			 -phyType $val(netif) \
			 -channelType $val(chan) \
			 -topoInstance $topo \
			 -agentTrace ON \
			 -routerTrace ON \
			 -macTrace ON \
			 -movementTrace OFF



	for {set i 0} {$i < $val(nn) } {incr i} {
		set node_($i) [$ns_ node]	
		$node_($i) random-motion 0		;# disable random motion
	}


#========================================================================================
#////////////////////POSITION DES NOEUDS LORS DE LA SIMULATION//////////////////////////

#
# Provide initial (X,Y, Z=0) co-ordinates for mobilenodes
#

#Node 0 is the source, Node 1 is the dst

	$node_(1) set X_ 200.0
	$node_(1) set Y_ 200.0
	$node_(1) set Z_ 0.0
	

	$node_(0) set X_ 250.0
	$node_(0) set Y_ 200.0
	$node_(0) set Z_ 0.0


	set x [expr 250.0 + $val(dist)] ;#definir x=250.0 + valeur de dist
	$node_(3) set X_ $x
	$node_(3) set Y_ 200.0
	$node_(3) set Z_ 0.0

	$node_(2) set X_ [expr $x + 100.0]
	$node_(2) set Y_ 200.0
	$node_(2) set Z_ 0.0
# Define node initial position in nam

for {set i 0} {$i < $val(nn)} {incr i} {

    # 20 defines the node size in nam, must adjust it according to your scenario
    # The function must be called after mobility model is defined
    
    $ns_ initial_node_pos $node_($i) 20
}
#==============================================================================
#////////////////CONFIGURATION DES FLUX DE TRAFIC DE SIMULATION////////////////
#Attach a data-sink to destination
	set sink_(1) [new Agent/LossMonitor]
	$ns_ attach-agent $node_(1) $sink_(1)
	set sink_(2) [new Agent/LossMonitor]
	$ns_ attach-agent $node_(3) $sink_(2)
       



#traffic...make sources talk to dst
		set udp_(0) [new Agent/UDP]
		$ns_ attach-agent $node_(0) $udp_(0)
		
		set cbr_(0) [new Application/Traffic/CBR]
		$cbr_(0) set packetSize_ 512 
		$cbr_(0) set interval_ 0.0008 
		$cbr_(0) set random_ 0.96749 
		$cbr_(0) set maxpkts_ 1000000
                $cbr_(0) set fid_ 2
		$cbr_(0) attach-agent $udp_(0)

		set udp_(2) [new Agent/UDP]
		$ns_ attach-agent $node_(2) $udp_(2)
				
		set cbr_(2) [new Application/Traffic/CBR]
		$cbr_(2) set packetSize_ 512 
		$cbr_(2) set interval_ 0.0008 
		$cbr_(2) set random_ 0.96749 
		$cbr_(2) set maxpkts_ 1000000
		$cbr_(2) attach-agent $udp_(2)

		$ns_ connect $udp_(0) $sink_(1) ;#FLUX 1
		$ns_ connect $udp_(2) $sink_(2) ;#FLUX 2

		$ns_ at 0.0 "$cbr_(0) start"
		$ns_ at 0.0 "$cbr_(2) start"
               

#//////////////PROCEDURE DE CALCULS DE PARAMETRES DE PERFORMENCES (DEBITS)//////////////////
set holdrate_(1) 0
set holdrate_(2) 0
set holdseq1 0
set holdseq2 0
set holdtime1 0
set holdtime2 0
proc record {} {
global sink_
global out_
global holdrate_
global recievepck_
global droppack_
global delay_
global holdseq1
global holdseq2
global holdtime1
global holdtime2
set ns [Simulator instance]
set time 0.1 ;             #Set Sampling Time to 0.9 Sec
set bw_(1) [$sink_(1) set bytes_]
set bw_(2) [$sink_(2) set bytes_]
set bw_(3) [$sink_(1) set npkts_]
set bw_(4) [$sink_(2) set npkts_]
set bw_(5) [$sink_(1) set lastPktTime_]
set bw_(6) [$sink_(2) set lastPktTime_]
set drops_(1) [$sink_(1) set nlost_]
set drops_(2) [$sink_(2) set nlost_]
set now [$ns now]
puts $out_(1) " $now [expr (($bw_(1)+$holdrate_(1)*8))/(2*$time*1000000)]"
puts $out_(2) " $now [expr (($bw_(2)+$holdrate_(2)*8))/(2*$time*1000000)]"
puts $recievepck_(1) " $now $bw_(3)" 
puts $recievepck_(2) " $now $bw_(4)"
puts $droppack_(1)  "$now [expr $drops_(1)/$time]"
puts $droppack_(2)  "$now [expr $drops_(2)/$time]"

if { $bw_(3) > $holdseq1 } {
                puts $delay_(1) "$now [expr -($bw_(5) - $holdtime1)/($bw_(3) - $holdseq1)]"
        } else {
                puts $delay_(1) "$now [expr -($bw_(3) - $holdseq1)]"
        }
if { $bw_(4) > $holdseq2 } {
                puts $delay_(2) "$now [expr -($bw_(6) - $holdtime2)/($bw_(4) - $holdseq2)]"
        } else {
                puts $delay_(2) "$now [expr -($bw_(4) - $holdseq2)]"
        }
$sink_(1) set bytes_ 0
$sink_(2) set bytes_ 0
$sink_(1) set npkts_ 0
$sink_(2) set npkts_ 0
$sink_(1) set nlost_ 0
$sink_(2) set nlost_ 0
set  holdseq1 $bw_(3)
set  holdseq2 $bw_(4)
set  holdtime1 $bw_(5)
set  holdtime2 $bw_(6)
set  holdrate_(1) $bw_(1)
set  holdrate_(2) $bw_(2)
$ns at [expr $now+$time] "record"   ;
}
$ns_ at 0.0 "record"
#===========================================================================

#=======================================================================
	
#
# Tell nodes when the simulation ends
#
for {set i 0} {$i < $val(nn) } {incr i} {
    $ns_ at $val(simtime) "$node_($i) reset";
}
$ns_ at $val(simtime) "stop"
$ns_ at $val(simtime).01 "puts \"NS EXITING...\" ; $ns_ halt"
proc stop {} {
    global ns_ tracefd 
    $ns_ flush-trace
    close $tracefd 
}

puts "Starting Simulation..."
$ns_ run
