#!WISH -f

#	TkJoe - Provide a friendly user interface for each program
#	        using getparam()
#
# 	Copyright (C) 1994 Joachim Sprave
# 
# 	This program is free software; you can redistribute it and/or modify
# 	it under the terms of the GNU General Public License as published by
# 	the Free Software Foundation; either version 2 of the License, or
# 	(at your option) any later version.
# 
# 	This program is distributed in the hope that it will be useful,
# 	but WITHOUT ANY WARRANTY; without even the implied warranty of
# 	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# 	GNU General Public License for more details.
# 
# 	You should have received a copy of the GNU General Public License
# 	along with this program; if not, write to the Free Software
# 	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.



#----------------------------------------------------------------------
# Frames and menus
#

frame .menu -borderwidth 1 -background grey


menubutton .menu.file -text File -menu .menu.file.m -relief raised -width 10
menu .menu.file.m

.menu.file.m add command -label New   -command newParms  
.menu.file.m add command -label Open  -command openParms 
.menu.file.m add command -label Save  -command saveParms 
.menu.file.m add command -label View -command viewParms 

menubutton .menu.run -text Run -menu .menu.run.m -relief raised -width 10
menu .menu.run.m

.menu.run.m add command -label XTerm      	-command {runProg xterm -e} 
.menu.run.m add command -label Background	-command {runProg}
.menu.run.m add command -label Nice		-command {runProg nice -19}

menubutton .menu.kill -text Kill -menu .menu.kill.m -relief raised -width 10
menu .menu.kill.m

.menu.kill.m add command -label "Terminate"	-command "killProg 15" 
.menu.kill.m add command -label "Kill"		-command "killProg 9"

button .menu.quit -text Quit -width 10 -command {cleanUp; exit}

pack .menu.file .menu.run .menu.kill .menu.quit -side left
pack .menu -side top -fill x



frame .entries 
frame .buttons

#----------------------------------------------------------------------
# Create a state window
#


toplevel .status
wm minsize . 100 50

text .status.msg -height 10 -width 80 \
		-state disabled -borderwidth 1 -relief sunken \
		-yscrollcommand ".status.scr set" -background white \
		-font -*-lucidatypewriter-medium-*-*-*-*-*-*-*-*-*-*-*

scrollbar .status.scr -command ".status.msg yview"

button .status.clear -text "Clear Message Window"\
		 -command clearMessage

pack .status.scr -side left -fill y -expand y
pack .status.msg -fill both -expand y
pack .status.clear -side bottom

#----------------------------------------------------------------------
# Read current settings
#
proc readParms {fname} {

	global keys
	global desc
	global type
	global vlue
	global exst

	set fd [open $fname r]

	while {[gets $fd line] > -1} {

		set tmp [lindex $line 0]
		if { [regexp {^[ \t]*$} $tmp] || [regexp {^#.*$} $tmp] } continue
                scan $line {%[^=	 ] = %[^=	# ]} key value
		if {[lsearch $keys $key] == -1} {
			putMessage "warning: unknown parameter $key ingnored"
		} elseif {[check$type($key) $key $value]} { 
                	setParm $key $value
			set exst($key) 1
		}
	}
	close $fd
}

#----------------------------------------------------------------------
# Set a parameter's value in the display
#
proc setParm {kw val} {
	global keys
        global vlue
        global type
	global cont

	switch $type($kw) {
		real { 
			set cont($kw) $val
			.entries.$kw.entry delete 0 end
			.entries.$kw.entry insert 0 $val
		}
		integer { 
			set cont($kw) $val
			.entries.$kw.entry delete 0 end
			.entries.$kw.entry insert 0 $val
		}
		string { 
			set cont($kw) $val
			.entries.$kw.entry delete 0 end
			.entries.$kw.entry insert 0 $val
		}
		boolean {
			if { $val != $cont($kw) } {
				.buttons.$kw invoke
			}
		}
		enum {
			.buttons.$kw.$val invoke
		}
	}
}

#----------------------------------------------------------------------
# Read the configuration
#
proc readConfig {fname} {

	global keys
	global desc
	global type
	global vlue
	global cont
	global optn
	global exst

	set fd [open $fname r]
	while {[gets $fd line] > -1} {

		set tmp [lindex $line 0]
		if { $tmp == "" || [regexp {#.*} $tmp] } continue

		scan $line {%s %s "%[^"]" %s %s} kwd opt dsc typ val

		set dsc [string trim $dsc]

		if [info exists keys] {
			lappend keys $kwd
		} else {
			set keys $kwd
		}
		set optn($kwd) $opt 
		set desc($kwd) $dsc
		set type($kwd) $typ
		set vlue($kwd) $val
		set exst($kwd) 0

		createNew$typ $kwd
	}
	close $fd
}

#----------------------------------------------------------------------
# Check functions: check a given value against its configured type
#
proc checkreal {kw val} {

        if { [regexp {^[+-]?[0-9]+\.?([eE][+-]?[0-9]+)?$} $val] \
        || [regexp {^[+-]?[0-9]*\.[0-9]+([eE][+-]?[0-9]+)?$} $val]} {
		set ret 1
	} else {
		putMessage "format error in $kw: $val is not a real number"
		set ret 0
	} 
	return $ret
}

proc checkinteger {kw val} {

	if {[regexp {^[+-]?[0-9]+$} $val]} {
		set ret 1
	} else {
		putMessage "format error in $kw: $val is not an integer"
		set ret 0
	} 
	return $ret
}

proc checkstring {kw val} {
	global vlue

	if {[regexp $vlue($kw) $val]} {
		set ret 1
	} else {
		putMessage "format error in $kw: $val does not match $vlue($kw)"
		set ret 0
	} 
	return $ret
}

proc checkboolean {kw val} {
	checkenum $kw $val
}

proc checkenum {kw val} {
	global vlue

	set vlist [split $vlue($kw) {|}]	
	
	if {[lsearch $vlist $val] != -1} {
		set ret 1
	} else {
		putMessage [format "format error in %s: $val is not in %s" \
			$kw $vlue($kw)]
		set ret 0
	} 
	return $ret
}

proc getForeground {opt} {
	if {$opt == {*}} {
		set ret blue
	} else {
		set ret black
	}
	return $ret
}

#----------------------------------------------------------------------
# Create functions: create an item for a given key word according to
#                   its type
#
proc createNewboolean {kw} {

	global keys
	global desc
	global type
	global vlue
	global optn
	global cont


	set yesno [split $vlue($kw) {|}]

	checkbutton .buttons.$kw \
		-text $desc($kw) \
		-onvalue  [lindex $yesno 1] \
		-offvalue [lindex $yesno 0] \
		-command "set exst($kw) 1" \
		-variable cont($kw) -foreground [getForeground $optn($kw)]
	pack .buttons.$kw -side top -expand yes -anchor w

	.buttons.$kw invoke
	.buttons.$kw invoke
}

proc createNewenum {kw} {

	global keys
	global desc
	global type
	global vlue
	global optn
	global cont

	frame .buttons.$kw

	label .buttons.$kw.l -background lightblue -text $desc($kw)
	pack .buttons.$kw.l -side top -expand yes -fill x

	foreach v [split $vlue($kw) {|}] {
		radiobutton .buttons.$kw.$v \
			-text $v \
			-variable cont($kw) \
			-value $v \
			-command "set exst($kw) 1" \
			-relief flat -foreground [getForeground $optn($kw)]

		pack .buttons.$kw.$v -side top -expand yes -anchor w
	}
	set first [lindex [split $vlue($kw) {|}] 0]
	pack .buttons.$kw -side top -expand yes -fill x

	.buttons.$kw.$first invoke
}

proc updateEntry {kw} {
	global cont
	global exst

	set cont($kw) [string trim [.entries.$kw.entry get]]
	.entries.$kw.entry delete 0 end
	.entries.$kw.entry insert 0 $cont($kw)
	if {$cont($kw) == ""} {
		set exst($kw) 0
	} else {
		set exst($kw) 1
	}
}

proc createNewstring {kw} {

	global keys
	global desc
	global type
	global vlue
	global optn
	global cont
	global exst

	frame .entries.$kw

	label .entries.$kw.label  -text $desc($kw) -foreground [getForeground $optn($kw)]
	entry .entries.$kw.entry -width 10 -relief raised -borderwidth 2
	pack  .entries.$kw.entry -side left
	pack  .entries.$kw.label -side left -expand y -anchor w
	pack  .entries.$kw -side top -expand yes -fill x
	bind  .entries.$kw.entry <Leave> "updateEntry $kw"
	set cont($kw) {}
}

proc createNewinteger {kw} {

	global keys
	global desc
	global type
	global vlue
	global optn
	global cont
	global exst

	frame .entries.$kw

	label .entries.$kw.label  -text $desc($kw) -foreground [getForeground $optn($kw)]
	entry .entries.$kw.entry -width 10 -relief raised -borderwidth 2
	pack  .entries.$kw.entry -side left
	pack  .entries.$kw.label -side left -expand y -anchor w
	pack  .entries.$kw -side top -expand yes -fill x
	bind  .entries.$kw.entry <Leave> "updateEntry $kw"
	set cont($kw) 0
}

proc createNewreal {kw} {

	global keys
	global desc
	global type
	global vlue
	global optn
	global cont
	global exst

	frame .entries.$kw

	label .entries.$kw.label  -text $desc($kw) -foreground [getForeground $optn($kw)]
	entry .entries.$kw.entry -width 10 -relief raised  -borderwidth 2
	pack  .entries.$kw.entry -side left
	pack  .entries.$kw.label -side left -expand y -anchor w
	pack  .entries.$kw -side top -expand yes -fill x
	bind  .entries.$kw.entry <Leave> "updateEntry $kw"
	set cont($kw) 0.0
}

#----------------------------------------------------------------------
# Put a string in the message line
#
proc putMessage {msg} {
	.status.msg  yview {end - 8 lines}
	.status.msg configure -state normal
	.status.msg insert end "$msg\n"
	.status.msg configure -state disabled
}

proc clearMessage {} {	
	.status.msg configure -state normal
	.status.msg delete 1.0 end
	.status.msg configure -state disabled
}

#----------------------------------------------------------------------
# File operations
#
proc newParms {} {

	global keys
	global exst
        global vlue
        global type
	global cont

	foreach kw $keys {
		set exst($kw) 0
		switch $type($kw) {
			real - integer { 
				set cont($kw) 0
				.entries.$kw.entry delete 0 end
				.entries.$kw.entry insert 0 {}
			}
			string { 
				set cont($kw) {}
				.entries.$kw.entry delete 0 end
				.entries.$kw.entry insert 0 {}
			}
			boolean {
				.buttons.$kw deselect
			}
			enum {
				.buttons.$kw.[lindex [split $vlue($kw) {|}] 0] invoke
			}
		}
	}
}

proc openParms {} {

	global parfile

	set parfile [fileSelect "Parameter File Selection" r]
	if {$parfile != -1} {
		newParms
		readParms $parfile
	} else {
		putMessage "Parameter file selection aborted."
	}
}

proc saveParms {} {

	global parfile

	if {![info exists parfile]} {
		set parfile [fileSelect "Parameter File Selection"]
	}
	writeParms $parfile
}

proc writeParms {fname} {

	global keys
	global desc
	global vlue
	global cont
	global exst

	set fd [open $fname "w"]

	foreach kw $keys {
		if {$exst($kw)} {
			puts $fd [format {%-20s = %10s # %s} \
					$kw $cont($kw) $desc($kw)]
		} else {
			puts $fd [format {# %-19s = <not set> # %s} \
					$kw $desc($kw)]
		}
	}
	close $fd
}

proc checkParms {} {

	global keys
	global type
	global optn
	global cont
	global exst

	set ok 1
	foreach kw $keys {
		if {$exst($kw)} {
			if {![check$type($kw) $kw $cont($kw)]} {
				set ok 0
			}
		} elseif {$optn($kw) == "!"} {
			set ok 0
			putMessage "mandatory parameter $kw not set"
		}
	}
	return $ok
}

proc viewParms {} {

	global keys
	global desc
	global type
	global vlue
	global cont
	global exst

	foreach kw $keys {
		if {$exst($kw)} {
			putMessage [format {%-20s = %10s # %s} \
					$kw $cont($kw) $desc($kw)]
		} else {
			putMessage [format {# %-19s = <not set> # %s} \
					$kw $desc($kw)]
		}
	}
}

#----------------------------------------------------------------------
# Run callbacks
#
proc runProg args {

	global cmd
	global pid
	global tmppar

	set running no
	if [info exists pid] {

		set fd [open "|ps" "r"]
		while {[gets $fd line] != -1} {
			if {[regexp "^\[\ \]*$pid" $line]} {
				set running yes
				unset pid
				break
			}
		}
		close $fd
	}
	if {$running} {
		putMessage "$cmd is still running."
	} else {
		if {[checkParms]} {
			writeParms $tmppar
			set pid [eval exec $args $cmd $tmppar \&]
			putMessage "$cmd started."
		} else {
			putMessage "$cmd not started."
		}
	}
}

proc killProg {sig} {
	global pid

	set msg "Nothing to kill."

	if [info exists pid] {
		if [catch {exec kill -$sig $pid}] {
			set msg "cannot kill $pid."
		} else {
			set msg "$pid killed."
		}
		unset pid
	}
	putMessage $msg
}


#----------------------------------------------------------------------


proc fsbOk {} {
	global ret
	global path

	set sel [.fsb.entry get]

	if {$sel == {}} {
		set ret -1
	} else {
		set ret $path/$sel
	}
	destroy .fsb
}

proc fsbCd {mode} {
	global path

	set sel [lindex [.fsb.d.list curselection] 0]

	if {$sel != {}} {
		set shargs [format "cd %s; pwd" $path/[.fsb.d.list get $sel]]
		set path [exec sh -c [format "cd %s; pwd" $path/[.fsb.d.list get $sel]]]
		fillFsb $path $mode
	}
}


proc fillFsb {path mode} {

	set files [split [exec ls -a1 $path] "\n"]

	.fsb.f.list delete 0 end
	.fsb.d.list delete 0 end

	.fsb.d.list insert end {..}
	foreach f [split [exec ls -1 $path] "\n"] {
		if { [file isdirectory $path/$f] } {
			.fsb.d.list insert end $f
		} else {
			set ins 1
			if {[string match {*x*} $mode] 
				&& ![file executable $path/$f]} {
				set ins 0
			}
			if {[string match {*r*} $mode] 
				&& ![file readable $path/$f]} {
				set ins 0
			}
			if {[string match {*w*} $mode] 
				&& ![file writable $path/$f]} {
				set ins 0
			}
			if {$ins} {
				.fsb.f.list insert end $f
			}
		}
	}
}

proc fsbGet {pos} {

	set sel [.fsb.f.list nearest $pos]
	if {$sel != {}} {
		.fsb.entry delete 0 end
		.fsb.entry insert end [.fsb.f.list get $sel]
	}
}

proc fileSelect {title {mode r}} {

	toplevel .fsb
	grab .fsb
	wm title .fsb $title

	global ret
	global path

	set path {.}

	frame .fsb.d -borderwidth 1 -relief ridge -background lightgrey
	frame .fsb.f -borderwidth 1 -relief ridge -background lightgrey

	entry .fsb.entry -background white -relief sunken

	button .fsb.d.button -text cd -command "fsbCd $mode"
	button .fsb.f.button -text OK -command fsbOk

	label .fsb.d.label -text Directories
	label .fsb.f.label -text Files

	listbox .fsb.d.list -yscrollcommand {.fsb.d.slide set} \
			-geometry 20x10 \
			-relief sunken \
			-borderwidth 1

	
	listbox .fsb.f.list -yscrollcommand {.fsb.f.slide set} \
			-geometry 20x10 \
			-relief sunken \
			-borderwidth 1

	scrollbar .fsb.f.slide  -command ".fsb.f.list yview"
	scrollbar .fsb.d.slide  -command ".fsb.d.list yview"

	bind .fsb.f.list <Double-Button-1> fsbOk
	bind .fsb.f.list <Button-1> "+fsbGet %y"
	bind .fsb.d.list <Double-Button-1> "fsbCd $mode"

	fillFsb . $mode

	pack .fsb.entry -side top -fill x -expand yes
	pack .fsb.d.label -side top -fill x -expand y
	pack .fsb.d.button -side bottom -fill x -expand yes
	pack .fsb.d.slide .fsb.d.list \
		-side left -expand y -fill both -padx 3 -pady 3

	pack .fsb.f.label -side top -fill x -expand y
	pack .fsb.f.button -side bottom -fill x -expand yes
	pack .fsb.f.list .fsb.f.slide \
		-side left -expand y -fill both -padx 3 -pady 3

	pack .fsb.d .fsb.f -side left -fill both -expand yes

	tkwait window .fsb
	return $ret
}	

#----------------------------------------------------------------------
# Clean up procedure
#

proc cleanUp {} {

	global tmpcnf
	global tmppar

	killProg 9
	exec rm -f $tmpcnf $tmppar
}	

#----------------------------------------------------------------------
# Main procedure: determine the configuration source, invoke configuration
#

proc main {argc argv} {

	global keys
	global tmpcnf
	global tmppar
	global cmd

	wm withdraw .
	wm maxsize . 640 480

	set tmpcnf "/tmp/tkjoe.cnf"
	set tmppar "/tmp/tkjoe.par"

	if {$argc > 1} {
		puts stderr {usage: tkjoe [command|filename]}
		exit 1
	} 
	if {$argc == 1} {
		set cmd [lindex $argv 0]
	} else {	
		set cmd	[fileSelect "Program Selection" rx]
	}

	if {$cmd == "-1" || ![info exists cmd]} {
		puts stderr "tkjoe: No file or command selected"
		exit 1
	}

	if {[file readable $cmd]} {
		if  {[file executable $cmd]} {
			set fd [open {|sh} {w}]
			puts $fd [format {%s 2>%s} $cmd $tmpcnf]
			puts $fd "cat $tmpcnf >/dev/null"
			close $fd
		} else {
			exec cp $cmd $tmpcnf
		}
	} else {
		puts stderr "tkjoe: cannot open $cmd"
		exit 1
	} 
	readConfig $tmpcnf
	wm deiconify .
	if {[file readable $cmd.in]} {
		readParms $cmd.in
	}

#	pack .status -side bottom -fill x -padx 3 -pady 3
	pack .entries .buttons -side left -expand yes -fill y
}

#----------------------------------------------------------------------
# Here we go
#

option add *Font -*-lucidatypewriter-medium-*-*-*-*-*-*-*-*-*-*-*
main $argc $argv


#================================EOF===================================
