-------------------------------------------------------------------------

                              _Overview_ 

			    Release 1.0

     X11R4 based graphics tool for visualization of Tierra (1) data.

	            by Marc Cygnus & Daniel Pirone
		     cocteau@tierra.slhs.udel.edu

-------------------------------------------------------------------------

Overview (ov) is a basic tool for the visualization of the spatial 
distribution of Tierra "critters" births and deaths. New features 
will be added as time allows.

ov communicates with a running tierra simulation via tcp/ip sockets.
This allows ov to talk to a simulation on the same machine, or on
a simulation running across the world ( isn't the net wonderful ? ).

The command syntax is :

ov machine.name port_num [high_water_mark [low_water_mark]]

where:
  machine.name   is the host machine's reachable address:
               eg : tierra.slhs.ude.edu , or 
                    tierra, or 
		    128.175.41.33 ...

  port_num       is a positive integer ( usually 7001 or greater )
     that is specified by the running tierra as its virtual port number 
     (VPORT).

  high_water_mark is an optional parameter that sets an upper limit
     on incoming packet buffering usually 50 is a good value for this 
     flow control setting. These are recommended for monitoring the
     ip data flows.

  low_water_mark  is an optional parameter that sets an lower limit
     on incomming packet buffering usually 40 is a good value for this 
     flow control setting. If a high_water_mark is set but not a low one,
     the value of 2 ( very low! ) will be used.

Upon a successful connection, the size of the tierrain soup ( core )
will be printed, and soon an X window will need to be placed.

Along the left hand side a numeric scale of the the soup address will be
given. On the right, a slider for moving around in really large soups.
at the bottom, a ranked list of color - to - size mappings is presented.

The grey areas represent section of the soup that are not owned at that
moment.

In most X systems, the function keys F1 - F3 will select a magnification 
range: F1 is the most "zoomed" in, F3 is farther out. Window resizing
is not fully supported, yet.

Adding the following lines to your ~/.Xdefaults will help the above.

Overview*memframe*translations: \
#override               <Key>F1: SetRez(hi) \n\
                                <Key>F2: SetRez(med) \n\
                                <Key>F3: SetRez(lo) \n\
                                <Key>F4: SetRez(zoom) \n\
                                <Key>F5: SetRez(micro) \n\
                                <Btn1Down>: OrgQuery()\n
Overview*memscroll*translations:        #override         <Btn2Up>: RedrawMem()

Note: the micro and zoom modes are experimental, and metamorph at any point.

-------------------------------------------------------------------------

(1) Tierra - Artificial Life system by Dr. Thomas S. Ray
available from anonymous FTP from tierra.slhs.udel.edu (128.175.41.33)
runs on most unix and msdos platforms.

                             Tom Ray
                       University of Delaware       
                  School of Life & Health Sciences
                      Newark, Delaware  19716
                        ray@brahms.udel.edu
                         302-451-2281 (FAX)
                            302-451-2753

