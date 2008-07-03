First, open two terminal windows.  In one you need to have at the location of
the RunUnit file.  Mine was located at

~/TSVEG/VE_Suite/install/rhel5.x64/share/vesuite/examples/InteractiveStove

The other terminal is for running vesuites in.

Do a source of the env settings, mine is 

~/TSVEG/vesenv_rhel5.sh. 

You have to source this in both terminals.  Meaning that you need to source the
env settings in the InteractiveStove demo location as well as the regular
location.

Once you have set your env in both location, load up vesuite in the terminal
that is not at the InteractiveStove location.  I did it using the

velauncher.py

Once vesuite is loaded up, go to the InteractiveStove demo, that you have open
in the other terminal.  Make sure that the X-ray shader is load, if not you need
to do this first.  Right click on the unit box and go to gem configure.  Once
open, add the x-ray shader to the stoves top. 
 
Before you open the baffle unit, you must run ./RunUnit file in the
InteractiveStove terminal.  Make sure this file points to the correct location
first for the IntStoveUnit binary file.  Mine was:

 Plugins/Unit/linux-x86_64/IntStoveUnit
 
Once you do the ./RunUnit it should say IntStoves :GetName called in that
terminal window. 

Now, double left click on the baffle unit, this will load up the grid for
drawing the baffle on.  Now you should be able to draw a baffle anywhere on the
grid.  This can be tough, heading the nodes is some times tough to do.  Next go
to the baffle definition and you can change the depth of the baffle to any
number between 1-12.  Click on the box next to the show contour(temperature)
this will allow the user to see the contour plot of temperature after the starcd
file has updated.  Note: that when you click on the contour plot box it will load
the last contour plot it has ran not the current one. Next click the Update
button and then the close button.  Then you must Submit the job, there is two
ways this can be done either click on the Submit Job button or go to the
connection drop down menu and select Submit Job. The last step is to go to the
Execution drop down menu and select Start Simulation.  It takes about 3-5
minutes for it to up date the contour on the screen.

If you would like you can go back to the baffle unit by left clicking twice on
it, and updating the baffles by either adding more or deleting previous baffles
or just changing the depth of the baffle. Repeat the above, clicking update,
close, submit job, start simulation.  It should keep changing the contour to
match that of the new design.





