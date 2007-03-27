For UNIX systems: How to Configure Icons & Menus for VE-Suite

Once VE-Suite is installed, you can configure your system to:
-Open a .ves file in VE-Suite when it is double-clicked.
-Give .ves files the VE-Suite icon.
-Put VE-Suite shortcuts in your menus.

Here's how:

1. Make sure the path to VE-Suite's executables is in your PATH variable. To test if it is, try running:
   velauncher.py
from the command line. If VE-Launcher opens, your PATH is fine. If not, you'll have to add it to PATH, with a command like:
   setenv PATH your_path_to_vesuite_bin:${PATH} (in tcsh)
or
   export PATH=your_path_to_vesuite_bin:${PATH} (in sh)


2. Run the vesuite config script for your desktop.
If you have GNOME 2.8+, run:
   ./vesuiteGnomeConfig.sh
If you have KDE, run:
   ./vesuiteKdeConfig.sh
These scripts can be found in the VE_Installer/installer directory.

3. Log off and log back in to load the changes.

WARNING: If double-clicking .ves files doesn't work after the configuration, check a .ves file's Properties->Open With. Since .ves files are also identified as XML files, your .xml file opener might override your .ves file opener. You can fix this by manually telling the computer to open .ves files with VE-Suite from now on.

On GNOME:
Right-click a .ves file. Choose Properties. Select the Open With... tag. Set it to open with VE-Suite Launcher and close the window.

On KDE:
Right-click a .ves file. Choose Properties. Click the Edit File Type icon next to Type: VES File. Under the Application Preference Order list, select VE-Suite Launcher and move it up to the top of the list. Click OK.

===UNIX Configuration Details===

--Mimetype Files--

Two files are used to define .ves's mimetype, depending on which desktop you're using:

-GNOME uses the VE-Suite.xml file.
-KDE uses the ves.desktop file.

No changes should need to be made to these files, although you might want to note that the mimetype of VE-Suite files is:
 application/x-vesuite

--Application .desktop Files--

The VE-Suite configuration installs four application .desktop files on your computer:
vesuite.desktop, vesuiteReboot.desktop, vesuiteShutdown.desktop, vesuiteWake.desktop

These define menu items for the VE-Suite executables. If you open them in a text editor, you can change their attributes. Two attributes you might want to change are:

   Categories=(categories for this program)
Assigns the executable to categories for menus, such as Graphics for inclusion
in the Graphics menu. Change this if you want to customize which categories VE-Suite appears in. (For example, remove the Graphics category and VE-Suite won't appear in the Graphics menu.) The default categories of the VE-Suite executables are:
 Graphics, Science, DataVisualization, Engineering, VESuite

   Exec=(path to executable)
The path to the program's executable. If you don't want to include the path to VE-Suite's executables in your path, you can change Exec to point at them instead. For example:
 Exec=/home/jdoe/VE_Suite1.0.5/bin/velauncher.py

===Extra===

--Adding a VE-Suite Menu--

Since the VE-Suite executables are grouped into the VESuite category by their .desktop files, you can make a custom menu for them by following these instructions:
1. Open .config/menus/applications.menu in a text editor.
2. Add these lines into the existing menu.
  <Menu>
    <Name>VE-Suite</Name>
    <Include>
      <Category>VESuite</Category>
    </Include>
  </Menu>
3. Log out and log back in to load the changes.
