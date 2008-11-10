================================================================================
    MICROSOFT FOUNDATION CLASS LIBRARY : OPPDUnit Project Overview
===============================================================================

The application wizard has created this OPPDUnit application for 
you.  This application not only demonstrates the basics of using the Microsoft 
Foundation Classes but is also a starting point for writing your application.

This file contains a summary of what you will find in each of the files that
make up your OPPDUnit application.

OPPDUnit.vcproj
    This is the main project file for VC++ projects generated using an application wizard. 
    It contains information about the version of Visual C++ that generated the file, and 
    information about the platforms, configurations, and project features selected with the
    application wizard.

OPPDUnit.h
    This is the main header file for the application.  It includes other
    project specific headers (including Resource.h) and declares the
    COPPDUnitApp application class.

OPPDUnit.cpp
    This is the main application source file that contains the application
    class COPPDUnitApp.

OPPDUnit.rc
    This is a listing of all of the Microsoft Windows resources that the
    program uses.  It includes the icons, bitmaps, and cursors that are stored
    in the RES subdirectory.  This file can be directly edited in Microsoft
    Visual C++. Your project resources are in 1033.

res\OPPDUnit.ico
    This is an icon file, which is used as the application's icon.  This
    icon is included by the main resource file OPPDUnit.rc.

res\OPPDUnit.rc2
    This file contains resources that are not edited by Microsoft 
    Visual C++. You should place all resources not editable by
    the resource editor in this file.
OPPDUnit.reg
    This is an example .reg file that shows you the kind of registration
    settings the framework will set for you.  You can use this as a .reg
    file to go along with your application.
OPPDUnit.idl
    This file contains the Interface Description Language source code for the
    type library of your application.

/////////////////////////////////////////////////////////////////////////////

The application wizard creates one dialog class and automation proxy class:
OPPDUnitDlg.h, OPPDUnitDlg.cpp - the dialog
    These files contain your COPPDUnitDlg class.  This class defines
    the behavior of your application's main dialog.  The dialog's template is
    in OPPDUnit.rc, which can be edited in Microsoft Visual C++.
DlgProxy.h, DlgProxy.cpp - the automation object
    These files contain your COPPDUnitDlgAutoProxy class.  This class
    is called the Automation proxy class for your dialog, because it
    takes care of exposing the Automation methods and properties that
    Automation controllers can use to access your dialog.  These methods
    and properties are not exposed from the dialog class directly, because
    in the case of a modal dialog-based MFC application it is cleaner and
    easier to keep the Automation object separate from the user interface.
/////////////////////////////////////////////////////////////////////////////

Other Features:

ActiveX Controls
    The application includes support to use ActiveX controls.
/////////////////////////////////////////////////////////////////////////////

Other standard files:

StdAfx.h, StdAfx.cpp
    These files are used to build a precompiled header (PCH) file
    named OPPDUnit.pch and a precompiled types file named StdAfx.obj.

Resource.h
    This is the standard header file, which defines new resource IDs.
    Microsoft Visual C++ reads and updates this file.

OPPDUnit.manifest
	Application manifest files are used by Windows XP to describe an applications 
	dependency on specific versions of Side-by-Side assemblies. The loader uses this 
	information to load the appropriate assembly from the assembly cache or private 
	from the application. The Application manifest  maybe included for redistribution 
	as an external .manifest file that is installed in the same folder as the application 
	executable or it may be included in the executable in the form of a resource. 
/////////////////////////////////////////////////////////////////////////////

Other notes:

The application wizard uses "TODO:" to indicate parts of the source code you
should add to or customize.

If your application uses MFC in a shared DLL, and your application is in a 
language other than the operating system's current language, you will need 
to copy the corresponding localized resources MFC70XXX.DLL from the Microsoft
Visual C++ CD-ROM under the Win\System directory to your computer's system or 
system32 directory, and rename it to be MFCLOC.DLL.  ("XXX" stands for the 
language abbreviation.  For example, MFC70DEU.DLL contains resources 
translated to German.)  If you don't do this, some of the UI elements of 
your application will remain in the language of the operating system.

/////////////////////////////////////////////////////////////////////////////
