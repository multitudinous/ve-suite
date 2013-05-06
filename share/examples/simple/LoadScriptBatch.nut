////////////////////////////////////////////////////////////////////////////////
// This script demonstrates the use of the AssociateScript signal to
// procedurally associate scripts with buttons on the "multi" pane of the
// Scripting tab in VE-Suite. The first argument to the signal is the path of
// the script. The second argument is the zero-based index of the button with
// which to associate the script. Valid indices are 0 through 11.
////////////////////////////////////////////////////////////////////////////////
StringIntSignalMaker <- StringIntSignal;
StringIntSignalMaker.RegisterSignal( "Script.AssociateScript", "AssociateScript" );
AssociateScript.signal( "LoadFiles.nut", 0 );
AssociateScript.signal( "CreatePlanes.nut", 1 );
AssociateScript.signal( "HideCube.nut", 2 );
AssociateScript.signal( "ShowCube.nut", 3 );
AssociateScript.signal( "HideCutPlanes.nut", 4 );
AssociateScript.signal( "ShowCutPlanes.nut", 5 );
