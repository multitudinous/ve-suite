function Execute()
{
    ToggleSignalMaker <- StringBoolSignal;
    ToggleSignalMaker.RegisterSignal( "ToggleCADNodeByName", "CADVisible" );
    CADVisible.signal( "%Surface0.75%", true );
}
