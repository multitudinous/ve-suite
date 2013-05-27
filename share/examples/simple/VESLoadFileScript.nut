////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Main Entry point                                  //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////
function Execute()
{
    VoidSignalMaker <- VoidSignal;
    VoidSignalMaker.RegisterSignal( "Script.FrameAll", "FrameAll" );
    sleeper <- Sleeper();

    loadFiles();
    sleeper.Sleep(1000);

    FrameAll.signal();
    sleeper.Sleep(1000);

    createContourPlane( "TestPlane", 5.0 );
    showCube( false );
    sleeper.Sleep(1500);

    createContourPlane( "TestPlane", 36.0 );
    createContourPlane( "TestPlane", 75.0 );
    showCube( true );
    createContourPlane( "TestPlane", 95.0 );
    sleeper.Sleep( 1500 );

    showCube( false );
    createLotsOfContourPlanes( 200 );

    // This will hide every viz feature whose name starts with "ScriptPlane".
    // To un-hide the planes, change the second argument to 'false'.
    //hideContourPlanes( "ScriptPlane%", true );
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Function definitions                              //
//                                                                            // 
////////////////////////////////////////////////////////////////////////////////

function loadFiles()
{
    SignalMaker <- StringSignal;
    SignalMaker.RegisterSignal( "VESLoadFile", "FileLoader" );
    FileLoader.signal( "Surface0.75.stl" );
    FileLoader.signal( "eightCorners.stl" );
    FileLoader.signal( "3scl2vec.vtu" );
}

// Attempt to hide the cube. Unsuccessful when called in the same script run
// as the function loading the file we're trying to toggle. Sync issues
// with scenegraph are the culprit. This works if called from a different run
// of the script
function showCube( visible )
{
    ToggleSignalMaker <- StringBoolSignal;
    ToggleSignalMaker.RegisterSignal( "ToggleCADNodeByName", "CADVisible" );
    CADVisible.signal( "%Surface0.75%", visible );
}

function createContourPlane( name, location )
{
    Contour <- VizPropertySet();
    Contour.CreateNewFeature("Contours");
    Contour.SetStringPropertyValue( "NameTag", name );
    Contour.SetDoublePropertyValue( "PlaneLocation", location );
    Contour.Save();
}

function createLotsOfContourPlanes( numberToCreate )
{
    local ids=[];
    // Open a bulk transaction so we can can do lots of db saves quickly, then
    // save out a batch of ContourPlanes. We need to store the UUID of each
    // saved plane in the "ids" array so we can later notify xplorer to process
    // it.
    transaction <- TweakStore();
    transaction.OpenTransaction();
    for( index <- 0.0; index < (100.0 + 100.0/numberToCreate); index += (100.0/numberToCreate) )
    {
        name <- "ScriptPlane";
        name += index;
        Contour <- VizPropertySet();
        Contour.CreateNewFeature("Contours");
        Contour.SetStringPropertyValue( "NameTag", name );
        Contour.SetDoublePropertyValue( "PlaneLocation", index );
        Contour.BulkSave( transaction );
        ids.append( Contour.GetUUIDAsString() );
    }
    transaction.CloseTransaction();

    // With the transaction closed, we can tell xplorer to look at the new
    // planes. Without this step, the planes added during the transaction won't
    // show up. We only have to do this step when using bulk transactions.
    TwoStringSignalMaker <- TwoStringSignal;
    TwoStringSignalMaker.RegisterSignal( "Script.AddVizFeature", "ProcessViz" );
    foreach(id in ids)
    {
        ProcessViz.signal( id, "ContourPlane" );
    }
}

function hideContourPlanes( pattern, hide )
{
    VizToggleSignalMaker <- StringBoolSignal;
    VizToggleSignalMaker.RegisterSignal( "HideVizFeatureByName", "HideViz" );
    HideViz.signal( pattern, hide );
}
