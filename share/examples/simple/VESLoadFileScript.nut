// See bottom of file for execution sequence

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Function definitions                              //
//                                                                            // 
////////////////////////////////////////////////////////////////////////////////

function loadFiles()
{
    SignalMaker <- VoidStdStringConstRefSignal;
    SignalMaker.RegisterSignal( "VESLoadFile", "FileLoader" );
//    FileLoader.signal( "Surface0.75.stl" );
    FileLoader.signal( "eightCorners.stl" );
    FileLoader.signal( "3scl2vec.vtu" );
}

// Attempt to hide the cube. Unsuccessful when called in the same script run
// as the function loading the file we're trying to toggle. Sync issues
// with scenegraph are the culprit. This works if called from a different run
// of the script
function hideCube()
{
    ToggleSignalMaker <- v_sscr_bcr_signal;
    ToggleSignalMaker.RegisterSignal( "ToggleCADNodeByName", "CADVisible" );
    CADVisible.signal( "%Surface0.75%", false );
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
    for( index <- 0.0; index < (100.0 + 100.0/numberToCreate); index += (100.0/numberToCreate) )
    {
        name <- "ScriptPlane";
        name += index;
        createContourPlane( name, index );
    }
}

function hideContourPlanes()
{
    VizToggleSignalMaker <- v_sscr_bcr_signal;
    VizToggleSignalMaker.RegisterSignal( "HideVizFeatureByName", "HideViz" );
    // This will hide every viz feature whose name starts with "ScriptPlane".
    // It will *not* hide the single plane created in the execution sequence 
    // below which has "TestPlane".
    // To un-hide the planes, change the second argument to HideViz to 'false'.
    HideViz.signal( "ScriptPlane%", true );
}

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//                          Execution sequence                                //
//                                                                            // 
////////////////////////////////////////////////////////////////////////////////

loadFiles();
//hideCube();
//createContourPlane( "TestPlane", 50.0 );
//createLotsOfContourPlanes( 10 );
//hideContourPlanes();

