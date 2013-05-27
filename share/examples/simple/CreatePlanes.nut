
function Execute()
{
    local numberToCreate = 50;
    local ids=[];
    // Open a bulk transaction so we can can do lots of db saves quickly, then
    // save out a batch of ContourPlanes. We need to store the UUID of each
    // saved plane in the "ids" array so we can later notify xplorer to process
    // it.
    transaction <- TweakStore();
    transaction.OpenTransaction();
    for( index <- 0.0;
         index < (100.0 + 100.0/numberToCreate);
         index += (100.0/numberToCreate) )
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
