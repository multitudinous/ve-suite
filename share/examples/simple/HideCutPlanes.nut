// This script will hide any viz features with a nametag beginning with
// ScriptPlane. The % character acts as a wildcard.
function Execute()
{
    VizToggleSignalMaker <- StringBoolSignal;
    VizToggleSignalMaker.RegisterSignal( "HideVizFeatureByName", "HideViz" );
    HideViz.signal( "ScriptPlane%", true );
}
