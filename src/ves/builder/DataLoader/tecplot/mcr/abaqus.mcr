#!MC 1200
# Created by Tecplot 360 build 12.0.0.3454

$!READDATASET  '"STANDARDSYNTAX" "1.0" "FILENAME_File" "|$SJKFILENAME|.inp" "AutoAssignStrandIDs" "Yes" "InitialPlotType" "Cartesian3D" "ShowFirstZoneOnly" "No"'
  DATASETREADER = 'ABAQUS Input (FEA)'
$!WRITEDATASET  "|$SJKFILENAME|.plt"
  INCLUDETEXT = NO
  INCLUDEGEOM = NO
  INCLUDECUSTOMLABELS = NO
  BINARY = YES
  USEPOINTFORMAT = NO
  PRECISION = 9
  TECPLOTVERSIONTOWRITE = TECPLOTCURRENT
$!QUIT
