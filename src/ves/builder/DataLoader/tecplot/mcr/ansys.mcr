#!MC 1200
# Created by Tecplot 360 build 12.0.0.3454
#
$!READDATASET  '"STANDARDSYNTAX" "1.0" "FILENAME_File" "|$SJKFILENAME|.rst" "AutoAssignStrandIDs" "Yes" "InitialPlotType" "Cartesian3D" "ShowFirstZoneOnly" "No"'
  DATASETREADER = 'ANSYS Results (FEA)'
$!DRAWGRAPHICS NO
$!EXTENDEDCOMMAND 
  COMMANDPROCESSORID = 'FEALoader'
  COMMAND = 'DeriveVariable Variable=\'VectorMagnitude\' SourceVarName=\'Displacement\''
$!EXTENDEDCOMMAND 
  COMMANDPROCESSORID = 'FEALoader'
  COMMAND = 'DeriveVariable Variable=\'VonMisesStressOrStrain\' SourceVarName=\'Stress\''
$!EXTENDEDCOMMAND 
  COMMANDPROCESSORID = 'FEALoader'
  COMMAND = 'DeriveVariable Variable=\'PrincipalStressesOrStrains\' SourceVarName=\'Stress\''
$!WRITEDATASET  "|$SJKFILENAME|.plt" 
  INCLUDETEXT = NO
  INCLUDEGEOM = NO
  INCLUDECUSTOMLABELS = NO
  INCLUDEDATASHARELINKAGE = YES
  BINARY = YES
  USEPOINTFORMAT = NO
  PRECISION = 9
  TECPLOTVERSIONTOWRITE = TECPLOTCURRENT
$!QUIT
