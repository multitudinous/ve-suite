#!MC 1200
# Created by Tecplot 360 build 12.0.0.3454
#
$!READDATASET  '"STANDARDSYNTAX" "1.0" "LoadOption" "MultipleCaseAndData" "FILELIST_Files" "2" "|$SJKFILENAME|.cas" "|$SJKFILENAME|.dat" "UnsteadyOption" "ReadTimeFromDataFiles" "AssignStrandIDs" "Yes" "GridZones" "CellsAndBoundaries" "IncludeParticleData" "No" "AllPolyZones" "No" "AverageToNodes" "No" "SaveUncompressedFiles" "No"'
  DATASETREADER = 'Fluent Data Loader'
$!WRITEDATASET  "|$SJKFILENAME|.plt"
  INCLUDETEXT = NO
  INCLUDEGEOM = NO
  INCLUDECUSTOMLABELS = NO
  BINARY = YES
  USEPOINTFORMAT = NO
  PRECISION = 9
  TECPLOTVERSIONTOWRITE = TECPLOTCURRENT
$!QUIT
