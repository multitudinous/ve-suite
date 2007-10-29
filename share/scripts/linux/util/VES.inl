#!/bin/csh -f
 
# Set environment variables and paths for running the program
#setenv VE_SUITE_HOME /home/vr/Applications/TSVEG/VE_Suite
#setenv VE_SUITE_HOME /home/users/sgent/VE_Suite

# Set the TAO nameServer properties used by the run scripts
#setenv TAO_MACHINE lego.vrac.iastate.edu
setenv TAO_MACHINE virtual1.inel.gov
setenv TAO_PORT 1236

#source ${VE_SUITE_HOME}/VE_Installer/setup.tsh

set MY_VJCONFIGS=/home/vr/vjconfig/2.0beta2

switch ($1)

  case -runharvunit:
    /home/virtual1/VES/VES_Install/TSVEG/inl/Int_Harvester/Int_HarvesterUnit/Int_HarvesterUnitApp \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService 
  breaksw

  case -simosg:
    project_tao_osg \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    ${VJ_BASE_DIR}/share/vrjuggler/data/configFiles/sim.base.jconf \
    ${VJ_BASE_DIR}/share/vrjuggler/data/configFiles/sim.wand.mixin.jconf
  breaksw

  case -dualhead:
    project_tao_osg \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    /home/virtual1/VES/VES_Install/TSVEG/bay_5_vr/dualhead_configs/dualhead.jconf
  breaksw

  case -simosgvep:
    project_tao_osg_vep \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    ${VJ_BASE_DIR}/share/vrjuggler/data/configFiles/sim.base.jconf \
    ${VJ_BASE_DIR}/share/vrjuggler/data/configFiles/sim.wand.mixin.jconf
  breaksw

  case -wall:  
    project_tao_osg_cluster \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    /home/users/mccdo/babycave/vecr.jconf
    /usr/share/Performer/bin/rmsem      
  breaksw

  case -nserv:
    killall -q Naming_Service 
    killall -q Exe_server 
    echo Starting TAO Naming Service on $TAO_MACHINE, port $TAO_PORT
    Naming_Service \
    -ORBEndPoint iiop://${TAO_MACHINE}:${TAO_PORT} &
    sleep 5
    echo TAO Naming Service Started
    echo Starting VE-CE...
    Exe_server \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService &
  breaksw
  
  case -shutdown:
    killall Naming_Service 
    killall Exe_server 
  breaksw

  case -menu:
    WinClient \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService
  breaksw

  case -surface:
    makeVtkSurface
  breaksw

  case -preproc:
    preprocessor
  breaksw

  case -surf2stl:
    convertSurfaceFileToStl
  breaksw

  case -transVTK:
    transformVTK
  breaksw

  case -cfd2VTK:
    translateToVtk
  breaksw

  case -make:
    cd ${VE_SUITE_HOME}/VE_Installer
    gmake
  breaksw
  
  case -makeclean:
    cd ${VE_SUITE_HOME}/VE_Installer
    gmake clean
    gmake cleandepend
  breaksw
  
  case -merge:
    mergeVertices
  breaksw

  case -meshView:
    meshViewer
  breaksw

  case -scalRange:
    whatIsScalarRange
  breaksw

  case -appendVTK:
    appendVTK  
  breaksw

  case -doc:
    mozilla file:///${VE_SUITE_HOME}/docs/starcd/initial.htm &
  breaksw
 
  case -texture:
    vtkTo3DTexture
  breaksw

  case -h:
  default:
  echo ""
  echo Usage:
  echo "VES.inl [ options ] "
  echo "         "-h = usage information "(this page)"
  echo The following indicates the workflow of getting the cfd data into the viewer.
  echo "   "-cfd2VTK = translate cfd flowdata to VTK format
  echo "     "-merge = merge vertices in unstructured grid
  echo "   "-preproc = precompute cutting planes
  echo This will start the viewer using the specified display
  echo "        "-c6 = c6 cave display
  echo "    "-c4open = c4 open cave display
  echo "  "-c4closed = c4 closed cave display
  echo "       "-sim = sim mode desktop display Performer graphics
  echo "    "-simosg = sim mode desktop display OpenSceneGraph graphics
  echo " "-simosgvep = sim mode desktop display OpenSceneGraph Patented graphics
  echo "    "-c6mono = c6 in mono
  echo "    "-c4mono = c4 closed in mono
  echo When the viewer has started, start the menu in another shell
  echo "      "-menu = start wxWidgets menu to control viewer
  echo The following utilities might be useful occasionaly
  echo "     "-nserv = restart TAO nameserver
  echo "   "-surface = make VTK surface from fluid mesh
  echo "  "-surf2stl = convert surface file to stl
  echo "  "-transVTK = move flowdata to align with geometry
  echo "      "-make = make all VE-Suite modules
  echo " "-makeclean = clean all VE-Suite modules
  echo "  "-meshView = view any vtk file
  echo " "-scalRange = view scalar range of any vtk file
  echo " "-appendVTK = append any two VTK files
  echo "       "-doc = view documentation for setting up a CFD post process case
  echo "  "-shutdown = shutdown VE-CE and TAO NameService
  echo "   "-texture = tool to create 3D textures
  echo ""
  
endsw
