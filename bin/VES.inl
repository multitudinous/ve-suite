#!/bin/sh -f
 
# Set environment variables and paths for running the program
export VE_SUITE_HOME=/home/virtual1/VE_Suite_Builds/VE_Suite

# Set the TAO nameServer properties used by the run scripts
export TAO_MACHINE=virtual2.inel.gov
export TAO_PORT=1234

export TAO_BUILD=TRUE
export CLUSTER_APP=FALSE
source ${VE_SUITE_HOME}/VE_Installer/setup.INL.sh

export MY_VJCONFIGS=/home/virtual1/VE_Suite_Builds/bay_5_vr

case $1 in
  
  -sim)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/project_tao \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    ${VJ_BASE_DIR}/share/vrjuggler/data/configFiles/simstandalone.jconf
  ;;

  -wall)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/project_cluster_tao \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService \
    ${MY_VJCONFIGS}bay_5_vr.jconf
  ;;

  -nserv)
    killall Naming_Service 
    killall Exe_server 
    echo Starting TAO Naming Service on $TAO_MACHINE, port $TAO_PORT
    Naming_Service -ORBEndPoint iiop://${TAO_MACHINE}:${TAO_PORT} &
    sleep 5
    echo TAO Naming Service Started
    echo Starting VE-CE...
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/Exe_server \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService &
  ;;
  
  -shutdown)
    killall Naming_Service 
    killall Exe_server 
  ;;

  -menu)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/WinClient \
    -ORBInitRef NameService=corbaloc:iiop:${TAO_MACHINE}:${TAO_PORT}/NameService
  ;;

  -surface)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/makeVtkSurface
  ;;

  -preproc)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/preprocessor
  ;;

  -surf2stl)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/convertSurfaceFileToStl
  ;;

  -transVTK)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/transformVTK
  ;;

  -cfd2VTK)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/translateToVtk
  ;;

  -make)
    cd ${VE_SUITE_HOME}/VE_Installer
    gmake
  ;;
  
  -makeclean)
    cd ${VE_SUITE_HOME}/VE_Installer
    gmake clean
    gmake cleandepend
  ;;
  
  -merge)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/mergeVertices
  ;;

  -meshView)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/meshViewer
  ;;

  -scalRange)
    ${VE_SUITE_HOME}/bin/${CFDHOSTTYPE}/whatIsScalarRange
  ;;

  -doc)
    mozilla file:///${VE_SUITE_HOME}/docs/starcd/initial.htm &
  ;;
 
  *)
   echo Use -h for help
   ;;
 
  -h)
  
  echo ""
  echo Usage:
  echo VES.inl [ options ] 
  echo "         "-h = usage information "(this page)"
  echo The following indicates the workflow of getting the cfd data into the viewer.
  echo "   "-cfd2VTK = translate cfd flowdata to VTK format
  echo "     "-merge = merge vertices in unstructured grid
  echo "   "-preproc = precompute cutting planes
  echo This will start the viewer using the specified display
  echo "       "-sim = sim mode desktop display
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
  echo "       "-doc = view documentation for setting up a CFD post process case
  echo "  "-shutdown = shutdown VE-CE and TAO NameService
  echo ""
  ;;
esac
