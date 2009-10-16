#include "VTKStage.h"
#include "OSGStage.h"
#include <cmath>

int main( int argc,
      char ** argv )
{
    std::cout << "creating vtk pipeline" << std::endl;
    VTKStage vtkStage( argv[ 1 ]);
	vtkStage.Update(); 
    std::cout << "end vtk pipeline" << std::endl;
    std::cout << "creating osg pipeline" << std::endl;

	OSGStage* osgStage = new OSGStage();
	osg::ref_ptr< osg::Group > root = new osg::Group;
	root->addChild( osgStage->createMesh(vtkStage.GetOutput(), "displacement 0 mm", "fg max prin stress 0 MPa" ) );

    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    viewer.setUpViewInWindow(100, 100, 1024,768,0);
    viewer.setSceneData( root.get() );

    viewer.setCameraManipulator( new osgGA::TrackballManipulator );

    // Create a PlayStateHandler to track elapsed simulation time
    // and play/pause state.
    osg::ref_ptr< PlayStateHandler > psh = new PlayStateHandler;
    viewer.addEventHandler( psh.get() );

    while (!viewer.done())
    {
        // Get time from the PlayStateHandler.
        double simTime = psh->getCurrentTime();
        viewer.frame( simTime );
    }

	return 0;
}
