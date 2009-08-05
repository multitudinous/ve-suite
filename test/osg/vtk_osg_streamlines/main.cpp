#include "VTKStage.h"
#include "OSGStage.h"
#include <cmath>

#if 1
int main( int argc,
      char ** argv )
{
    std::cout << "creating vtk pipeline" << std::endl;
    VTKStage* vtkStage = new VTKStage( argv[ 1 ]);
	//VTKStage* vtkStage = new VTKStage( "C:\\Dougm\\Downloads\\streamline_output.vtp");
	vtkStage->Update( ); 
	std::cout << "end vtk pipeline" << std::endl;
    std::cout << "creating osg pipeline" << std::endl;

	OSGStage osgStage;
	
	osg::ref_ptr< osg::Group > root = new osg::Group;

	int mult=10;  //This is a mulitiplier to create extra points using linear interplation to smooth out the animation

	root->addChild(osgStage.createInstanced(vtkStage->GetOutput(), mult));
	
    std::cout << "end osg pipeline" << std::endl;

    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    //viewer.setUpViewOnSingleScreen( 0 );
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

    return( 0 );
}
#endif