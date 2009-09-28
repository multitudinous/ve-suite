#include "VTKStage.h"
#include "OSGStage.h"
#include <cmath>

int main( int argc,
      char ** argv )
{
    std::cout << "creating vtk pipeline" << std::endl;
    VTKStage* vtkStage = new VTKStage( argv[ 1 ]);
	//VTKStage vtkStage( argv[ 1 ]);
	vtkStage->Update( atoi( argv[ 2 ]) ); //only use every 100th data
	//vtkStage.Dump("C:\\Dougm\\testyang.vtk");
    std::cout << "end vtk pipeline" << std::endl;
    std::cout << "creating osg pipeline" << std::endl;

	OSGStage osgStage;
	
	osg::ref_ptr< osg::Node > root;

	root = osgStage.createInstanced(vtkStage->GetOutput(),"","Density");
    std::cout << "end osg pipeline" << std::endl;

    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    //viewer.setUpViewOnSingleScreen( 0 );
	viewer.setUpViewInWindow(100, 100, 1024,768,0);
    viewer.setSceneData( root.get() );
    return( viewer.run() );
}