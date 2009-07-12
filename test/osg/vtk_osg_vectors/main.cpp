#include "VTKStage.h"
#include "OSGStage.h"

int main( int argc,
      char ** argv )
{
    
	VTKStage vtkStage( argv[ 1 ]);
	vtkStage.Update(100); //only use every 100th data
	//vtkStage.Dump("C:\\Dougm\\testyang.vtk");

	OSGStage osgStage;
	
	osg::ref_ptr< osg::Node > root;

	root = osgStage.createInstanced(vtkStage.GetOutput());

    osgViewer::Viewer viewer;
    viewer.addEventHandler( new osgViewer::StatsHandler );
    //viewer.setUpViewOnSingleScreen( 0 );
	viewer.setUpViewInWindow(100, 100, 1024,768,0);
    viewer.setSceneData( root.get() );
    return( viewer.run() );
}