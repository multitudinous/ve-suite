#pragma once
 //This class wraps the OSG Vertex Shader based rendering
 //the Testure2D data is used as away to transfer data into the shader
 //It acts as a raw array instead of 2D data
 
#ifdef WIN32
#include <windows.h>
#endif
 
#include <osgDB/ReadFile>
#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osg/Geometry>
#include <osg/PositionAttitudeTransform>
#include <vtkDebugLeaks.h>
#include <vtkCellDataToPointData.h>
#include <vtkPolyData.h>
#include <vtkPolyDataMapper.h>
#include <vtkGlyph3D.h>
#include <vtkDataSet.h>
#include <vtkDataSetReader.h>
#include <vtkCellArray.h>
#include <vtkMaskPoints.h>
#include <vtkPlane.h>
#include <vtkPointData.h>
#include <math.h>
#include <ves/xplorer/event/viz/cfdPresetVector.h>
#include <vtksys/ios/sstream>
#include <iostream>
#include <string>
 
using namespace std;
 
 
namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class VE_SCENEGRAPH_EXPORTS OSGStage
{
public:
       OSGStage(void);
       ~OSGStage(void);

       //create a osgNode
       osg::Geode* createInstanced(vtkPolyData* glyph, string vectorName="", string scalarName="", osg::Geode* geode = NULL);
 
private:

       //an m x n texture is used to transfer data
       int tm;
       int tn;
 
       //two utility functions used to determine tm and tn, since texture dimension needs to be 2^n
       int mylog2(unsigned x);
       int mypow2(unsigned x);
 
       //create the glyph arrow in OSG
       void createArrow( osg::Geometry& geom, int nInstances=1 );
 
       //create the position array based on the passed in VTK points
       float* createPositionArray( int m, int n , vtkPoints* points);

       //create the attitude (rotation) array based on the passed VTK dataArray (dataArray should have 3 components per tuple)
       float* createAttitudeArray( int m, int n, vtkDataArray* dataArray);

       //create the scalar data array based on the passed VTK dataArray (dataArray should have 1 components per tuple)
       float* createScalarArray( int m, int n, vtkDataArray* dataArray);


};
} //end scenegraph
} //end xplorer
} //end ves
