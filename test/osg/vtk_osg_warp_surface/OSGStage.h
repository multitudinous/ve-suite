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
#include "VTKStage.h"
#include <vtkPointData.h>
#include <math.h>

#include <osgViewer/Viewer>
#include <osgViewer/ViewerEventHandlers>
#include <osgGA/TrackballManipulator>

#include <osg/Geometry>
#include <osg/io_utils>
#include <osg/Math>

// Allows you to change the animation play rate:
//   '+' speed up
//   '-' slow down
//   'p' pause
class PlayStateHandler
    : public osgGA::GUIEventHandler
{
public:
    PlayStateHandler()
      : elapsedTime( 0. ),
        scalar( 1. ),
        paused( false )
    {}

    double getCurrentTime() const
    {
        if( paused )
            return( elapsedTime );
        else
            return( elapsedTime + ( timer.time_s() * scalar ) );
    }

    virtual bool handle( const osgGA::GUIEventAdapter & event_adaptor,
                         osgGA::GUIActionAdapter & action_adaptor );

private:
    osg::Timer timer;
    double elapsedTime;
    double scalar;
    bool paused;
};

class OSGStage
{
public:
	OSGStage(void);
	~OSGStage(void);

	//create a osgNode
	//polydata is supposed to be triangle strips
	//displacement is the vetice displacemnet vector for each of the point data in polydata
	//colorScalar is the scalar for the point data in polydata used to color the result
	osg::Group* createMesh(vtkPolyData* polydata, string displacement, string colorScalar);

private:

	//an m x n texture is used to transfer data
	int tm; 
	int tn;

	//two utility functions used to determine tm and tn, since texture dimension needs to be 2^n
	int mylog2(unsigned x);
	int mypow2(unsigned x);
		
	void OSGStage::createMeshData( osg::Geometry* geom, vtkPolyData* polydata, string displacement, string colorScalar);

};
