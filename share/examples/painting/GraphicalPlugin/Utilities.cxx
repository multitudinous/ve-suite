// Name: VirtualPaint Demo
// Utilities.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011

// STL includes
#include <string>
#include <iostream>
#include <sstream>
#include <osgText/Text>
#include <osg/Geode>
#include <osg/CameraNode>
#include <osg/Depth>
#include <osg/Geometry>
#include <osgDB/ReadFile>
#include <osgDB/FileUtils>

#include "Utilities.h"
#include "GunInputAdapterMouse.h"

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

// **************************************************************************

osg::Node *LoadMousePointer(const string &mouseCursorImage, float sizex, float sizey)

{
	osg::ref_ptr<osg::Geode> geode = new osg::Geode;
	if (geode.valid() == false)
		return 0;
	osg::ref_ptr<osg::Geometry> geom = osg::createTexturedQuadGeometry(osg::Vec3(-sizex, -sizey, 0.0f), osg::Vec3(sizex, 0.0f, 0.0f), osg::Vec3(0.0f, sizey, 0.0f));
	if (geom.valid() == false)
		return 0;
	geode->addDrawable(geom.get());
	// load the mouse pointer image from disk.
	osg::ref_ptr<osg::Image> image = osgDB::readImageFile(mouseCursorImage);
	if (image.valid())
	{
		// create a texture to hold the image.
		osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D(image.get());
		if (texture.valid())
		{
			osg::StateSet *stateSet = geom->getOrCreateStateSet();
			if (stateSet)
			{
				stateSet->setTextureAttributeAndModes(0, texture.get());
				stateSet->setMode(GL_BLEND, osg::StateAttribute::ON);
				stateSet->setRenderBinDetails(10000, "DepthSortedBin");
				// don't write to the depth buffer.
				stateSet->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
				osg::ref_ptr<osg::Depth> depth = new osg::Depth;
				if (depth.valid())
				{
					depth->setWriteMask(false);
					stateSet->setAttributeAndModes(depth.get(), osg::StateAttribute::OFF);
				}
			}
		}
	}
	osg::ref_ptr<osg::CameraNode> camera = new osg::CameraNode;
	if (camera.valid() == false)
		return 0;
	// set the projection matrix.
	camera->setProjectionMatrix(osg::Matrix::ortho2D(-1, 1, -1, 1));
	// set the view matrix.
	camera->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
	camera->setViewMatrix(osg::Matrix::identity());
	// only clear the depth buffer.
	camera->setClearMask(GL_DEPTH_BUFFER_BIT);
	// draw subgraph after main camera view.
	camera->setRenderOrder(osg::CameraNode::NESTED_RENDER);
	// add the mouse cursor / pointer object to the node.
	camera->addChild(geode.get());
	osg::ref_ptr<MouseTracker> mouseTracker = new MouseTracker;
	if (mouseTracker.valid())
		camera->setEventCallback(mouseTracker.get());
	return camera.release();
}

// **************************************************************************

// from http://www.20papercups.net/programming/openscenegraph-dual-screens-twinview/

int getNumScreens(void)
{
    osg::GraphicsContext::WindowingSystemInterface* wsi = osg::GraphicsContext::getWindowingSystemInterface();
    osg::GraphicsContext::ScreenIdentifier si;
    si.readDISPLAY();
    return wsi->getNumScreens(si);
}
