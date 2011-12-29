// Name: VirtualPaint Demo
// MainScene.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011
//
// Technique inspired by ISU examples 'rtt.cpp'

#include "MainScene.h"
#include "ShaderSupport.h"

#include <osg/Node>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgDB/ReadFile>
#include <osg/MatrixTransform>
#include <osgViewer/Viewer>
#include <osgUtil/TangentSpaceGenerator>
#include <osg/Depth>
#include <osgwTools/Version.h>

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

static Geometry* createTexturedQuadGeometryLocal(const Vec3& corner,const Vec3& widthVec,const Vec3& heightVec, float l, float b, float r, float t);


// this is taken from the RTT example, and it sets the reference frame,
// projection and view/projection matrix on the top-level Camera.
// We don't wish to do this, because we may have both screen-space AND
// non-screen-space elements in the graph under the top-level child.
// See BuildBackground below.

void createMainRenderGraph( osg::Camera *mainCamera, osg::Texture2D* )
{
	// Configure Camera to draw fullscreen textured quad
	mainCamera->setClearColor( osg::Vec4( 0., 1., 0., 1. ) ); // should never see this.
	mainCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER, osg::Camera::FRAME_BUFFER ); // redundent. these are the default values

	mainCamera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
	mainCamera->setViewMatrix( osg::Matrixd::identity() );
	mainCamera->setProjectionMatrix( osg::Matrixd::identity() );

	// a geode to hold the full-scene quad
	osg::ref_ptr<osg::Geode> geode = new osg::Geode;
	if (geode.valid() == false)
		return;
	// build a quad for texturing. (Use local copy of createTexturedQuadGeometry)
	osg::ref_ptr<osg::Geometry> geom = createTexturedQuadGeometryLocal(osg::Vec3( -1,-1,0 ), osg::Vec3( 2,0,0 ), osg::Vec3( 0,2,0 ), 0.0f, 0.0f, 1.0f, 1.0f);
	if (geom.valid() == false)
		return;
	// Compute tangent space for geometry
	osg::ref_ptr< osgUtil::TangentSpaceGenerator > tsg = new osgUtil::TangentSpaceGenerator;
	tsg->generate( geom.get(), 0 ); // all textures in this test use the same texcoords, so we use 0 and not TEXUNIT_BUMP
	geom->setVertexAttribData( TANGENT_ATR_UNIT, osg::Geometry::ArrayData(tsg->getTangentArray(), osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
	geom->setVertexAttribData( BINORMAL_ATR_UNIT, osg::Geometry::ArrayData(tsg->getBinormalArray(), osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );

	geode->addDrawable(geom.get());

	// load an background image from disk.
	osg::ref_ptr<osg::Image> image = osgDB::readImageFile("images/paperUV.png");
	if (image.valid())
	{
		// create a texture to hold the image.
		osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D;
		if (texture.valid())
		{
			texture->setImage(image.get());                 // set the texture image.
			// add the texture to the Geode.
			geode->getOrCreateStateSet()->setTextureAttributeAndModes(TEXUNIT_BG, texture.get());
		}
	}

/*
	geode->getOrCreateStateSet()->setTextureAttributeAndModes(
		0, tex, osg::StateAttribute::ON );
*/
	// set the render order to be before normal OSG stuff.
	geode->getOrCreateStateSet()->setRenderBinDetails(-1, "RenderBin");
	// don't write to the depth buffer or compare against it.
	geode->getOrCreateStateSet()->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
	osg::ref_ptr<osg::Depth> depth = new osg::Depth;
	if (depth)
	{
		depth->setWriteMask(false);
		geode->getOrCreateStateSet()->setAttributeAndModes(depth.get(), osg::StateAttribute::OFF);
	}
	geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

	mainCamera->addChild( geode.get() );

} // createMainRenderGraph

// **************************************************************************

// This creates a Projection and Matrix and uses them to set the reference frame
// and projection matrix for the child subgraph, allowing other children of this
// camera to potentially have different projection.

osg::Node *BuildBackground(void)

{
	// a geode to hold the full-scene quad
	osg::ref_ptr<osg::Geode> geode = new osg::Geode;
	if (geode.valid() == false)
		return 0;
	// build a quad for texturing. (Use local copy of createTexturedQuadGeometry)
	osg::ref_ptr<osg::Geometry> geom = createTexturedQuadGeometryLocal(osg::Vec3(0, 0, 0), osg::Vec3(1.0f, 0, 0), osg::Vec3(0, 1.0f, 0), 0.0f, 0.0f, 1.0f, 1.0f);
	if (geom.valid() == false)
		return 0;
	// Compute tangent space for geometry
	osg::ref_ptr< osgUtil::TangentSpaceGenerator > tsg = new osgUtil::TangentSpaceGenerator;
	tsg->generate( geom.get(), 0 ); // all textures in this test use the same texcoords, so we use 0 and not TEXUNIT_BUMP
	geom->setVertexAttribData( TANGENT_ATR_UNIT, osg::Geometry::ArrayData(tsg->getTangentArray(), osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );
	geom->setVertexAttribData( BINORMAL_ATR_UNIT, osg::Geometry::ArrayData(tsg->getBinormalArray(), osg::Geometry::BIND_PER_VERTEX, GL_FALSE ) );

	geode->addDrawable(geom.get());

	// load an background image from disk.
	osg::ref_ptr<osg::Image> image = osgDB::readImageFile("images/paperUV.png");
	if (image.valid())
	{
		// create a texture to hold the image.
		osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D;
		if (texture.valid())
		{
			texture->setImage(image.get());                 // set the texture image.
			// add the texture to the Geode.
			geode->getOrCreateStateSet()->setTextureAttributeAndModes(TEXUNIT_BG, texture.get());
		}
	}
	// build a MatrixTransform and Projection so that it covers the entire screen and
	// does not do any depth testing (so that it doesn't conflict with the draw space
	// of the painting surface).
	osg::ref_ptr<osg::MatrixTransform> matrix = new osg::MatrixTransform();
	if (matrix.valid() == false)
		return 0;
	matrix->setReferenceFrame(osg::Transform::ABSOLUTE_RF);
	matrix->setMatrix(osg::Matrix::identity());
	// add the geode with the textured quad to the matrix.
	matrix->addChild(geode.get());
	// create a projection to view it properly (in flat 2D).
	osg::ref_ptr<osg::Projection> proj = new osg::Projection();
	if (proj.valid() == false)
		return 0;
	proj->setMatrix(osg::Matrix::ortho2D(0, 1, 0, 1));
	// add the matrix to the projection.
	proj->addChild(matrix.get());
	// get the stateset of the projection in order to modify a few things.
	osg::ref_ptr<osg::StateSet> stateSet = proj->getOrCreateStateSet();
	if (stateSet)
	{
		// set the render order to be before normal OSG stuff.
		stateSet->setRenderBinDetails(-1, "RenderBin");
		// don't write to the depth buffer.
		stateSet->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
		osg::ref_ptr<osg::Depth> depth = new osg::Depth;
		if (depth)
		{
			depth->setWriteMask(false);
			stateSet->setAttributeAndModes(depth.get(), osg::StateAttribute::OFF);
		}
	}
	// don't let lighting affect the backdrop.
	proj->getOrCreateStateSet()->setMode(GL_LIGHTING, osg::StateAttribute::OFF);
	return proj.release();
}


// taken from Geometry.cpp, and modified to support multiple texcoords
Geometry* createTexturedQuadGeometryLocal(const Vec3& corner,const Vec3& widthVec,const Vec3& heightVec, float l, float b, float r, float t)
{
	Geometry* geom = new Geometry;

	Vec3Array* coords = new Vec3Array(4);
	(*coords)[0] = corner+heightVec;
	(*coords)[1] = corner;
	(*coords)[2] = corner+widthVec;
	(*coords)[3] = corner+widthVec+heightVec;
	geom->setVertexArray(coords);

	Vec2Array* tcoords = new Vec2Array(4);
	(*tcoords)[0].set(l,t);
	(*tcoords)[1].set(l,b);
	(*tcoords)[2].set(r,b);
	(*tcoords)[3].set(r,t);
	geom->setTexCoordArray(0,tcoords);
	geom->setTexCoordArray(1,tcoords);

	osg::Vec4Array* colours = new osg::Vec4Array(4);
	(*colours)[0].set(1.0f,1.0f,1.0,1.0f);
	(*colours)[1].set(1.0f,1.0f,1.0,1.0f);
	(*colours)[2].set(1.0f,1.0f,1.0,1.0f);
	(*colours)[3].set(1.0f,1.0f,1.0,1.0f);
	geom->setColorArray(colours);

	osg::Vec3Array* normals = new osg::Vec3Array(4);
	osg::Vec3 normal = widthVec^heightVec;
	normal.normalize();
	(*normals)[0] = normal;
	(*normals)[1] = normal;
	(*normals)[2] = normal;
	(*normals)[3] = normal;
	geom->setNormalArray(normals);

	geom->addPrimitiveSet(new DrawArrays(PrimitiveSet::QUADS,0,4));

	return geom;
}


// **************************************************************************
// load the model that will be used as the paint surface.

osg::Node *LoadPaintSurface(void)
{
   osg::ref_ptr<osg::Node> model = osgDB::readNodeFile("models/door.osg");
   if (model.valid() == false)
      return 0;
   return model.release();

// this code can be used to programmatically rotate a model if the paint surface models are
// consistently built with a different orientation. Alternatively, osgconv can be used to
// convert and re-orient the model. e.g.
//
//    osgconv -o 90-1,0,0 door.3ds door.osg
//

/*
#ifndef PI
#define PI 3.1415926535897932384626433832795
#endif

#define DegreesToRads (PI / 180.0)
#define RadsToDegrees (180.0 / PI)

   osg::ref_ptr<osg::PositionAttitudeTransform> xform = new osg::PositionAttitudeTransform;
   if (xform.valid() == false)
      return 0;
   xform->addChild(model);
   osg::Quat rot(90 * DegreesToRads, osg::X_AXIS, 0, osg::Y_AXIS, 0, osg::Z_AXIS);
   xform->setAttitude(rot);
   return xform.release();
*/
}

// **************************************************************************
