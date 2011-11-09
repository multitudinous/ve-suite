// Name: VirtualPaint Demo
// RTTScene.cpp
//
// Authors:
//   Chris 'Xenon' Hanson, AlphaPixel.
//   John Enright, Digital Transforms.
//
// Origination Date: July 19, 2011
//
// Technique inspired by ISU examples 'rtt.cpp'

#include "RTTScene.h"
#include "ShaderSupport.h"

#include <osg/Camera>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/Texture2D>
#include <osg/TexEnv>
#include <osgViewer/Viewer>
#include <osg/ClearNode>
#include <osg/BlendEquation>
#include <osg/BlendFunc>
#include <osg/PositionAttitudeTransform>
#include <osgwTools/Version.h>
#include <osgDB/ReadFile>

// a convenience so don't have to prefix STL stuff with "std::".
using namespace std;
// ditto osg
using namespace osg;

// may need to access these from elsewhere
osg::ref_ptr< osg::Camera > preRenderCamera;
osg::ref_ptr<osg::Texture2D> RTTtex;
osg::ref_ptr<osg::PositionAttitudeTransform> brushPAT(new osg::PositionAttitudeTransform);
osg::ref_ptr<osg::ClearNode> clear;

bool clearOnNextFrame;

void setClearOnNextFrame(void)
{
	clearOnNextFrame = true; // just once
} // setClearOnNextFrame

osg::Node* createPreRenderSubGraph( double aspect )
{
	// add the RTT geometry here.

	// add transform so we can move it around
	brushPAT = new osg::PositionAttitudeTransform;

	clear = new osg::ClearNode;
	clear->setRequiresClear(false); // we don't want to clear the texture every time
	clearOnNextFrame = false;
	clear->addChild(brushPAT.get());

	// load the brush image
	osg::ref_ptr<osg::Image> brushPic = osgDB::readImageFile("images/BrushMask.png");
	if ((brushPic.valid() == false) || (brushPic->data() == 0))
		return NULL;

	// make a brush-sized quad below the PAT
	osg::ref_ptr<osg::Geode> geode( new osg::Geode );
	const float scale(0.001f), scaleTwo(scale * 2.0f);
	osg::Vec3 brushProportional(brushPic->s(), brushPic->t(), 0.0f);

	// <<<>>> all this sizing is a guess for now
	geode->addDrawable( osg::createTexturedQuadGeometry(
		osg::Vec3( -brushProportional.x() * scale / aspect, -brushProportional.y() * scale, 0.0f ),
		osg::Vec3( brushProportional.x() * scaleTwo / aspect, 0.0f, 0.0f ), osg::Vec3( 0.0f, brushProportional.y() * scaleTwo, 0.0f ) ) );


	// create a texture to hold the image.
	osg::ref_ptr<osg::Texture2D> texture = new osg::Texture2D;
	if (texture.valid())
	{
		texture->setImage(brushPic.get()); // set the texture image.
		texture->setWrap(osg::Texture::WRAP_S, osg::Texture::CLAMP_TO_EDGE); 
		texture->setWrap(osg::Texture::WRAP_T, osg::Texture::CLAMP_TO_EDGE);
	} // if

	// add the texture to the brush Geode.
	geode->getOrCreateStateSet()->setTextureAttributeAndModes(
		TEXUNIT_SPATTER, texture.get(), osg::StateAttribute::ON );

    // set the spatter shaders
    setupSpatterShaders(geode.get());

	// For this state set, turn blending on (so alpha texture looks right)
	geode->getOrCreateStateSet()->setMode(GL_BLEND,osg::StateAttribute::ON);

    // set the blend equation to ADD
    osg::BlendEquation* blendAdd = new osg::BlendEquation(osg::BlendEquation::FUNC_ADD);
    geode->getOrCreateStateSet()->setAttributeAndModes(blendAdd, osg::StateAttribute::ON);

    // make sure the BlendFunc is what we want (1, 1)
    osg::BlendFunc *blendOneOne = new osg::BlendFunc(osg::BlendFunc::ONE, osg::BlendFunc::ONE);
    geode->getOrCreateStateSet()->setAttributeAndModes(blendOneOne, osg::StateAttribute::ON);

	// turn off lighting
	geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF );
	// don't test against the depth buffer.
	geode->getOrCreateStateSet()->setMode(GL_DEPTH_TEST, osg::StateAttribute::OFF);
	brushPAT->addChild(geode.get());

	// initially, disable the brush geode at the PAT level
	brushPAT->setNodeMask(0);
	// and center it
	brushPAT->setPosition(osg::Vec3(0.0f, 0.0f, 0.0f));

	return( clear.get() ); // not release() because there's a global copy. Ugly.

} // createPreRenderSubGraph

void configureCameraReferenceFrame(osg::Camera *camera)
{
	// setup fullscreen rendering coordinate space
	camera->setReferenceFrame( osg::Camera::ABSOLUTE_RF );
	camera->setViewMatrix( osg::Matrixd::identity() );
	camera->setProjectionMatrix( osg::Matrixd::identity() );

} // configureCameraReferenceFrame


osg::Node* createPreRenderGraph( const unsigned int &width, const unsigned int &height)
{
	preRenderCamera = new osg::Camera;

	// Create the texture. We'll use this as our output buffer.
	// Note it has no image data; not required.
	RTTtex = new osg::Texture2D;
	RTTtex->setTextureWidth( width );
	RTTtex->setTextureHeight( height );
	RTTtex->setInternalFormat( GL_RGBA );
	RTTtex->setBorderWidth( 0 );
	RTTtex->setFilter( osg::Texture::MIN_FILTER, osg::Texture::NEAREST );
	RTTtex->setFilter( osg::Texture::MAG_FILTER, osg::Texture::NEAREST );

	// Attach the texture to the camera. Tell it to use multisampling.
	// Internally, OSG allocates a multisampled renderbuffer, renders to it,
	// and at the end of the frame performs a BlitFramebuffer into our texture.
	preRenderCamera->attach( osg::Camera::COLOR_BUFFER0, RTTtex.get(), 0, 0, false, 0, 0 );
	preRenderCamera->setRenderTargetImplementation( osg::Camera::FRAME_BUFFER_OBJECT, osg::Camera::FRAME_BUFFER );
#if( OSGWORKS_OSG_VERSION >= 20906 )
	preRenderCamera->setImplicitBufferAttachmentMask(
		osg::Camera::IMPLICIT_COLOR_BUFFER_ATTACHMENT|osg::Camera::IMPLICIT_DEPTH_BUFFER_ATTACHMENT,
		osg::Camera::IMPLICIT_COLOR_BUFFER_ATTACHMENT );
#endif

	preRenderCamera->setRenderOrder( osg::Camera::PRE_RENDER );

	// setup fullscreen rendering coordinate space
	configureCameraReferenceFrame(preRenderCamera.get());

	// this step seems to be necessary with the prerender camera, though the ISU
	// rtt example (which is a post render camera) lacks it
	preRenderCamera->setViewport(0,0,width,height);

	preRenderCamera->addChild(createPreRenderSubGraph( (double)width/(double)height ));

	return( preRenderCamera.release() );
} // createPreRenderGraph

void frameUpdate(void)
{

	if(clearOnNextFrame)
	{
		clear->setRequiresClear(true); // just this once
		clearOnNextFrame = false;
	} // if
	else
	{
		clear->setRequiresClear(false); // back to normal
	} // else

} // frameUpdate


void RTTSpray(double xPos, double yPos, double xMax, double yMax, float strength)
{
	osg::Vec3 newCenter(((xPos * 2)/xMax) - 1.0f, ((yPos * 2)/yMax) - 1.0f, 0.0f);

	// position the brush
	brushPAT->setPosition(newCenter);
	brushPAT->setNodeMask(~0); // show the brush

	// pass strength
    setSpatterStrength(strength);

} // RTTSpray

void StopRTTSpray(void)
{
	brushPAT->setNodeMask(0);
} // StopRTTSpray

