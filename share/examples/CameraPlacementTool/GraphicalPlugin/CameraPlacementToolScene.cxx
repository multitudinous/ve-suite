// --- My Includes --- //
#include "CameraPlacementToolScene.h"
#include "CameraPlacementToolShaders.h"
#include "CameraEntity.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Texture2D>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

// --- Bullet Includes --- //

// --- C/C++ Libraries --- //

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::CameraPlacementToolScene(
    ves::xplorer::scenegraph::DCS* pluginDCS )
:
mShaders(),
mCameraEntity( 0 ),
mPluginDCS( pluginDCS ),
mTorus( 0 )
{
    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolScene::~CameraPlacementToolScene()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
cpt::CameraEntity* CameraPlacementToolScene::GetActiveCameraEntity()
{
    return mCameraEntity.get();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolScene::Initialize()
{
    //Initialize the shader programs
    mShaders = cpt::CameraPlacementToolShadersPtr(
        new cpt::CameraPlacementToolShaders() );

    //Initialize the camera entities
    mCameraEntity = new cpt::CameraEntity( mPluginDCS.get() );

    mCameraEntity->SetNameAndDescriptions( std::string( "Camera" ) );

    mTorus = new ves::xplorer::scenegraph::DCS();
    mTorus->setPosition( osg::Vec3d( 0, 10, 0 ) );
    osg::ref_ptr< osg::Node > node =
        osgDB::readNodeFile( std::string( "Models/torus.ive" ) );
    mTorus->addChild( node.get() );
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mTorus->setDescriptions( descriptorsList );
    mTorus->setName( std::string( "Torus" ) );

    mTorus->setStateSet( mCameraEntity->getStateSet() );

    mPluginDCS->AddChild( mTorus.get() );
}
////////////////////////////////////////////////////////////////////////////////
