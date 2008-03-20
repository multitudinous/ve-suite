// --- My Includes --- //
#include "CameraEntity.h"
#include "CameraEntityCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

//C/C++ Libraries
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity()
:
osg::Camera(),
mNearPlane( 5.0 ),
mFarPlane( 10.0 ),
mDCS( 0 ),
mCameraGeometry( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mFrustumColor( 0 ),
mTexGenNode( 0 ),
mCameraEntityCallback( 0 )
{
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > parentDCS =
        ves::xplorer::scenegraph::SceneManager::instance()->GetWorldDCS();

    Initialize( parentDCS.get() );
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity( ves::xplorer::scenegraph::DCS* parentDCS )
:
osg::Camera(),
mNearPlane( 5.0 ),
mFarPlane( 10.0 ),
mDCS( 0 ),
mCameraGeometry( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mFrustumColor( 0 ),
mTexGenNode( 0 ),
mCameraEntityCallback( 0 )
{
    Initialize( parentDCS );
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity( const CameraEntity& cameraEntity,
                            const osg::CopyOp& copyop )
:
osg::Camera( cameraEntity, copyop ),
mDCS( 0 ),
mCameraGeometry( 0 ),
mFrustumGeode( 0 ),
mFrustumGeometry( 0 ),
mFrustumVertices( 0 ),
mFrustumColor( 0 ),
mTexGenNode( 0 ),
mCameraEntityCallback( 0 )
{
    if( &cameraEntity != this )
    {
        mNearPlane = cameraEntity.mNearPlane;
        mFarPlane = cameraEntity.mFarPlane;
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::~CameraEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::Initialize( ves::xplorer::scenegraph::DCS* parentDCS )
{   
    //Initialize this
    setRenderOrder( osg::Camera::PRE_RENDER );
    setComputeNearFarMode( osg::Camera::DO_NOT_COMPUTE_NEAR_FAR );
    setViewMatrixAsLookAt( osg::Vec3( 0, 0, 0 ),
                           osg::Vec3( 0, 1, 0 ),
                           osg::Vec3( 0, 0, 1 ) );
    setProjectionMatrixAsPerspective( 30.0, 1.0, mNearPlane, mFarPlane );
    parentDCS->addChild( this );

    //Initialize mTexGenNode
    mTexGenNode = new osg::TexGenNode();
    mTexGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    mTexGenNode->setTextureUnit( 0 );
    CalculateMatrixMVPT();
    parentDCS->addChild( mTexGenNode.get() );

    //Initialize mDCS
    mDCS = new ves::xplorer::scenegraph::DCS();
    parentDCS->addChild( mDCS.get() );

    //Initialize mCameraGeometry
    mCameraGeometry = osgDB::readNodeFile( std::string( "Models/camera.ive" ) );
    mDCS->addChild( mCameraGeometry.get() );
    
    //Initialize mFrustumGeode
    CreateViewFrustumGeode();

    //Initialize mCameraEntityCallback
    mCameraEntityCallback = new cpt::CameraEntityCallback();
    setUpdateCallback( mCameraEntityCallback.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CalculateMatrixMVPT()
{
    //Compute the matrix which takes a vertex from local coords into tex coords
    //Multiply the ModelView(MV) by the Projection(P) by the Texture(T) matrix
    mMVPT = getViewMatrix() *
            getProjectionMatrix() *
            osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
            osg::Matrix::scale( 0.5f, 0.5f, 0.5f );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateViewFrustumGeode()
{
    mFrustumGeode = new osg::Geode();
    mFrustumGeometry = new osg::Geometry();
    mFrustumVertices = new osg::Vec3Array();
    mFrustumColor = new osg::Vec4Array();

    mFrustumVertices->resize( 9 );
    mFrustumGeometry->setVertexArray( mFrustumVertices.get() );

    mFrustumColor->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    mFrustumGeometry->setColorArray( mFrustumColor.get() );
    mFrustumGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINES, 8, idxLines ) );
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    mFrustumGeometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    mFrustumGeode->addDrawable( mFrustumGeometry.get() );

    mFrustumGeode->getOrCreateStateSet()->setMode(
        GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    mDCS->addChild( mFrustumGeode.get() );

    UpdateViewFrustumGeode();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    mDCS->setDescriptions( descriptorsList );
    mDCS->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::UpdateViewFrustumGeode()
{
    osg::Matrixd projectionMatrix = getProjectionMatrix();

    //Get near and far from the Projection matrix.
    mNearPlane  = projectionMatrix( 3, 2 ) /
                ( projectionMatrix( 2, 2 ) - 1.0 );
    mFarPlane =  projectionMatrix( 3, 2 ) /
               ( projectionMatrix( 2, 2 ) + 1.0 );

    //Get the sides of the near plane.
    const double nLeft =   mNearPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                          projectionMatrix( 0, 0 );
    const double nRight =  mNearPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                          projectionMatrix( 0, 0 );
    const double nTop =    mNearPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                          projectionMatrix( 1, 1 );
    const double nBottom = mNearPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                          projectionMatrix( 1, 1 );

    //Get the sides of the far plane.
    const double fLeft =   mFarPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double fRight =  mFarPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double fTop =    mFarPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                         projectionMatrix( 1, 1 );
    const double fBottom = mFarPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                         projectionMatrix( 1, 1 );

    //Our vertex array needs only 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    (*mFrustumVertices)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*mFrustumVertices)[ 1 ].set( nLeft, mNearPlane, nBottom );
    (*mFrustumVertices)[ 2 ].set( nRight, mNearPlane, nBottom );
    (*mFrustumVertices)[ 3 ].set( nRight, mNearPlane, nTop );
    (*mFrustumVertices)[ 4 ].set( nLeft, mNearPlane, nTop );
    (*mFrustumVertices)[ 5 ].set( fLeft, mFarPlane, fBottom );
    (*mFrustumVertices)[ 6 ].set( fRight, mFarPlane, fBottom );
    (*mFrustumVertices)[ 7 ].set( fRight, mFarPlane, fTop );
    (*mFrustumVertices)[ 8 ].set( fLeft, mFarPlane, fTop );
    
    mFrustumGeometry->dirtyDisplayList();
    mFrustumGeometry->dirtyBound();
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DrawCameraGeometry( bool onOff )
{
    mCameraGeometry->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DrawViewFrustum( bool onOff )
{
    mFrustumGeode->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CameraEntity::GetDCS()
{
    return mDCS.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::TexGenNode* CameraEntity::GetTexGenNode()
{    
    return mTexGenNode.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::Matrixd CameraEntity::GetMatrixMVPT()
{
    return mMVPT;
}
////////////////////////////////////////////////////////////////////////////////
