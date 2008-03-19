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
mDCS( 0 ),
mFrustumGeode( 0 ),
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
mDCS( 0 ),
mFrustumGeode( 0 ),
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
mTexGenNode( 0 ),
mCameraEntityCallback( 0 )
{
    ;
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
    setViewMatrixAsLookAt( osg::Vec3( 0, 0, 0 ),     //eye position
                           osg::Vec3( 0, 1, 0 ),     //center position
                           osg::Vec3( 0, 0, 1 ) );   //up vector
    setProjectionMatrixAsPerspective( 5.0, 1.0, 5.0, 10.0 );
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
    mFrustumGeode = new osg::Geode();
    mDCS->addChild( mFrustumGeode.get() );
    mFrustumGeode->getOrCreateStateSet()->setMode( GL_LIGHTING,
        osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );
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
    osg::Matrixd projectionMatrix = getProjectionMatrix();

    //Get near and far from the Projection matrix.
    const double nearPlane = projectionMatrix( 3, 2 ) /
                           ( projectionMatrix( 2, 2 ) - 1.0 );
    const double farPlane =  projectionMatrix( 3, 2 ) /
                           ( projectionMatrix( 2, 2 ) + 1.0 );

    //Get the sides of the near plane.
    const double nLeft =   nearPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nRight =  nearPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                         projectionMatrix( 0, 0 );
    const double nTop =    nearPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                         projectionMatrix( 1, 1 );
    const double nBottom = nearPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                         projectionMatrix( 1, 1 );

    //Get the sides of the far plane.
    const double fLeft =   farPlane * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                        projectionMatrix( 0, 0 );
    const double fRight =  farPlane * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                        projectionMatrix( 0, 0 );
    const double fTop =    farPlane * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                        projectionMatrix( 1, 1 );
    const double fBottom = farPlane * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                        projectionMatrix( 1, 1 );

    //Our vertex array needs only 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 9 );
    (*vertices)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ].set( nLeft, nearPlane, nBottom );
    (*vertices)[ 2 ].set( nRight, nearPlane, nBottom );
    (*vertices)[ 3 ].set( nRight, nearPlane, nTop );
    (*vertices)[ 4 ].set( nLeft, nearPlane, nTop );
    (*vertices)[ 5 ].set( fLeft, farPlane, fBottom );
    (*vertices)[ 6 ].set( fRight, farPlane, fBottom );
    (*vertices)[ 7 ].set( fRight, farPlane, fTop );
    (*vertices)[ 8 ].set( fLeft, farPlane, fTop );

    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    geometry->setVertexArray( vertices.get() );

    osg::ref_ptr< osg::Vec4Array > color = new osg::Vec4Array();
    color->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geometry->setColorArray( color.get() );
    geometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    geometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINES, 8, idxLines ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort(
        osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    mFrustumGeode->addDrawable( geometry.get() );
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
