// --- My Includes --- //
#include "CameraEntity.h"
#include "CameraEntityCallback.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Camera>
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/TexGenNode>

#include <osgDB/ReadFile>

//C/C++ Libraries
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
CameraEntity::CameraEntity( ves::xplorer::scenegraph::DCS* parentDCS )
:
m_camera( 0 ),
m_dcs( 0 ),
m_frustum( 0 ),
m_texGenNode( 0 ),
m_cameraEntityCallback( 0 )
{
    Initialize( parentDCS );
}
////////////////////////////////////////////////////////////////////////////////
CameraEntity::~CameraEntity()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::Initialize( ves::xplorer::scenegraph::DCS* parentDCS )
{   
    m_camera = new osg::Camera();
    m_camera->setRenderOrder( osg::Camera::PRE_RENDER );
    //Set the camera defaults
    m_camera->setViewMatrixAsLookAt( osg::Vec3( 0, 0, 0 ),     //eye position
                                     osg::Vec3( 0, 1, 0 ),     //center position
                                     osg::Vec3( 0, 0, 1 ) );   //up vector
    m_camera->setProjectionMatrixAsPerspective( 5.0, 1.0, 5.0, 10.0 );
    parentDCS->addChild( m_camera.get() );

    m_dcs = new ves::xplorer::scenegraph::DCS();
    m_dcs->addChild( osgDB::readNodeFile( std::string( "Models/camera.ive" ) ) );
    parentDCS->addChild( m_dcs.get() );

    CreateViewFrustumGeometry();

    m_texGenNode = new osg::TexGenNode();
    m_texGenNode->getTexGen()->setMode( osg::TexGen::EYE_LINEAR );
    m_texGenNode->setTextureUnit( 0 );
    parentDCS->addChild( m_texGenNode.get() );

    //Compute the matrix which takes a vertex from local coords into tex coords
    //Multiply the ModelView(MV) by the Projection(P) by the Texture(T) matrix
    osg::Matrixd MVPT = m_camera->getViewMatrix() *
                        m_camera->getProjectionMatrix() *
                        osg::Matrix::translate( 1.0f, 1.0f, 1.0f ) *
                        osg::Matrix::scale( 0.5f, 0.5f, 0.5f );

    //Set the callback on m_camera since m_dcs already has a callback
    m_cameraEntityCallback = new cpt::CameraEntityCallback();
    m_cameraEntityCallback->SetDCS( m_dcs.get() );
    m_cameraEntityCallback->SetTexGenNode( m_texGenNode.get() );
    m_cameraEntityCallback->SetMatrixMVPT( MVPT );
    m_camera->setUpdateCallback( m_cameraEntityCallback.get() );
}

////////////////////////////////////////////////////////////////////////////////
void CameraEntity::CreateViewFrustumGeometry()
{
    osg::Matrixd projectionMatrix = m_camera->getProjectionMatrix();

    //Get near and far from the Projection matrix.
    const double near = projectionMatrix( 3, 2 ) /
                      ( projectionMatrix( 2, 2 ) - 1.0 );
    const double far =  projectionMatrix( 3, 2 ) /
                      ( projectionMatrix( 2, 2 ) + 1.0 );

    //Get the sides of the near plane.
    const double nLeft =   near * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                    projectionMatrix( 0, 0 );
    const double nRight =  near * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                    projectionMatrix( 0, 0 );
    const double nTop =    near * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                    projectionMatrix( 1, 1 );
    const double nBottom = near * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                    projectionMatrix( 1, 1 );

    //Get the sides of the far plane.
    const double fLeft =   far * ( projectionMatrix( 2, 0 ) - 1.0 ) /
                                   projectionMatrix( 0, 0 );
    const double fRight =  far * ( projectionMatrix( 2, 0 ) + 1.0 ) /
                                   projectionMatrix( 0, 0 );
    const double fTop =    far * ( projectionMatrix( 2, 1 ) + 1.0 ) /
                                   projectionMatrix( 1, 1 );
    const double fBottom = far * ( projectionMatrix( 2, 1 ) - 1.0 ) /
                                   projectionMatrix( 1, 1 );

    //Our vertex array needs only 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    osg::ref_ptr< osg::Vec3Array > vertices = new osg::Vec3Array();
    vertices->resize( 9 );
    (*vertices)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*vertices)[ 1 ].set( nLeft, near, nBottom );
    (*vertices)[ 2 ].set( nRight, near, nBottom );
    (*vertices)[ 3 ].set( nRight, near, nTop );
    (*vertices)[ 4 ].set( nLeft, near, nTop );
    (*vertices)[ 5 ].set( fLeft, far, fBottom );
    (*vertices)[ 6 ].set( fRight, far, fBottom );
    (*vertices)[ 7 ].set( fRight, far, fTop );
    (*vertices)[ 8 ].set( fLeft, far, fTop );

    osg::ref_ptr< osg::Geometry > geometry = new osg::Geometry();
    geometry->setVertexArray( vertices.get() );

    osg::ref_ptr< osg::Vec4Array > color = new osg::Vec4Array();
    color->push_back( osg::Vec4( 1.0, 1.0, 0.0, 1.0 ) );
    geometry->setColorArray( color.get() );
    geometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINES, 8, idxLines ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    m_frustum = new osg::Geode();
    m_frustum->addDrawable( geometry.get() );

    m_frustum->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    m_dcs->addChild( m_frustum.get() );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::SetNameAndDescriptions( const std::string& name )
{
    osg::Node::DescriptionList descriptorsList;
    descriptorsList.push_back( "VE_XML_ID" );
    descriptorsList.push_back( "" );
    m_dcs->setDescriptions( descriptorsList );
    m_dcs->setName( name );
}
////////////////////////////////////////////////////////////////////////////////
void CameraEntity::DrawViewFrustum( bool onOff )
{
    m_frustum->setNodeMask( onOff );
}
////////////////////////////////////////////////////////////////////////////////
ves::xplorer::scenegraph::DCS* CameraEntity::GetDCS()
{
    return m_dcs.get();
}
////////////////////////////////////////////////////////////////////////////////
osg::TexGenNode* CameraEntity::GetTexGenNode()
{    
    return m_texGenNode.get();
}
////////////////////////////////////////////////////////////////////////////////