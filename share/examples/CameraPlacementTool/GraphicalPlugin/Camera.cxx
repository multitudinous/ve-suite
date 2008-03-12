// --- My Includes --- //
#include "Camera.h"

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/DCS.h>

// --- OSG Includes --- //
#include <osg/Geode>
#include <osg/Geometry>
#include <osg/MatrixTransform>

#include <osgDB/ReadFile>

//C/C++ Libraries
#include <iostream>

using namespace cpt;

////////////////////////////////////////////////////////////////////////////////
Camera::Camera( ves::xplorer::scenegraph::DCS* parentDCS )
:
m_dcs( new ves::xplorer::scenegraph::DCS() )
{
    parentDCS->AddChild( m_dcs.get() );

    Initialize();
}
////////////////////////////////////////////////////////////////////////////////
Camera::~Camera()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void Camera::Initialize()
{
    m_dcs->addChild( osgDB::readNodeFile( std::string( "Models/camera.ive" ) ) );

    //Set the projection matrix for the camera
    osg::Matrixd proj;
    proj.makePerspective( 30.0, 1.0, 1.0, 5.0 );
    m_projectionMatrix.set( proj.ptr() );

    DrawViewFrustum();
}
////////////////////////////////////////////////////////////////////////////////
void Camera::DrawViewFrustum()
{
    //Get near and far from the Projection matrix.
    const double near = m_projectionMatrix.mData[ 11 ] /
                      ( m_projectionMatrix.mData[ 10 ] - 1.0 );
    const double far =  m_projectionMatrix.mData[ 11 ] /
                      ( m_projectionMatrix.mData[ 10 ] + 1.0 );

    //Get the sides of the near plane.
    const double nLeft =   near * ( m_projectionMatrix.mData[ 2 ] - 1.0 ) /
                                    m_projectionMatrix.mData[ 0 ];
    const double nRight =  near * ( m_projectionMatrix.mData[ 2 ] + 1.0 ) /
                                    m_projectionMatrix.mData[ 0 ];
    const double nTop =    near * ( m_projectionMatrix.mData[ 6 ] + 1.0 ) /
                                    m_projectionMatrix.mData[ 5 ];
    const double nBottom = near * ( m_projectionMatrix.mData[ 6 ] - 1.0 ) /
                                    m_projectionMatrix.mData[ 5 ];

    //Get the sides of the far plane.
    const double fLeft =   far * ( m_projectionMatrix.mData[ 2 ] - 1.0 ) /
                                   m_projectionMatrix.mData[ 0 ];
    const double fRight =  far * ( m_projectionMatrix.mData[ 2 ] + 1.0 ) /
                                   m_projectionMatrix.mData[ 0 ];
    const double fTop =    far * ( m_projectionMatrix.mData[ 6 ] + 1.0 ) /
                                   m_projectionMatrix.mData[ 5 ];
    const double fBottom = far * ( m_projectionMatrix.mData[ 6 ] - 1.0 ) /
                                   m_projectionMatrix.mData[ 5 ];

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
    color->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    geometry->setColorArray( color.get() );
    geometry->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINES, 8, idxLines ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    geometry->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->addDrawable( geometry.get() );

    geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    m_dcs->addChild( geode.get() );

    //Create parent MatrixTransform to transform the view volume by the inverse ModelView matrix 
    //osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform();
    //mt->setMatrix( osg::Matrixd::inverse( mv ) );
    //mt->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////