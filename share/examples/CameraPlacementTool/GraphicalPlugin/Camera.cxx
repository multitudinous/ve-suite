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
m_parentDCS( parentDCS )
{
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
    //Set the projection matrix for the camera
    osg::Matrixd proj;
    proj.makePerspective( 30.0, 1.0, 1.0, 10.0 );
    m_projectionMatrix.set( proj.ptr() );

    DrawViewFrustum();
}
////////////////////////////////////////////////////////////////////////////////
void Camera::DrawViewFrustum()
{
    //Get near and far from the Projection matrix.
    const double near = m_projectionMatrix[ 3 ][ 2 ] / ( m_projectionMatrix[ 2 ][ 2 ] - 1.0 );
    const double far = m_projectionMatrix[ 3 ][ 2 ] / ( 1.0 + m_projectionMatrix[ 2 ][ 2 ] );

    //Get the sides of the near plane.
    const double nLeft = near * ( m_projectionMatrix[ 2 ][ 0 ] - 1.0 ) / m_projectionMatrix[ 0 ][ 0 ];
    const double nRight = near * ( 1.0 + m_projectionMatrix[ 2 ][ 0 ] ) / m_projectionMatrix[ 0 ][ 0 ];
    const double nTop = near * ( 1.0 + m_projectionMatrix[ 2 ][ 1 ] ) / m_projectionMatrix[ 1 ][ 1 ];
    const double nBottom = near * ( m_projectionMatrix[ 2 ][ 1 ] - 1.0 ) / m_projectionMatrix[ 1 ][ 1 ];

    //Get the sides of the far plane.
    const double fLeft = far * ( m_projectionMatrix[ 2 ][ 0 ] - 1.0 ) / m_projectionMatrix[ 0 ][ 0 ];
    const double fRight = far * ( 1.0 + m_projectionMatrix[ 2 ][ 0 ] ) / m_projectionMatrix[ 0 ][ 0 ];
    const double fTop = far * ( 1.0 + m_projectionMatrix[ 2 ][ 1 ] ) / m_projectionMatrix[ 1 ][ 1 ];
    const double fBottom = far * ( m_projectionMatrix[ 2 ][ 1 ] - 1.0 ) / m_projectionMatrix[ 1 ][ 1 ];

    //Our vertex array needs only 9 vertices:
    //The origin, and the eight corners of the near and far planes.
    osg::ref_ptr< osg::Vec3Array > v = new osg::Vec3Array();
    v->resize( 9 );
    (*v)[ 0 ].set( 0.0, 0.0, 0.0 );
    (*v)[ 1 ].set( nLeft, nBottom, -near );
    (*v)[ 2 ].set( nRight, nBottom, -near );
    (*v)[ 3 ].set( nRight, nTop, -near );
    (*v)[ 4 ].set( nLeft, nTop, -near );
    (*v)[ 5 ].set( fLeft, fBottom, -far );
    (*v)[ 6 ].set( fRight, fBottom, -far );
    (*v)[ 7 ].set( fRight, fTop, -far );
    (*v)[ 8 ].set( fLeft, fTop, -far );

    osg::ref_ptr< osg::Geometry > geom = new osg::Geometry();
    geom->setVertexArray( v.get() );

    osg::ref_ptr< osg::Vec4Array > c = new osg::Vec4Array();
    c->push_back( osg::Vec4( 1.0, 1.0, 1.0, 1.0 ) );
    geom->setColorArray( c.get() );
    geom->setColorBinding( osg::Geometry::BIND_OVERALL );

    GLushort idxLines[ 8 ]  = { 0, 5, 0, 6, 0, 7, 0, 8 };
    GLushort idxLoops0[ 4 ] = { 1, 2, 3, 4 };
    GLushort idxLoops1[ 4 ] = { 5, 6, 7, 8 };
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINES, 8, idxLines ) );
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops0 ) );
    geom->addPrimitiveSet( new osg::DrawElementsUShort( osg::PrimitiveSet::LINE_LOOP, 4, idxLoops1 ) );

    osg::ref_ptr< osg::Geode > geode = new osg::Geode();
    geode->addDrawable( geom.get() );

    geode->getOrCreateStateSet()->setMode( GL_LIGHTING, osg::StateAttribute::OFF | osg::StateAttribute::PROTECTED );

    m_parentDCS->addChild( geode.get() );

    //Create parent MatrixTransform to transform the view volume by the inverse ModelView matrix 
    //osg::ref_ptr< osg::MatrixTransform > mt = new osg::MatrixTransform();
    //mt->setMatrix( osg::Matrixd::inverse( mv ) );
    //mt->addChild( geode.get() );
}
////////////////////////////////////////////////////////////////////////////////