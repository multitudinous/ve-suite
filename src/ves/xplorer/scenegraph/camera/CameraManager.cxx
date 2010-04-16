/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/camera/CameraManager.h>
#include <ves/xplorer/scenegraph/camera/CameraObject.h>

#include <ves/xplorer/scenegraph/Masks.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osgUtil/IntersectionVisitor>
#include <osgUtil/LineSegmentIntersector>

#include <osg/Geode>
#include <osg/Geometry>

using namespace ves::xplorer::scenegraph::camera;

////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager()
    :
    osg::Group(),
    m_enabled( false ),
    m_activeCamera( NULL )
{
    Enable();
    m_rttQuad = CreateMasterCameraQuad();
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::CameraManager(
    const CameraManager& cameraManager, const osg::CopyOp& copyop )
    :
    osg::Group( cameraManager, copyop ),
    m_enabled( cameraManager.m_enabled ),
    m_activeCamera( cameraManager.m_activeCamera )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraManager::~CameraManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::addChild( CameraObject* child )
{
    return osg::Group::addChild( child );
}
////////////////////////////////////////////////////////////////////////////////
/*
osg::BoundingSphere CameraManager::computeBound() const
{
    osg::BoundingSphere bsphere;

    return bsphere;
}
*/
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::ConvertNodeToCameraObject(
    osg::Node* const node )
{
    return dynamic_cast< CameraObject* >( node );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::Enable( const bool& enable )
{
    m_enabled = enable;

    if( m_enabled )
    {
        setNodeMask( NodeMask::CAMERA_MANAGER );
    }
    else
    {
        setNodeMask( NodeMask::NONE );
    }
}
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::GetActiveCameraObject() const
{
    return m_activeCamera;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::Handle(
    Event::Enum event,
    osgUtil::LineSegmentIntersector& deviceInput )
{
    CameraObject* cameraObject = TestForIntersections( deviceInput );

    switch( event )
    {
    case Event::FOCUS:
    {
        if( cameraObject )
        {
            //camera->DoSomething();
            std::cout<< "Just focused on a camera in the scene!!!" << std::endl;
        }

        break;
    }
    case Event::RELEASE:
    {
        SetActiveCameraObject( cameraObject );

        break;
    }
    default:
    {
        m_activeCamera = NULL;

        break;
    }
    } //end switch( event )

    return cameraObject;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::insertChild( unsigned int index, CameraObject* child )
{
    return osg::Group::insertChild( index, child );
}
////////////////////////////////////////////////////////////////////////////////
const bool CameraManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::replaceChild( CameraObject* origChild, CameraObject* newChild )
{
    return osg::Group::replaceChild( origChild, newChild );
}
////////////////////////////////////////////////////////////////////////////////
void CameraManager::SetActiveCameraObject( CameraObject* cameraObject )
{
    if( cameraObject == m_activeCamera )
    {
        return;
    }

    m_activeCamera = cameraObject;

    if( m_activeCamera )
    {
        //Update UI

        //Set selected object

        //Connect Dragger
    }
}
////////////////////////////////////////////////////////////////////////////////
bool CameraManager::setChild( unsigned int i, CameraObject* node )
{
    return osg::Group::setChild( i, node );
}
////////////////////////////////////////////////////////////////////////////////
CameraObject* const CameraManager::TestForIntersections(
    osgUtil::LineSegmentIntersector& deviceInput )
{
    osgUtil::IntersectionVisitor intersectionVisitor( &deviceInput );

    //Use bitwise NOT operator to get opposite of osg::Cameras NodeMask
    //Need to fix this to a more stable implementation
    unsigned int traversalMask = ~0xffffffdf;
    intersectionVisitor.setTraversalMask( traversalMask );

    accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        deviceInput.getIntersections();
    if( intersections.empty() )
    {
        return NULL;
    }

    osg::NodePath nodePath = intersections.begin()->nodePath;
    osg::NodePath::iterator nodePathItr = nodePath.begin();
    ++nodePathItr;

    return ConvertNodeToCameraObject( *nodePathItr );
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* CameraManager::CreateMasterCameraQuad()
{
    ///These are in pixel space
    osg::ref_ptr< osg::Vec3Array > cameraViewQuadVertices = 
        new osg::Vec3Array();
    cameraViewQuadVertices->resize( 4 );
    (*cameraViewQuadVertices)[ 0 ].set( 0.0, 0.0, -1.0 );
    (*cameraViewQuadVertices)[ 1 ].set( 100.0, 0.0, -1.0 );
    (*cameraViewQuadVertices)[ 2 ].set( 100.0, 100.0, -1.0 );
    (*cameraViewQuadVertices)[ 3 ].set( 0.0, 100.0, -1.0 );

    //Get the texture coordinates for the quad
    osg::ref_ptr< osg::Vec2Array > quadTexCoords = new osg::Vec2Array();
    quadTexCoords->resize( 4 );
    (*quadTexCoords)[ 0 ].set( 0.0, 0.0 );
    (*quadTexCoords)[ 1 ].set( 1.0, 0.0 );
    (*quadTexCoords)[ 2 ].set( 1.0, 1.0 );
    (*quadTexCoords)[ 3 ].set( 0.0, 1.0 );

    //Create the quad geometry
    osg::ref_ptr< osg::Geometry > quadGeometry = new osg::Geometry();
    quadGeometry->setVertexArray( cameraViewQuadVertices.get() );
    quadGeometry->addPrimitiveSet( new osg::DrawArrays( 
        osg::PrimitiveSet::QUADS, 0, cameraViewQuadVertices->size() ) );
    quadGeometry->setTexCoordArray( 0, quadTexCoords.get() );
    quadGeometry->setUseDisplayList( true );
    //quadGeometry->setColorBinding( osg::Geometry::BIND_OFF );
    osg::ref_ptr< osg::Vec4Array > c = new osg::Vec4Array();
    c->push_back( osg::Vec4( 1.0, 1.0, 0., 1. ) );
    quadGeometry->setColorArray( c.get() );
    quadGeometry->setColorBinding( osg::Geometry::BIND_OVERALL );
    quadGeometry->dirtyDisplayList();
    quadGeometry->dirtyBound();

    //Set the stateset for the quad
    osg::ref_ptr< osg::StateSet > stateset =
        quadGeometry->getOrCreateStateSet();
    stateset->setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    osg::Geode* quadGeode = new osg::Geode();
    quadGeode->setCullingActive( false );
    quadGeode->addDrawable( quadGeometry.get() );
    quadGeode->getOrCreateStateSet()->
        setMode( GL_LIGHTING, osg::StateAttribute::OFF );

    return quadGeode;
}
////////////////////////////////////////////////////////////////////////////////
osg::Geode* CameraManager::GetCameraManagerQuad()
{
    return m_rttQuad.get();
}
////////////////////////////////////////////////////////////////////////////////
