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
 * Date modified: $Date: 2009-05-13 15:17:12 -0600 (Wed, 13 May 2009) $
 * Version:       $Rev: 12684 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: ManipulatorManager.cxx 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/manipulator/ManipulatorManager.h>
#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>
#include <ves/xplorer/scenegraph/manipulator/TransformManipulator.h>
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>

// --- OSG Includes --- //
#include <osg/AutoTransform>

#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph::manipulator;

////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::ManipulatorManager()
    :
    osg::Camera(),
    m_enabled( false ),
    //NodeMask is an unsigned int
    m_nodeMask( 0x1 ),
    m_activeManipulator( NULL ),
    m_activeDragger( NULL ),
    m_sceneManipulator( NULL )
{
    setClearMask( GL_DEPTH_BUFFER_BIT );
    setRenderOrder( osg::Camera::POST_RENDER );
    setReferenceFrame( osg::Transform::RELATIVE_RF );
    TurnOff();

    m_sceneManipulator = new TransformManipulator();
    //Turn off the scene manipulator until requested by user
    m_sceneManipulator->TurnOff();
    addChild( m_sceneManipulator.get() );
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::ManipulatorManager(
    const ManipulatorManager& manipulatorManager, const osg::CopyOp& copyop )
    :
    osg::Camera( manipulatorManager, copyop ),
    m_enabled( manipulatorManager.m_enabled ),
    m_nodeMask( manipulatorManager.m_nodeMask ),
    m_nodePath( manipulatorManager.m_nodePath ),
    m_nodePathItr( manipulatorManager.m_nodePathItr ),
    m_activeManipulator( manipulatorManager.m_activeManipulator ),
    m_activeDragger( manipulatorManager.m_activeDragger )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorManager::~ManipulatorManager()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::addChild( Manipulator* child )
{
    return osg::Group::addChild( child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
Manipulator* ManipulatorManager::ConvertNodeToManipulator( osg::Node* node )
{
    return static_cast< Manipulator* >( node );
}
////////////////////////////////////////////////////////////////////////////////
Manipulator* ManipulatorManager::GetChild( unsigned int i )
{
    osg::AutoTransform* autoTransform =
        static_cast< osg::AutoTransform* >( osg::Group::getChild( i ) );
    return ConvertNodeToManipulator( autoTransform->getChild( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
TransformManipulator* const ManipulatorManager::GetSceneManipulator() const
{
    return m_sceneManipulator.get();
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::Handle(
    Event::Enum event,
    osgUtil::LineSegmentIntersector* testForIntersections )
{
    //If we want to test for manipulator intersections
    if( testForIntersections )
    {
        //If no manipulator intersections, return
        if( !TestForIntersections( testForIntersections ) )
        {
            return false;
        }
    }

    //Make sure the active manipulator is valid before we continue
    if( !m_activeManipulator )
    {
        return false;
    }

    switch( event )
    {
        case Event::FOCUS:
        {
            m_activeDragger =
                m_activeManipulator->Focus( m_nodePathItr );

            return m_activeDragger;
        }
        case Event::PUSH:
        {
            m_activeDragger =
                m_activeManipulator->Push(
                    *m_deviceInput, m_nodePath, m_nodePathItr );

            return m_activeDragger;
        }
        case Event::DRAG:
        {
            if( m_activeDragger )
            {
                return m_activeDragger->Drag( *m_deviceInput );
            }

            return false;
        }
        case Event::RELEASE:
        {
            m_activeDragger = NULL;

            return m_activeManipulator->Release( m_nodePathItr );
        }
        default:
        {
            m_activeDragger = NULL;

            return false;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::insertChild( unsigned int index, Manipulator* child )
{
    return osg::Group::insertChild( index, child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
const bool ManipulatorManager::IsEnabled() const
{
    return m_enabled;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::replaceChild(
    Manipulator* origChild, Manipulator* newChild )
{
    return osg::Group::replaceChild(
        origChild->getParents().front(), newChild->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::setChild( unsigned int i, Manipulator* node )
{
    return osg::Group::setChild( i, node->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorManager::TestForIntersections(
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    osgUtil::IntersectionVisitor intersectionVisitor( lineSegmentIntersector );
    accept( intersectionVisitor );

    osgUtil::LineSegmentIntersector::Intersections& intersections =
        lineSegmentIntersector->getIntersections();
    if( intersections.empty() )
    {
        //Reset the active manipulator
        if( m_activeManipulator )
        {
            m_activeManipulator = NULL;
        }

        //Reset the active dragger
        if( m_activeDragger )
        {
            m_activeDragger->UseColor( ColorTag::DEFAULT );
            m_activeDragger = NULL;
        }

        return false;
    }

    //Get the full node path from selected dragger to this
    m_nodePath = intersections.begin()->nodePath;

    //Increment past this and the AutoTransform above a Manipulator
    m_nodePathItr = m_nodePath.begin();
    ++m_nodePathItr;
    ++m_nodePathItr;

    m_activeManipulator = ConvertNodeToManipulator( *m_nodePathItr );
    m_deviceInput = lineSegmentIntersector;

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::TurnOff()
{
    m_enabled = false;

    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorManager::TurnOn()
{
    m_enabled = true;

    setNodeMask( m_nodeMask );
}
////////////////////////////////////////////////////////////////////////////////
