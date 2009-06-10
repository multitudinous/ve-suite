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
 * Id:            $Id: ManipulatorRoot.cxx 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

// --- VE-Suite Includes --- //
#include <ves/xplorer/scenegraph/ManipulatorRoot.h>
#include <ves/xplorer/scenegraph/SceneManager.h>

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>
#include <ves/xplorer/scenegraph/manipulator/Dragger.h>

// --- OSG Includes --- //
#include <osg/AutoTransform>

#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot()
    :
    osg::Group(),
    m_activeManipulator( NULL ),
    m_activeDragger( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot(
    const ManipulatorRoot& manipulatorRoot, const osg::CopyOp& copyop )
    :
    osg::Group( manipulatorRoot, copyop ),
    m_nodePath( manipulatorRoot.m_nodePath ),
    m_nodePathItr( manipulatorRoot.m_nodePathItr ),
    m_activeManipulator( manipulatorRoot.m_activeManipulator ),
    m_activeDragger( manipulatorRoot.m_activeDragger )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::~ManipulatorRoot()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::addChild( manipulator::Manipulator* child )
{
    return osg::Group::addChild( child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
manipulator::Manipulator* ManipulatorRoot::ConvertNodeToManipulator(
    osg::Node* node )
{
    return static_cast< manipulator::Manipulator* >( node );
}
////////////////////////////////////////////////////////////////////////////////
manipulator::Manipulator* ManipulatorRoot::GetChild( unsigned int i )
{
    osg::AutoTransform* autoTransform =
        dynamic_cast< osg::AutoTransform* >( osg::Group::getChild( i ) );
    return ConvertNodeToManipulator( autoTransform->getChild( 0 ) );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::Handle(
    manipulator::Event::Enum event,
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
        case manipulator::Event::FOCUS:
        {
            m_activeDragger =
                m_activeManipulator->Focus( m_nodePathItr );

            return( m_activeDragger );
        }
        case manipulator::Event::PUSH:
        {
            m_activeDragger =
                m_activeManipulator->Push(
                    *m_deviceInput, m_nodePath, m_nodePathItr );

            return( m_activeDragger );
        }
        case manipulator::Event::DRAG:
        {
            if( m_activeDragger )
            {
                return( m_activeDragger->Drag( *m_deviceInput ) );
            }

            return false;
        }
        case manipulator::Event::RELEASE:
        {
            m_activeDragger = NULL;

            return( m_activeManipulator->Release( m_nodePathItr ) );
        }
        default:
        {
            m_activeDragger = NULL;

            return( m_activeDragger );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::insertChild(
    unsigned int index, manipulator::Manipulator* child )
{
    return osg::Group::insertChild( index, child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::replaceChild(
    manipulator::Manipulator* origChild, manipulator::Manipulator* newChild )
{
    return osg::Group::replaceChild(
        origChild->getParents().front(), newChild->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::setChild( unsigned int i, manipulator::Manipulator* node )
{
    return osg::Group::setChild( i, node->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::TestForIntersections(
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    osgUtil::IntersectionVisitor intersectionVisitor(
        lineSegmentIntersector );

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
            m_activeDragger->UseColor( manipulator::ColorTag::DEFAULT );
            m_activeDragger = NULL;
        }

        return false;
    }

    //Get the full node path
    m_nodePath = intersections.begin()->nodePath;
    //Remove this ManipulatorRoot and the AutoTransform above the manipulators
    //m_nodePath = osg::NodePath( m_nodePath.begin() + 2, m_nodePath.end() );
    m_nodePathItr = m_nodePath.begin();
    //Increment past this - ManipulatorRoot
    ++m_nodePathItr;
    //Increment past the AutoTransform above Manipulators
    ++m_nodePathItr;

    m_activeManipulator = ConvertNodeToManipulator( *m_nodePathItr );
    
    m_deviceInput = lineSegmentIntersector;

    return true;
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorRoot::TurnOff()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorRoot::TurnOn()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
