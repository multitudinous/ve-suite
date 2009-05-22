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
    m_activeManipulator( manipulatorRoot.m_activeManipulator.get() ),
    m_activeDragger( manipulatorRoot.m_activeDragger.get() )
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
    return dynamic_cast< manipulator::Manipulator* >( node );
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
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    //If we want to test for manipulator intersections
    if( lineSegmentIntersector )
    {
        //If no manipulator intersections, return
        if( !TestForIntersections( lineSegmentIntersector ) )
        {
            return false;
        }
    }

    switch( event )
    {
        case manipulator::Event::FOCUS:
        case manipulator::Event::PUSH:
        case manipulator::Event::RELEASE:
        {
            if( m_activeManipulator.valid() )
            {
                manipulator::Dragger* newDragger =
                    m_activeManipulator->Handle( event, m_nodePathItr );
                //Something bad happened
                if( !newDragger )
                {
                    //Debug output - newDragger should always be valid
                    return false;
                }

                //If we are focused on a new valid dragger,
                //reset the color of the previous dragger
                //and set active dragger to the new dragger
                if( newDragger != m_activeDragger )
                {
                    /*
                    if( m_activeDragger.valid() )
                    {
                        m_activeDragger->UseColor(
                            manipulator::ColorTag::DEFAULT );
                    }
                    */

                    m_activeDragger = newDragger;
                }

                return true;
            }

            return false;
        }
        case manipulator::Event::DRAG:
        {
            if( m_activeDragger.valid() )
            {
                if( m_activeDragger->Handle( event, m_nodePathItr ) )
                {
                    return true;
                }
            }

            return false;
        }
        default:
        {
            return false;
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
        if( m_activeManipulator.valid() )
        {
            m_activeManipulator = NULL;
        }

        //Reset the active dragger
        if( m_activeDragger.valid() )
        {
            m_activeDragger->UseColor( manipulator::ColorTag::DEFAULT );
            m_activeDragger = NULL;
        }

        return false;
    }

    m_nodePath = intersections.begin()->nodePath;
    m_nodePathItr = m_nodePath.begin();
    //Increment past this - ManipulatorRoot
    ++m_nodePathItr;
    //Increment past the AutoTransform above Manipulators
    ++m_nodePathItr;

    m_activeManipulator = ConvertNodeToManipulator( *m_nodePathItr );

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
