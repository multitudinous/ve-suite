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

#include <ves/xplorer/Debug.h>

#include <ves/xplorer/scenegraph/manipulator/Manipulator.h>

// --- OSG Includes --- //
#include <osgUtil/LineSegmentIntersector>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot()
    :
    osg::Group(),
    m_activeManipulator( NULL )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::ManipulatorRoot(
    const ManipulatorRoot& manipulatorRoot, const osg::CopyOp& copyop )
    :
    osg::Group( manipulatorRoot, copyop ),
    m_nodePath( manipulatorRoot.m_nodePath ),
    m_activeManipulator( manipulatorRoot.m_activeManipulator.get() )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
ManipulatorRoot::~ManipulatorRoot()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::addChild( Manipulator* child )
{
    return osg::Group::addChild( child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
Manipulator* ManipulatorRoot::ConvertNodeToManipulator(
    osg::Node* node )
{
    return dynamic_cast< Manipulator* >(
        node->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
Manipulator* ManipulatorRoot::GetChild( unsigned int i )
{
    return ConvertNodeToManipulator( osg::Group::getChild( i ) );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::Handle(
    Event::Enum event,
    osgUtil::LineSegmentIntersector* lineSegmentIntersector )
{
    if( lineSegmentIntersector )
    {
        osgUtil::IntersectionVisitor intersectionVisitor(
            lineSegmentIntersector );

        accept( intersectionVisitor );

        osgUtil::LineSegmentIntersector::Intersections& intersections =
            lineSegmentIntersector->getIntersections();
        if( intersections.empty() )
        {
            vprDEBUG( vesDBG, 2 )
                << "|\tManipulatorRoot::Handle - No manipulator hit"
                << std::endl << vprDEBUG_FLUSH;

            return false;
        }

        m_nodePath = intersections.begin()->nodePath;
        m_activeManipulator = ConvertNodeToManipulator( m_nodePath.front() );
    }

    switch( event )
    {
        case Event::FOCUS:
        {
            if( m_activeManipulator.valid() )
            {
                m_activeManipulator->Handle( event );
            }

            break;
        }
        case Event::DRAG:
        case Event::RELEASE:
        {
            if( m_activeManipulator.valid() )
            {
                m_activeManipulator->Handle( event );
            }

            break;
        }
        default:
        {
            break;
        }
    }

    /*
    for( size_t i = 0; i < getNumChildren(); ++i )
    {
        Manipulator* manipulator = GetChild( i );
        if( manipulator->Handle( event ) )
        {
            //return true;
        }
    }
    */

    return false;
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::insertChild(
    unsigned int index, Manipulator* child )
{
    return osg::Group::insertChild( index, child->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::replaceChild(
    Manipulator* origChild,
    Manipulator* newChild )
{
    return osg::Group::replaceChild(
        origChild->getParents().front(), newChild->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
bool ManipulatorRoot::setChild( unsigned int i, Manipulator* node )
{
    return osg::Group::setChild( i, node->getParents().front() );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorRoot::TurnOn()
{
    setNodeMask( 1 );
}
////////////////////////////////////////////////////////////////////////////////
void ManipulatorRoot::TurnOff()
{
    setNodeMask( 0 );
}
////////////////////////////////////////////////////////////////////////////////
