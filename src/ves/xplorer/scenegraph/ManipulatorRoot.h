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
 * Id:            $Id: ManipulatorRoot.h 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef MANIPULATOR_ROOT_H
#define MANIPULATOR_ROOT_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Enums.h>

// --- OSG Includes --- //
#include <osg/Group>

namespace osgUtil
{
class LineSegmentIntersector;
}

// --- C/C++ Includes --- //

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class Manipulator;

/*!\file ManipulatorRoot.h
 * ManipulatorRoot API
 */

/*!\class ves::xplorer::scenegraph::ManipulatorRoot
 *
 */
class VE_SCENEGRAPH_EXPORTS ManipulatorRoot : public osg::Group
{
public:
    ///Constructor
    ManipulatorRoot();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ManipulatorRoot(
        const ManipulatorRoot& manipulatorRoot,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph, ManipulatorRoot );

    ///Override the addChild function to only accept Manipulators
    virtual bool addChild( Manipulator* child );

    ///
    Manipulator* ConvertNodeToManipulator( osg::Node* node );

    ///Can't override the getChild function, so create our own
    Manipulator* GetChild( unsigned int i );

    ///
    virtual bool Handle(
        Event::Enum event,
        osgUtil::LineSegmentIntersector* lineSegmentIntersector = NULL );

    ///Override the insertChild function to only accept Manipulators
    virtual bool insertChild(
        unsigned int index, Manipulator* child );

    ///Override the replaceChild function to only accept Manipulators
    virtual bool replaceChild(
        Manipulator* origChild,
        Manipulator* newChild );

    ///Override the setChild function to only accept Manipulators
    virtual bool setChild( unsigned int i, Manipulator* node );

    ///Activate the manipulator root
    void TurnOn();

    ///Deactivate the manipulator root
    void TurnOff();

protected:
    ///Destructor
    virtual ~ManipulatorRoot();

private:
    ///
    osg::NodePath m_nodePath;

    ///
    osg::ref_ptr< Manipulator > m_activeManipulator;

    ///
    osg::ref_ptr< osgUtil::LineSegmentIntersector > m_lineSegmentIntersector;

};
} //end scenegraph
} //end xplorer
} //end ves

#endif //MANIPULATOR_ROOT_H
