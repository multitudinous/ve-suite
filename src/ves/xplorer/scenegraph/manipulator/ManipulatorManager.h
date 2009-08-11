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
 * Id:            $Id: ManipulatorManager.h 12684 2009-05-13 21:17:12Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

// --- OSG Includes --- //
#include <osg/Camera>

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
namespace manipulator
{
class Manipulator;
class TransformManipulator;
class Dragger;

/*!\file ManipulatorManager.h
 * ManipulatorManager API
 */

/*!\class ves::xplorer::scenegraph::manipulator::ManipulatorManager
 *
 */
class VE_SCENEGRAPH_EXPORTS ManipulatorManager : public osg::Camera
{
public:
    ///Constructor
    ManipulatorManager();

    ///Copy constructor using CopyOp to manage deep vs shallow copy
    ManipulatorManager(
        const ManipulatorManager& manipulatorManager,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( ves::xplorer::scenegraph::manipulator, ManipulatorManager );

    ///Override the addChild function to only accept Manipulators
    virtual bool addChild( Manipulator* child );

    ///Override the computeBound function to return an empty bounding sphere
    virtual osg::BoundingSphere computeBound() const;

    ///Can't override the getChild function, so create our own
    Manipulator* GetChild( unsigned int i );

    ///
    ///\return
    TransformManipulator* const GetSceneManipulator() const;

    ///
    virtual bool Handle(
        Event::Enum event,
        osgUtil::LineSegmentIntersector* testForIntersections = NULL );

    ///Override the insertChild function to only accept Manipulators
    virtual bool insertChild( unsigned int index, Manipulator* child );

    ///
    const bool IsEnabled() const;

    ///Override the replaceChild function to only accept Manipulators
    virtual bool replaceChild( Manipulator* origChild, Manipulator* newChild );

    ///Override the setChild function to only accept Manipulators
    virtual bool setChild( unsigned int i, Manipulator* node );

    ///Deactivate the manipulator manager
    void TurnOff();

    ///Activate the manipulator manager
    void TurnOn();

protected:
    ///Destructor
    virtual ~ManipulatorManager();

private:
    ///
    Manipulator* ConvertNodeToManipulator( osg::Node* node );

    ///
    bool TestForIntersections(
        osgUtil::LineSegmentIntersector* lineSegmentIntersector );

    ///
    bool m_enabled;

    ///
    unsigned int m_nodeMask;

    ///
    osg::NodePath m_nodePath;

    ///
    osg::NodePath::iterator m_nodePathItr;

    ///
    Manipulator* m_activeManipulator;

    ///
    Dragger* m_activeDragger;

    ///
    osgUtil::LineSegmentIntersector* m_deviceInput;

    ///
    ///Not sure if this guy should live here, but will work for now
    osg::ref_ptr< TransformManipulator > m_sceneManipulator;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H
