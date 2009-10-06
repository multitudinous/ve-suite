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

#ifndef VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H
#define VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/manipulator/Definitions.h>

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
namespace manipulator
{
class Dragger;
class RotateTwist;
class TransformManipulator;

/*!\file ManipulatorManager.h
 * ManipulatorManager API
 */

/*!\class ves::xplorer::scenegraph::manipulator::ManipulatorManager
 *
 */
class VE_SCENEGRAPH_EXPORTS ManipulatorManager : public osg::Group
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

    ///Override the addChild function to only accept Draggers
    virtual bool addChild( Dragger* child );

    ///Override the computeBound function to return an empty bounding sphere
    //virtual osg::BoundingSphere computeBound() const;

    ///
    void Enable( const bool& enable = true );

    ///Can't override the getChild function, so create our own
    Dragger* GetChild( unsigned int i );

    ///
    ///\return
    RotateTwist* const GetTwistManipulator() const;

    ///
    ///\return
    TransformManipulator* const GetSceneManipulator() const;

    ///
    virtual bool Handle(
        Event::Enum event,
        osgUtil::LineSegmentIntersector* lineSegmentIntersector = NULL );

    ///Override the insertChild function to only accept Manipulators
    virtual bool insertChild( unsigned int index, Dragger* child );

    ///
    const bool IsEnabled() const;

    ///Override the replaceChild function to only accept Manipulators
    virtual bool replaceChild( Dragger* origChild, Dragger* newChild );

    ///Override the setChild function to only accept Manipulators
    virtual bool setChild( unsigned int i, Dragger* node );

    ///
    void SetDraggerScale( double draggerScale );
    
protected:
    ///Destructor
    virtual ~ManipulatorManager();

private:
    ///
    Dragger* ConvertNodeToDragger( osg::Node* node );

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
    Dragger* m_rootDragger;

    ///
    Dragger* m_leafDragger;

    ///
    osgUtil::LineSegmentIntersector* m_deviceInput;

    ///
    osg::ref_ptr< RotateTwist > m_rotateTwist;

    ///
    osg::ref_ptr< TransformManipulator > m_sceneManipulator;
    
    ///Scale the dragger size
    double m_draggerSize;

public:
    //friend class Dragger;

};
} //end manipulator
} //end scenegraph
} //end xplorer
} //end ves

#endif //VES_XPLORER_SCENEGRAPH_MANIPULATOR_MANIPULATOR_MANAGER_H
