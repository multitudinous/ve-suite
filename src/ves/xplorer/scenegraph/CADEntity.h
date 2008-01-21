/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
#ifndef CAD_ENTITY_H
#define CAD_ENTITY_H

/*!\file CADEntity.h
*/

/*!\class ves::xplorer::scenegraph::CADEntity
*
*/

/*!\namespace ves::xplorer::scenegraph
*
*/

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

//Should not have to include these here
#include <ves/xplorer/scenegraph/DCS.h>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class DCS;
class CADEntityHelper;
class PhysicsRigidBody;
}
}
}

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/Fog>

namespace osg
{
class Fog;
}
#endif

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace ves
{
namespace xplorer
{
namespace scenegraph
{
class PhysicsSimulator;

class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
    ///Base Constructor
    ///\param geomFile The geometry file to be read in
    ///\param parentDCS The parent DCS that CADEntity is added to
    ///\param isStream Is the file a stream
    ///\param occlude Occlude the node with osgOQ
    ///\param physicsSimulator Sets a pointer to the PhysicsSimulator singleton
    CADEntity( std::string geomFile,
               ves::xplorer::scenegraph::DCS* parentDCS,
               bool isStream = false,
               bool occlude = false,
               PhysicsSimulator* physicsSimulator = NULL );

    ///Constructor that takes an osg::Node*
    ///\param node
    ///\param parentDCS
    ///\param physicsSimulator Sets a pointer to the PhysicsSimulator singleton
    CADEntity( osg::Node* node,
               ves::xplorer::scenegraph::DCS* parentDCS,
               PhysicsSimulator* physicsSimulator = NULL );

    ///Constructor that takes a CADEntityHelper and deep copies
    ///the osg node contained in the CADEntityHelper
    ///\param nodeToCopy The node to copy
    ///\param parentDCS The parent DCS that CADEntity is added to
    ///\param physicsSimulator Sets a pointer to the PhysicsSimulator singleton
    CADEntity( ves::xplorer::scenegraph::CADEntityHelper* nodeToCopy,
               ves::xplorer::scenegraph::DCS* parentDCS,
               PhysicsSimulator* physicsSimulator = NULL );

    ///Destructor
    virtual ~CADEntity();

    ///Initializes physics for CADEntity
    //Unless this is called, physics will not work
    void InitPhysics();

    ///Returns the DCS of CADEntity
    ves::xplorer::scenegraph::DCS* GetDCS();

    ///Returns the node of CADEntity
    ves::xplorer::scenegraph::CADEntityHelper* GetNode();

    ///Returns the physics rigid body of CADEntity
    ves::xplorer::scenegraph::PhysicsRigidBody* GetPhysicsRigidBody();

    ///Returns the filename of CADEntity
    std::string GetFilename();

    ///Returns the transparency state of the node
    bool GetTransparentFlag();

    ///Set the transparency state of the node
    ///\param flag The transparency state
    void SetTransparencyFlag( bool flag );

private:
    ///A helper class to give added functionality to CADEntity
    ves::xplorer::scenegraph::CADEntityHelper* m_cadEntityHelper;
    ///The DCS of CADEntity
    osg::ref_ptr< ves::xplorer::scenegraph::DCS > m_dcs;

    bool m_physicsFlag;///<The current state of physics for CADEntity
    bool m_transparencyFlag;///<The current state of transparency

    std::string m_fileName;///<The name of the geometry file loaded

    PhysicsSimulator* m_physicsSimulator;///<A pointer to the PhysicsSimulator singleton
    PhysicsRigidBody* m_physicsRigidBody;
};
}
}
}

#endif //CAD_ENTITY_H
