/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CAD_ENTITY_H
#define CAD_ENTITY_H

/*!\file CADEntity.h
*/

/*!\class VE_SceneGraph::CADEntity
* 
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

//Should not have to include these here
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/PhysicsRigidBody.h"

namespace VE_SceneGraph
{
class DCS;
class CADEntityHelper;
class PhysicsRigidBody;
}

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/Fog>

namespace osg
{
class Fog;
}
#endif //_OSG

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
    ///Base Constructor
    ///\param geomFile The geometry file to be read in
    ///\param parentDCS The parent DCS that CADEntity is added to
    ///\param isStream Is the file a stream
    CADEntity( std::string geomFile, VE_SceneGraph::DCS* parentDCS, bool isStream = false );

    ///Constructor that takes a CADEntityHelper and deep copies the osg node contained in the CADEntityHelper
    ///\param nodeToCopy The node to copy
    ///\param parentDCS The parent DCS that CADEntity is added to
    CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, VE_SceneGraph::DCS* parentDCS );

    ///Destructor
    ~CADEntity();

    ///Initializes physics for CADEntity
    //Unless this is called, physics will not work
    void InitPhysics();

    ///Returns the DCS of CADEntity
    VE_SceneGraph::DCS* GetDCS();

    ///Returns the node of CADEntity
    VE_SceneGraph::CADEntityHelper* GetNode();

    ///Returns the physics rigid body of CADEntity
    VE_SceneGraph::PhysicsRigidBody* GetPhysicsRigidBody();

    ///Returns the filename of CADEntity
    std::string GetFilename();

    ///Returns the transparency state of the node
    bool GetTransparentFlag();

    ///Set the transparency state of the node
    ///\param flag The transparency state
    void SetTransparencyFlag( bool flag );

private:
    VE_SceneGraph::CADEntityHelper* m_cadEntityHelper;///<A helper class to give added functionality to CADEntity
    osg::ref_ptr< VE_SceneGraph::DCS > m_dcs;///<The DCS of CADEntity
    osg::ref_ptr< VE_SceneGraph::PhysicsRigidBody > m_physicsRigidBody;///<The physics rigid body representation of CADEntity

    bool m_transparencyFlag;///<The current state of transparency

    std::string m_fileName;///<The name of the geometry file loaded

};
}

#endif //CAD_ENTITY_H
