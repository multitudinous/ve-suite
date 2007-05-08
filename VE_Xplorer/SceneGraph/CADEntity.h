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
#endif

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS CADEntity
{
public:
   ///Base Constructor
   ///\param geomFile
   ///\param worldDCS
   ///\param isStream
	CADEntity( std::string geomFile, VE_SceneGraph::DCS* worldDCS, bool isStream = false );

   ///Constructor that takes a CADEntityHelper and deep copies the osg node contained in the CADEntityHelper
   ///\param nodeToCopy
   ///\param worldDCS
	CADEntity( VE_SceneGraph::CADEntityHelper* nodeToCopy, VE_SceneGraph::DCS* worldDCS );

   ///Destructor
   ~CADEntity();

   ///This will initialize physics for CADEntity
   ///Unless this is called, physics will not work
   void InitPhysics( void );

   ///Returns the DCS of CADEntity
	VE_SceneGraph::DCS* GetDCS( void );

   ///Returns the node of CADEntity
   VE_SceneGraph::CADEntityHelper* GetNode( void );

   ///Returns the rigid body of CADEntity
   VE_SceneGraph::PhysicsRigidBody* GetRigidBody( void );

   ///Returns the filename of CADEntity
   std::string GetFilename( void );

   ///Returns the transparency state of CADEntity
   bool GetTransparentFlag( void );

   ///
   ///\param
   void SetGeometryFilename( std::string );

   ///
   ///\param
   void SetTransparencyFlag( bool );

private:
	VE_SceneGraph::CADEntityHelper* node;///<
	osg::ref_ptr< VE_SceneGraph::DCS > dcs;///<The DCS of 
   osg::ref_ptr< VE_SceneGraph::PhysicsRigidBody > rigid_body;///<The physics representation of the node read in

   bool _transparencyFlag;///<The current state of transparency

   std::string fileName;///<The name of the geometry file loaded

};
}

#endif //CAD_ENTITY_H
