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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_PFSCENEMANAGEMENT_H
#define CFD_PFSCENEMANAGEMENT_H
/*!\file cfdPfSceneManagement.h
cfdPfSceneManagement API
*/

/*!\class VE_SceneGraph::cfdPfSceneManagement
*
*/
#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Switch.h"

#ifdef _PERFORMER
class pfLightModel;
class pfLightSource;
#elif _OSG
#include <osg/ref_ptr>
#endif

namespace VE_SceneGraph
{
	class DCS;
   class Group;
   class Switch;
	class CADEntity;
}

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS cfdPfSceneManagement //: public vpr::Singleton< cfdPfSceneManagement >
{
public:
   void Initialize( std::string );
   void CleanUp( void );
   void InitScene( void );

	VE_SceneGraph::Group* GetRootNode( void );
	VE_SceneGraph::DCS* GetWorldDCS( void );
	VE_SceneGraph::DCS* GetNetworkDCS( void );
	VE_SceneGraph::DCS* GetActiveSwitchNode( void );

   ///Set the node on the switch node that is active
   ///\param activeNode node to activate
   void SetActiveSwitchNode( int activeNode );

   ///Switch the logo on and off
   ///\param trueFalse Turn the logo on and off.
   void ViewLogo(bool trueFalse);

   ///PreFrameUpdate call to sync dcs information across cluster
   void PreFrameUpdate( void );

private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdPfSceneManagement >;
   //cfdPfSceneManagement(const cfdPfSceneManagement& o) { ; }
   //cfdPfSceneManagement& operator=(const cfdPfSceneManagement& o) { ; }
   cfdPfSceneManagement( void );
   ~cfdPfSceneManagement(){ ; } // Never gets called, don't implement
   vprSingletonHeader( cfdPfSceneManagement );

   //std::string    param;
   std::string _param;
   osg::ref_ptr< VE_SceneGraph::Group > rootNode;
   osg::ref_ptr< VE_SceneGraph::DCS > _logoNode;
   osg::ref_ptr< VE_SceneGraph::Switch > _logoSwitch;///<Node to switch between the logo and the worldDCS
	osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;///<Node to control navigation
   osg::ref_ptr< VE_SceneGraph::DCS > networkDCS;///<Node to hold a network view of the system under investigation
   VE_SceneGraph::CADEntity* _movingPyramidsAssembly;///<Logo Animated pyramids
   VE_SceneGraph::CADEntity* _textPart;///<Logo Text

   #ifdef _PERFORMER
      pfLightModel*  sunModel;
      pfLightSource* sun;
      pfLightSource* lit;
   #endif

protected:
   ///create the model for the logo.
   #ifdef _OSG
      void _createLogo();
   #endif
};
}
#endif
