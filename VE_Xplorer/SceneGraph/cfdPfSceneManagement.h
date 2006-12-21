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

namespace VE_SceneGraph
{
   class cfdDCS;
   class cfdGroup;
   class cfdSwitch;
   class cfdFILE;
}
#ifdef _PERFORMER
class pfLightModel;
class pfLightSource;
#elif _OSG
#endif

#include <vpr/Util/Singleton.h>
#include "VE_Installer/include/VEConfig.h"

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS cfdPfSceneManagement //: public vpr::Singleton< cfdPfSceneManagement >
{
public:
   void Initialize( std::string );
   void CleanUp( void );
   void InitScene( void );

   cfdGroup* GetRootNode( void );
   cfdDCS*   GetWorldDCS( void );
   cfdDCS*   GetNetworkDCS( void );
   ///Set the node on the switch node that is active
   ///\param activeNode node to activate
   void SetActiveSwitchNode( int activeNode );

   ///Switch the logo on and off
   ///\param trueFalse Turn the logo on and off.
   void ViewLogo(bool trueFalse);
   void PreFrameUpdate();

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
   cfdGroup* rootNode;
   cfdDCS* _logoNode;
   cfdSwitch* _logoSwitch;///<Node to switch between the logo and the worldDCS
   cfdDCS* worldDCS;///<Node to control navigation
   cfdDCS* networkDCS;///<Node to hold a network view of the system under investigation
   VE_SceneGraph::cfdFILE* _movingPyramidsAssembly;///<Logo Animated pyramids
   VE_SceneGraph::cfdFILE* _textPart;///<Logo Text
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
