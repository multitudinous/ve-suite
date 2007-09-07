/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
#ifndef SCENE_MANAGER_H
#define SCENE_MANAGER_H

/*!\file SceneManager.h
*/

/*!\class VE_SceneGraph::SceneManager
*
*/

/*!\namespace VE_SceneGraph
*
*/

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"
#include "VE_Xplorer/SceneGraph/Group.h"
#include "VE_Xplorer/SceneGraph/Switch.h"

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#include <osg/ClearNode>
#endif

#include <osgOQ/OcclusionQueryRoot.h>
#include <osgOQ/OcclusionQueryContext.h>

// --- VR Juggler Includes --- //
#include <vpr/Util/Singleton.h>

namespace VE_SceneGraph
{
   class DCS;
   class Group;
   class Switch;
   class CADEntity;
}

namespace VE_SceneGraph
{
class VE_SCENEGRAPH_EXPORTS SceneManager //: public vpr::Singleton< SceneManager >
{
public:
   ///???
   ///\param param
   void Initialize( std::string param );

   ///Acts as the destructor
   //void CleanUp();

   ///Initialize the scene
   void InitScene();

   ///Return the root node of the scenegraph
   VE_SceneGraph::Group* GetRootNode();

   ///Return the world DCS of the scenegraph
   VE_SceneGraph::DCS* GetWorldDCS();

   ///Return the network DCS of the scenegraph
   VE_SceneGraph::DCS* GetNetworkDCS();

   ///Return the active switch node of the scenegraph
   VE_SceneGraph::DCS* GetActiveSwitchNode();
   ///Return the active switch node of the scenegraph
   osgOQ::OcclusionQueryContext* GetOcclusionQueryContext();
   ///Set the node on the switch node that is active
   ///\param activeNode The node to activate
   void SetActiveSwitchNode( int activeNode );

   ///Switch the logo on and off
   ///\param trueFalse Turn the logo on and off
   void ViewLogo( bool trueFalse );
   ///PreFrameUpdate call to sync DCS information across cluster
   void PreFrameUpdate();
   ///Set the background color
   ///\param color the color to set the background color
   void SetBackgroundColor( std::vector< double > color );
   ///Reset the osgOQC node accessor
   ///Should be used when new ves files are loaded
   void ResetOcclusionQueryContext();
       
private:
   //Required so that vpr::Singleton can instantiate this class
   //Friend class vpr::Singleton< SceneManager >;
   //SceneManager(const SceneManager& o){;}
   //SceneManager& operator=(const SceneManager& o){;}

   ///Base Constructor
   SceneManager();

   ///Destructor
   ///Never gets called, don't implement
   ~SceneManager();

   vprSingletonHeader( SceneManager );

   std::string _param;///<
   osg::ref_ptr< VE_SceneGraph::Group > rootNode;///<The root node of our scenegraph
   osg::ref_ptr< VE_SceneGraph::DCS > _logoNode;///<The node which contains our logo
   osg::ref_ptr< VE_SceneGraph::Switch > _logoSwitch;///<Node to switch between the logo and the worldDCS
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;///<Node to control navigation
   osg::ref_ptr< VE_SceneGraph::DCS > networkDCS;///<Node to hold a network view of the system under investigation
   
   //The logo
   VE_SceneGraph::CADEntity* m_blueArrow;
   VE_SceneGraph::CADEntity* m_greyArrow;
   VE_SceneGraph::CADEntity* m_orangeArrow;
   VE_SceneGraph::CADEntity* m_veText;///<Logo VE text
   VE_SceneGraph::CADEntity* m_suiteText;///<Logo SUITE text

    ///Clear node to control the background color
    osg::ref_ptr< osg::ClearNode > m_clrNode;
    osg::ref_ptr<osgOQ::OcclusionQueryContext> m_oqc;

protected:
   ///Create the model for the logo
   #ifdef _OSG
      void _createLogo();
   #endif

};
}

#endif //SCENE_MANAGER_H
