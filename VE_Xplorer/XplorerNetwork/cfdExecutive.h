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
#ifndef CFD_EXECUTIVE_H
#define CFD_EXECUTIVE_H
/*!\file cfdExecutive.h
cfdExecutive API
*/
/*!\class VE_Xplorer::cfdExecutive
* 
*/

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Installer/include/VEConfig.h"
#include <vpr/Util/Singleton.h>

#include <map>
#include <string>
#include <vector>

namespace VE_SceneGraph
{
   class cfdDCS; 
   class cfdGroup;
}

namespace VE_Xplorer
{
   class cfdGauges;
   class cfdDashboard;
   class cfdInteractiveGeometry;
   class Body_UI_i;
   class cfdCommandArray;
   class cfdVEAvailModules;
   class cfdVEBaseClass;
   class cfdVEAvail_Modules;
   class cfdVjObsWrapper;
   class cfdThread;
}

namespace VE_XML
{
   class XMLObject;
   namespace VE_Model
   {
      class Model;
   }
}

namespace VE_EVENTS
{
   class EventHandler;
}

namespace Body { class Executive; }
namespace CosNaming { class NamingContext; }
namespace PortableServer { class POA; }

namespace VE_Xplorer
{
class VE_XPLORER_NETWORK_EXPORTS cfdExecutive : public cfdGlobalBase//: public vpr::Singleton< cfdModelHandler >
{
private:
   // Required so that vpr::Singleton can instantiate this class.
   //friend class vpr::Singleton< cfdExecutive >;
   //cfdExecutive(const cfdExecutive& o) { ; }
   //cfdExecutive& operator=(const cfdExecutive& o) { ; }
   // this class should be a singleton
   // constructor
   cfdExecutive( void ){;}
   
   // destructor
   virtual ~cfdExecutive( void ){;}
   vprSingletonHeader( cfdExecutive );   
public:
   void Initialize( CosNaming::NamingContext*, PortableServer::POA* );
   void CleanUp( void );
   
   ///the Computational Engine
   Body::Executive* _exec;
   CosNaming::NamingContext* naming_context;

   ///Functions that operate on the Executive
   void GetNetwork( void );
   void GetOutput( std::string name);
   void GetEverything( void );
   void HowToUse( std::string name);
   ///Get intial module information from the executive
   void InitModules( void );
   ///Update function called from within latePreFrame
   void UpdateModules( void );
   ///Update function called from within latePreFrame
   void PreFrameUpdate( void );
   ///Update function called from within latePreFrame
   void PostFrameUpdate( void );
   ///Function called within preFrame to allow cfdExecutive
   ///to have access to scalar information
   void UnbindORB( void );
   ///compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray* ){ return true; }
   ///in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand(){ ; }
   ///This function returns the map of the current plugins 
   ///so that evehenthandlers can manipulate the plugins while
   ///with commands from the gui
   std::map<int, cfdVEBaseClass* >* GetTheCurrentPlugins( void );
   ///Loading the Available Modules
   cfdVEAvail_Modules* av_modules;
   ///Register plugin specific eh's so that cfdexecutive only calls the correct
   ///plugin for a specific command
   //bool RegisterEHForGEPlugin( std::string commandName, cfdVEBaseClass* baseClass );
   
private:
   ///Laod data from CE
   void LoadDataFromCE( void );

   std::string _activeScalarName;
   cfdGauges* _gauges;
   cfdDashboard* _dashBoard;
   cfdInteractiveGeometry* _geometry;
   Body_UI_i* ui_i;
   VE_SceneGraph::cfdGroup* _masterNode;
   std::vector< VE_XML::XMLObject* > currentModels;

   bool _doneWithCalculations;
   bool updateNetworkString;
   bool runGetEverythingThread;
   // Classes and variables for multithreading.
   //cfdThread* thread;
   // _id_map : maps a module id to an interface object for a module's inputs.
   //std::map<int, Interface>   _it_map;

   // _pt_map : maps a module id to an interface object for a module's port data.
   //std::map<int, Interface>   _pt_map;

   // _pt_map : maps a module id to an interface object for a module's geom data.
   //std::map<int, Interface>   _geom_map;

   // _ot_map : maps a module id to an interface object for a modules's outputs.
   //std::map<int, Interface>   _ot_map;

   // _name_map : maps a module id to it's module name.
   std::map< int, std::string> _id_map;
   std::map< int, VE_XML::VE_Model::Model* > idToModel;

   // _name_map : maps a module name to it's module id.
   std::map<int, cfdVEBaseClass* > _plugins;

   // map to hold unique plugin command names and associated plugin pointers
   std::map< int, std::map< std::string, cfdVEBaseClass* > > pluginEHMap;
   
   std::map< std::string,VE_EVENTS::EventHandler*> _eventHandlers;///<The event handler for commands.
};
}
#endif
