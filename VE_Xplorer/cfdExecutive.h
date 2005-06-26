/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: cfdExecutive.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_EXECUTIVE_H
#define CFD_EXECUTIVE_H

#include "VE_Xplorer/cfdGlobalBase.h"
#include "interface.h"

#include <map>
#include <string>

namespace VE_SceneGraph{
   class cfdDCS; 
   class cfdGroup;
}
class cfdGauges;
class cfdDashboard;
class cfdExecutiveConfiguration;
class cfdInteractiveGeometry;
class Body_UI_i;
class cfdCommandArray;
class Network;
class cfdVEAvailModules;
class cfdVEBaseClass;
class cfdVEAvail_Modules;
class cfdVjObsWrapper;
class cfdThread;

namespace Body { class Executive; }
namespace CosNaming { class NamingContext; }
namespace PortableServer { class POA; }

class cfdExecutive : public cfdGlobalBase
{
   public:
      cfdExecutive( CosNaming::NamingContext*, PortableServer::POA* );

      ~cfdExecutive( void );

      void init_orb_naming( void );

      // the Computational Engine
      Body::Executive* _exec;

      CosNaming::NamingContext* naming_context;

      // _id_map : maps a module id to an interface object for a module's inputs.
      std::map<int, Interface>   _it_map;
  
      // _pt_map : maps a module id to an interface object for a module's port data.
      std::map<int, Interface>   _pt_map;
  
      // _ot_map : maps a module id to an interface object for a modules's outputs.
      //std::map<int, Interface>   _ot_map;
  
      // _name_map : maps a module id to it's module name.
      std::map< int, std::string> _id_map;
      std::map< std::string, int > _name_map;
  
      // _name_map : maps a module name to it's module id.
      std::map<int, cfdVEBaseClass* > _plugins;

      // Functions that operate on the Executive
      void GetNetwork( void );
      void GetOutput( std::string name);
      void GetPort( std::string name);
      void GetEverything( void );
      void HowToUse( std::string name);

      // Get intial module information from the executive
      void InitModules( void );

      // Update function called from within latePreFrame
      void UpdateModules( void );

      // Update function called from within latePreFrame
      void PreFrameUpdate( void );
 
      // Function called within preFrame to allow cfdExecutive
      // to have access to scalar information
      void UnbindORB( void );

      void SetCalculationsFlag( bool );

      bool GetCalculationsFlag( void );

      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdCommandArray* );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();

      //Loading the Available Modules
      cfdVEAvail_Modules* av_modules; 

      // Network class to decode network string
      Network* _network;
   private:
      
      cfdExecutiveConfiguration* _param;
      std::string _activeScalarName;
      cfdGauges* _gauges;
      cfdDashboard* _dashBoard;
      cfdInteractiveGeometry* _geometry;
      Body_UI_i* ui_i;
      VE_SceneGraph::cfdGroup* _masterNode;

      bool _doneWithCalculations;
      bool updateNetworkString;
      bool runGetEverythingThread;
      // Classes and variables for multithreading.
      //cfdThread* thread;
};

#endif
