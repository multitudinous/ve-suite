/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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

#include "moduleC.h"
#include "moduleS.h"
#include "interface.h"
#include "cfd1DTextInput.h"
#include "cfdGlobalBase.h"

#include <orbsvcs/CosNamingC.h>
#include <map>

#include <vpr/vpr.h>
#include <vpr/Sync/Mutex.h>
#include <vpr/Sync/Guard.h>

class cfdDCS;
class cfdGroup;
class cfdGauges;
class cfdDashboard;
class cfdExecutiveConfiguration;
class cfdInteractiveGeometry;
class Body_UI_i;
class cfdDataSet;
class cfdCommandArray;

class cfdExecutive : public cfdGlobalBase
{
   public:
      cfdExecutive( CosNaming::NamingContext_ptr nameing, cfdDCS*  );

      ~cfdExecutive( void );

      void init_orb_naming( void );

      // the Computational Engine
      Body::Executive_var _exec;
      //CORBA::ORB_var orb;
      PortableServer::POA_var poa;
      CosNaming::NamingContext_var naming_context;

      // _id_map : maps a module id to an interface object for a module's inputs.
      std::map<int, Interface>   _it_map;
  
      // _pt_map : maps a module id to an interface object for a module'ss port data.
      std::map<int, Interface>   _pt_map;
  
      // _ot_map : maps a module id to an interface object for a modules's outputs.
      std::map<int, Interface>   _ot_map;
  
      // _name_map : maps a module name to it's module id.
      std::map<std::string, int> _name_map;
  
      // Functions that operate on the Executive
      void GetNetwork( void );
      void GetOutput( std::string name);
      void GetPort( std::string name);
      void GetEverything ( void );
      void HowToUse( std::string name);

      // Get intial module information from the executive
      void InitModules( void );

      // Update function called from within preFrame
      void UpdateModules( void );
   
      // Function called within preFrame to allow cfdExecutive
      // to have access to scalar information
      void SetActiveDataSet( cfdDataSet* );

      void UnbindORB( void );

      cfdDataSet* _3dMesh;

      void SetCalculationsFlag( bool );

      bool GetCalculationsFlag( void );

      // compare VjObs_i commandArray with its child's value
      virtual bool CheckCommandId( cfdCommandArray* );

      // in future, multi-threaded apps will make a copy of VjObs_i commandArray
      virtual void UpdateCommand();

   private:
      
      cfdExecutiveConfiguration* _param;
      std::string _activeScalarName;
      cfdGauges* _gauges;
      cfdDashboard* _dashBoard;
      cfdInteractiveGeometry* _geometry;
      Body_UI_i* ui_i;
      cfdDCS* worldDCS;
      cfdGroup* _masterNode;

      vpr::Mutex  mValueLock;  /**< A mutex to protect variables accesses */
      bool _doneWithCalculations;

};

#endif
