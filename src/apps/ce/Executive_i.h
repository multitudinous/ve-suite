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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef EXECUTIVE_I_H_
#define EXECUTIVE_I_H_

#include <ves/ce/util/Network.h>
#include <ves/ce/util/Scheduler.h>
#include <ves/ce/util/Module.h>
#include <ves/ce/util/Connection.h>
#include <ves/ce/util/IPort.h>
#include <ves/ce/util/OPort.h>
using namespace VE_CE::Utilities;

#include <ves/open/skel/moduleS.h>
//#include "VE_Conductor/Framework/package.h>
#include <string>
#include <vector>
#include <map>
#include <orbsvcs/CosNamingC.h>
/*
#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif *//* ACE_LACKS_PRAGMA_ONCE */
/*
typedef struct {
  CORBA::Long module_id;
  std::vector<std::string> data;
  std::string msg;
} MODULE_DATA;
*/
//Class Body_Executive_i

class QueryThread;
class Execute_Thread;

class  Body_Executive_i : public virtual POA_Body::Executive
{
public:
  //Constructor 
  Body_Executive_i (CosNaming::NamingContext_ptr nc);
  
  //Destructor 
  virtual ~Body_Executive_i (void);
   
  void execute_next_mod (long module_id);
 
protected:

  void execute( std::string );

  std::map<std::string, Body::Unit_var> _mod_units;
  std::map<std::string, Execute_Thread*> _exec_thread;
  std::map<std::string, QueryThread*> queryThreads;

  CosNaming::NamingContext_var naming_context_;
  std::map<std::string, Body::UI_var> uis_;

  //Interface _network_intf;
  //Interface _global_intf;

  Network*   _network;
  Scheduler* _scheduler;

  // For tracking power requirements/generated and plant efficiencies
  std::map<long, double> _module_powers;
  std::map<long, double> _thermal_input;

  Types::ArrayLong watch_list_;

  ACE_Thread_Mutex _mutex;
  ACE_Thread_Mutex query;

public:

virtual char * GetImportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const char * data
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * GetExportData (
    CORBA::Long module_id,
    CORBA::Long port_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    const Types::Profile & data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual void GetProfileData (
    CORBA::Long module_id,
    CORBA::Long port_id,
    Types::Profile_out data
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual void SetModuleMessage (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetModuleResult (
    CORBA::Long module_id,
    const char * result
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual char * GetModuleResult (
    CORBA::Long module_id
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual void SetNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * GetNetwork ( const char* moduleName
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetModuleUI (
    CORBA::Long module_id,
    const char * ui
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));
 
virtual void SetWatchList (
    const Types::ArrayLong & id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual ::Types::ArrayLong * GetWatchList (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * GetStatus (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void StopCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void PauseCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void Resume (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * Query ( const char * command
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void RegisterUI (
    const char * UIName,
	Body::UI_ptr ui
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void RegisterUnit (
    const char * UnitName,
	Body::Unit_ptr unit,
    CORBA::Long module_id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UnRegisterUI (
    const char * UIName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual void UnRegisterUnit (
    const char * UnitName
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));// TAO_IDL - Generated from
// be/be_visitor_operation/operation_ih.cpp:43

virtual CORBA::Long GetGlobalMod (
    Types::ArrayLong_out ids
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetID( const char * moduleName, ::CORBA::Long id )
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));

virtual void DeleteModuleInstance( const char * moduleName, ::CORBA::Long module_id )
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));

virtual void SetParams( const char * moduleName, ::CORBA::Long module_id, const char * param )
   ACE_THROW_SPEC (( ::CORBA::SystemException, ::Error::EUnknown ));


void ClientMessage(const char *msg);

};

#endif
