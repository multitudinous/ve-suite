#ifndef EXECUTIVE_I_H_
#define EXECUTIVE_I_H_

#include "Execute_Thread.h"

#include "Network_Exec.h"
#include "Scheduler.h"

#include "moduleS.h"
#include "package.h"
#include <string>
#include <vector>
#include <map>
#include <orbsvcs/orbsvcs/CosNamingC.h>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

typedef struct {
  CORBA::Long module_id;
  std::vector<std::string> data;
  std::string msg;
} MODULE_DATA;

//Class Body_Executive_i
class  Body_Executive_i : public virtual POA_Body::Executive
{
public:
  //Constructor 
  Body_Executive_i (CosNaming::NamingContext_ptr nc);
  
  //Destructor 
  virtual ~Body_Executive_i (void);
   
  void execute_next_mod (long module_id);
 
protected:

  void execute (std::string);

  std::map<std::string, Body::Unit_var> _mod_units;
  std::map<std::string, Execute_Thread*> _exec_thread;

  CosNaming::NamingContext_var naming_context_;
  std::map<std::string, Body::UI_var> uis_;

  Interface _network_intf;
  Interface _global_intf;

  Network*   _network;
  Scheduler* _scheduler;

  Types::ArrayLong watch_list_;

  ACE_Thread_Mutex _mutex;

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

virtual char * GetNetwork (
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

virtual void RegisterUI (
    const char * UIName
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void RegisterUnit (
    const char * UnitName,
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

};

#endif
