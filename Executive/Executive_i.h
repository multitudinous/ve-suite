#ifndef EXECUTIVE_I_H_
#define EXECUTIVE_I_H_

#include "moduleS.h"
#include "interface.h"
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
  
protected:

  CosNaming::NamingContext_var naming_context_;
  Body::Unit_var cmu_unit_;
  std::map<std::string, Body::UI_var> uis_;

  std::map<int, MODULE_DATA> msg_data_;
  std::map<int, Interface> network_;
  std::map<int, Interface> port_data_;
  std::map<int, Interface> output_data_;

  Types::ArrayLong watch_list_;
  std::string status_;
  
  std::vector<CORBA::Long> exec_list_;
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

virtual void SetModuleMessage (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

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
    const char * UnitName
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

};

#endif
