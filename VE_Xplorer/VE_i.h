#ifndef VE_I_H_
#define VE_I_H_

#ifdef WIN32
#include <winsock2.h>
#endif
//#include "Network.h"
#include "moduleS.h"
#include "cfdExecutive.h"
//#include "moduleC.h"
#include <iostream>
#include <string>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

//Class Body_UI_i
class  Body_UI_i : public virtual POA_Body::UI
{
 public:
  //Constructor 
  Body_UI_i (Body::Executive_ptr exec, std::string name);
  
  //Destructor 
  virtual ~Body_UI_i (void);
  
  std::string UIName_;

 protected:
  Body::Executive_var executive_;
  cfdExecutive* executive;
  
 public:

  void SetcfdExecutive(cfdExecutive* nw) { 
	  executive=nw; 
  };

virtual void UpdateNetwork (
    const char * network
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateModuleUI (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateModuleResult (
    CORBA::Long module_id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void UpdateLinkContent (
    CORBA::Long id,
    const char * msg
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void Raise (
    const char * notification
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));
};


#endif
