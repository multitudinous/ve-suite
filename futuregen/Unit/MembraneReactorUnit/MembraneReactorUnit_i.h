#ifndef UNIT_I_H_
#define UNIT_I_H_

#include "moduleS.h"
#include "package.h" //so it can use the xerces stuff
#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

extern "C"
{
  void cdfnor_(int* which, double* p, double* q, double* x, 
	       double* mean, double* sd, int* status, double* bound);
}

//Class Body_Unit_i
class  Body_Unit_i : public virtual POA_Body::Unit
{
 public:
  //Constructor 
  Body_Unit_i (Body::Executive_ptr exec, std::string name);
  
  //Destructor 
  virtual ~Body_Unit_i (void);
  
  std::string UnitName_;
  CORBA::Long id_;
  std::string status_;
  std::string data_;

 protected:
  Body::Executive_var executive_;
  int return_state;
  void error(std::string msg);
  void warning(std::string msg);

  double memb_diameter;
  double Pd_thickness;
  double L_rxr;
  double CO_conv_want;
  double shell_diameter;
  double mr_inlet_temp;
  double H2O_CO;
  long case_type;
  long n_modules;
  long f_pre_mr;
  long f_H2O_CO;

 public:

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

virtual char * GetStatusMessage (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * GetUserData (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetParams (
    const char * param
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetID (
    CORBA::Long id
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual CORBA::Long GetID (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual void SetName (
    const char * name
    ACE_ENV_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));

virtual char * GetName (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));
};

#endif
