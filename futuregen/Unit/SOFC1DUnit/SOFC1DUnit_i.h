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

  double a_height;
  double a_width;
  double a_space;
  double a_ecd;
  double a_tcoeff;
  double a_thick;
  double a_presdrop;
  double c_height;
  double c_width;
  double c_space;
  double c_ecdb;
  double c_ecdm;
  double c_tcoeffb;
  double c_tcoeffm;
  double c_thick;
  double c_presdrop;
  double s_thick;
  double s_heatcap;
  double s_density;
  double e_thick;
  double e_preexp;
  double e_exp;
  double f_preexp;
  double f_exp;
  double a_preexp;
  double a_exp;
  double i_preexp;
  double i_exp;
  double l_heatcap;
  double l_density;
  double l_length;
  double l_width;
  double stop_time;
  double loadres;
  string work_dir;
  long l_numCells;
  long ax_nodes;
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
