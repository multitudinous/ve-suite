#ifndef UNIT_I_H_
#define UNIT_I_H_

#include <Afx.h>
#include "OPPDWrapper.h"
#include "VEOpen/skel/moduleS.h"
#include "VE_Conductor/Framework/package.h" //so it can use the xerces stuff

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

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

  OPPDWrapper* Wrapper;

 protected:
  Body::Executive_var executive_;
  int return_state;
  void error(std::string msg);
  void warning(std::string msg);

  vector<double> fuelpardbls;
  vector<double> compardbls;
  vector<double> ambpardbls;
  vector<double> ventpardbls;
  vector<double> detectpardbls;
  vector<double> fv1thicktime;
  vector<double> fv1thicktemp;
  vector<double> fv2thicktime;
  vector<double> fv2thicktemp;
  vector<double> nvthicktime;
  vector<double> nvthicktemp;
  double intlinthickdbl;
  double massfuelburndbl;
  double solidfuelareadbl;
  double evalabvfiredbl;
  double cblburnareadbl;
  long tempmethod;
  long tempcalcmethod;
  long detectortype;
  long flametype;
  long detacttemp;
  long matselindex;
  long fuelselindex;
  long vismatselindex;
  long durmatselindex;
  long vispropselindex; 
  long viscombselindex;
  long detrtiselindex;
  long dettempratselindex;
  long detspaceselindex;
  long cableselindex;
  long killexcel;
  int flag;
  double tsec;
  double tmin;
  double hrrkw;
  double hrrbtu;
  double detsprinktime;
  double detsmtime;
  double detfthtime;
  double flwallinehgt;
  double flcornerhgt;
  double flwallhgt;
  double hrrhrr;
  double hrrburndur;
  double hrrhgthesk;
  double hrrhgtthom;
  double pltemp;
  double tcltemp;
  double visdist;
  double fv1thintemp;
  double fv2thintemp;
  double nvthintemp;
  
  char* _paramHack;
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
