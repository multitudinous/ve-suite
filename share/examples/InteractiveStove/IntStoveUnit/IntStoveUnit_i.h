#ifndef UNIT_I_H_
#define UNIT_I_H_

#include <ves/open/moduleS.h>
#include <ves/ce/unitwrapper/UnitWrapper.h>
//#include "package.h" //so it can use the xerces stuff

using namespace std;

class Create_stove;

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

//Class Body_Unit_i
class  Body_Unit_i : public UnitWrapper
{
 public:
  //Constructor 
  Body_Unit_i (Body::Executive_ptr exec, std::string name);
  
  //Destructor 
  virtual ~Body_Unit_i (void);

  Create_stove* New_stove;

    long m_baffNum;
    std::vector< double > m_baffle1;
    std::vector< double > m_baffle2;
    std::vector< double > m_baffle3;
    std::vector< double > m_baffle4;
    std::vector< double > m_baffle5;
    std::vector< double > m_baffle6;
    std::vector< double > m_baffle7;
    unsigned int m_runNum;

 protected:
  Body::Executive_var executive_;
  int return_state;
  int runnum;
  void error(std::string msg);
  void warning(std::string msg);

  long numbaffles;
  vector<double> baffle1;
  vector<double> baffle2;
  vector<double> baffle3;
  vector<double> baffle4;
  vector<double> baffle5;
  vector<double> baffle6;
  vector<double> baffle7;
  vector<string> baffle1str;
  vector<string> baffle2str;
  vector<string> baffle3str;
  vector<string> baffle4str;
  vector<string> baffle5str;
  vector<string> baffle6str;
  vector<string> baffle7str;

 public:

virtual void StartCalc (
    ACE_ENV_SINGLE_ARG_DECL
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException
    , Error::EUnknown
  ));
};

#endif
