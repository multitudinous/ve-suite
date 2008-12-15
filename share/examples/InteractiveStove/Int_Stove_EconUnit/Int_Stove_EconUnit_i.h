#ifndef UNIT_I_H_
#define UNIT_I_H_
#include <src/ves/ce/UnitWrapper/UnitWrapper.h>

class ExcelWrapper;

using namespace std;

class  Body_Unit_i : public UnitWrapper
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

  ExcelWrapper* Wrapper;

 protected:
  Body::Executive_var executive_;
  int return_state;
  //void error(std::string msg);
  //void warning(std::string msg);

  long numbaffles;
  vector<double> baffle1;
  vector<double> baffle2;
  vector<double> baffle3;
  vector<double> baffle4;
  vector<double> baffle5;
  vector<double> baffle6;
  vector<double> baffle7;
  vector<double> cost_array;
  long closesheets;
  bool excelRunning;
  
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
};

#endif

