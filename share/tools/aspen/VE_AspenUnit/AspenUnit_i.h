// -*- C++ -*-
//
// $Id$

// ****  Code generated by the The ACE ORB (TAO) IDL Compiler ****
// TAO and the TAO IDL Compiler have been developed by:
//       Center for Distributed Object Computing
//       Washington University
//       St. Louis, MO
//       USA
//       http://www.cs.wustl.edu/~schmidt/doc-center.html
// and
//       Distributed Object Computing Laboratory
//       University of California at Irvine
//       Irvine, CA
//       USA
//       http://doc.ece.uci.edu/
// and
//       Institute for Software Integrated Systems
//       Vanderbilt University
//       Nashville, TN
//       USA
//       http://www.isis.vanderbilt.edu/
//
// Information about TAO is available at:
//     http://www.cs.wustl.edu/~schmidt/TAO.html

// TAO_IDL - Generated from 
// .\be\be_codegen.cpp:1001

#ifndef MODULEI_H_
#define MODULEI_H_
#include "BKPParser.h"
#include "DynParser.h"
#include <ves/open/moduleS.h>
#include <ves/open/xml/CommandPtr.h>
#include "VE_AspenUnit.h"
#include "VE_AspenUnitDlg.h"
#include "CorbaUnitManager.h"

#include <set>

class  Body_Unit_i : public virtual POA_Body::Unit
{
public:
    // Constructor 
    Body_Unit_i (std::string name, CVE_AspenUnitDlg * dialog,
        CorbaUnitManager * parent, std::string dir );
    //Body_Unit_i() {};
    //Destructor 
    virtual ~Body_Unit_i (void);

    std::string UnitName_;
    Types::ArrayLong ids_;
    ::CORBA::Long cur_id_;

    std::string status_;
    std::string data_;

protected:
    //Body::Executive_var executive_;
    unsigned int return_state;
    CVE_AspenUnitDlg * theDialog;
    CorbaUnitManager * theParent;
    CEdit * AspenLog;
    std::set< std::string > mQueryCommandNames;
    std::string mWorkingDir;
    std::string mFileName;

public:
    BKPParser* bkp;
    DynParser* dyn;
  
  void ShowAspen( );  
  void HideAspen( );  
  void CloseAspen( );
  void ReinitializeAspen( );
  void SaveAspen( std::string filename );
  void StepSim( );
  
  virtual
  void StartCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void StopCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void PauseCalc (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void Resume (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  char * GetStatusMessage (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  char * GetUserData (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
	  void SetParams (CORBA::Long id,
      const char * param
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  void SetID (
      ::CORBA::Long id
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

  virtual
  void SetCurID (
      ::CORBA::Long id
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
	  ::Types::ArrayLong* GetID (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
	  ::CORBA::Long GetCurID (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));

  virtual void Body_Unit_i::DeleteModuleInstance(CORBA::Long id) 
	ACE_THROW_SPEC ((
    ::CORBA::SystemException,
    ::Error::EUnknown
  ));

  virtual
  void SetName (
      const char * name
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  char * GetName (
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  
  virtual
  char * Query (const char* commands
      
    )
    ACE_THROW_SPEC ((
      ::CORBA::SystemException,
      ::Error::EUnknown
    ));
  void ReinitializeBlock(ves::open::xml::CommandPtr cmd);
  char* handleGetNetwork(ves::open::xml::CommandPtr cmd);
  char* handleOpenSimulation(ves::open::xml::CommandPtr cmd);
  char* handleSaveAs(ves::open::xml::CommandPtr cmd);
  char* handleGetModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetInputModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetInputModuleProperties(ves::open::xml::CommandPtr cmd);
  char* handleGetOutputModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetOutputModuleProperties(ves::open::xml::CommandPtr cmd);
  char* handleGetStreamModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetStreamInputModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetStreamInputModuleProperties(ves::open::xml::CommandPtr cmd);
  char* handleGetStreamOutputModuleParamList(ves::open::xml::CommandPtr cmd);
  char* handleGetStreamOutputModuleProperties(ves::open::xml::CommandPtr cmd);
  void SetParam(ves::open::xml::CommandPtr cmd);

private:
  bool bkpFlag;
  bool dynFlag;
};


#endif /* MODULEI_H_  */

