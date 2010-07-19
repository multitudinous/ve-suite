#ifndef VIRTUAL_LAB_UNIT_H
#define VIRTUAL_LAB_UNIT_H

#include <string>
#include <iostream>
#include <ves/open/moduleS.h>

#include <ves/ce/unitwrapper/UnitWrapper.h>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif /* ACE_LACKS_PRAGMA_ONCE */

namespace ves
{
namespace ce
{
namespace unitwrapper
{
   class EventHandler;
}
}
namespace open
{
namespace xml
{
namespace model
{
   class Model;
}
}
}
}

class Body_Unit_i : public UnitWrapper
{
public:
	Body_Unit_i (Body::Executive_ptr exec, std::string name );
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

    std::string mTextOne;

public:
	virtual void StartCalc( ACE_ENV_SINGLE_ARG_DECL )
	ACE_THROW_SPEC (( CORBA::SystemException, Error::EUnknown ));

	virtual void StopCalc (
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

#endif //VIRTUAL_LAB_UNIT

