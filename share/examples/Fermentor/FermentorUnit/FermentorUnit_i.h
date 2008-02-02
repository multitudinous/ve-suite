#ifndef FERMENTOR_UNIT_I_H
#define FERMENTOR_UNIT_I_H

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

// --- C/C++ Libraries --- //
#include <string>

class Body_Unit_i : public virtual POA_Body::Unit
{
public:
    Body_Unit_i( Body::Executive_ptr exec, std::string name );
    virtual ~Body_Unit_i();

    std::string UnitName_;
    CORBA::Long id_;
    std::string status_;
    std::string data_;

protected:
    Body::Executive_var executive_;
    int return_state;
    void error( std::string msg );
    void warning( std::string msg );

    double agitation;
    double air_conc;
    double ini_ph;
    double nitrate_conc;
    double temperature;
    double hours;
    long cycle_ID;

public:
    virtual void StartCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual void StopCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual void PauseCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual void Resume( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual char* GetStatusMessage( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual char* GetUserData( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException,Error::EUnknown ) );

    virtual void SetParams( CORBA::Long id, const char* param ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC ( ( CORBA::SystemException,Error::EUnknown ) );

    virtual void SetID( CORBA::Long id ACE_ENV_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual Types::ArrayLong* GetID( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void SetName( const char* name ACE_ENV_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual char* GetName( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void SetCurID( ::CORBA::Long id )
        ACE_THROW_SPEC( ( ::CORBA::SystemException,::Error::EUnknown ) );

    virtual ::CORBA::Long GetCurID()
        ACE_THROW_SPEC( ( ::CORBA::SystemException,::Error::EUnknown ) );  

    virtual char* Query( const char* commands )
        ACE_THROW_SPEC( ( ::CORBA::SystemException,::Error::EUnknown ) );

    virtual void DeleteModuleInstance( ::CORBA::Long module_id )
        ACE_THROW_SPEC( ( ::CORBA::SystemException,::Error::EUnknown ) );
};

#endif //FERMENTOR_UNIT_I_H
