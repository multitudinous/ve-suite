#ifndef HYPER_LAB_UNIT_CLIENT_H
#define HYPER_LAB_UNIT_CLIENT_H

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

class Body_Unit_i;

// --- C/C++ Libraries --- //
#include <string>

class CorbaUnitManager
{
public:
    CorbaUnitManager();
    ~CorbaUnitManager();

    void SetRunORBFlag( bool run );
    void RunORB( std::string computerName, std::string portNumber, std::string unitName );
    void CheckCORBAWork();

    Body_Unit_i* GetUnitObject();

private:
    Body_Unit_i* unit_i;
    CORBA::ORB_var orb;
};

#endif; //HYPER_LAB_UNIT_CLIENT_H
