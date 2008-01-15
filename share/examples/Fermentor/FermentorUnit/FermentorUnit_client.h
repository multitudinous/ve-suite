#ifndef FERMENTOR_UNIT_CLIENT_H
#define FERMENTOR_UNIT_CLIENT_H

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

// --- C/C++ Libraries --- //
#include <string>

class Body_Unit_i;

class CorbaUnitManager
{
public:
    CorbaUnitManager();
    ~CorbaUnitManager();
    void SetRunORBFlag( bool run );
    void RunORB( std::string computerName ,std::string portNumber, std::string unitName );
    Body_Unit_i* GetUnitObject();
    void CheckCORBAWork();

private:
    Body_Unit_i* unit_i;
    CORBA::ORB_var orb;
};

#endif //FERMENTOR_UNIT_CLIENT_H
