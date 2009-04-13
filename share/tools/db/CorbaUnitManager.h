
#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

// --- ACE/TAO Includes --- //
class Body_Unit_i;

// --- C/C++ Includes --- //
#include <string>

class CorbaUnitManager
{
public:
    ///Constructor
    CorbaUnitManager();

    ///Destructor
    ~CorbaUnitManager();

    ///
    void SetRunORBFlag( bool run );

    ///
    void RunORB();

    ///
    void CheckCORBAWork();

    ///
    void DestroyORB();

    ///
    bool CleanUp();

    ///
    Body_Unit_i* GetUnitObject();

protected:

private:
    ///
    bool unit_i_instantiated;

    ///
    std::string computerName;

    ///
    std::string portNumber;

    ///
    std::string unitName;

    ///
    Body_Unit_i* unit_i;

    ///
    CORBA::ORB_var orb;

    ///
    PortableServer::POA_var poa;

    ///
    Body::Executive_var exec;

};

#endif //CORBA_UNIT_MANAGER
