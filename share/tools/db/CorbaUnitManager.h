
#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

class Body_Unit_i;

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32

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
    bool RunORB(
        std::string& workingDirectory,
        std::string& computerName,
        std::string& portNumber );

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
    Body_Unit_i* unit_i;

    ///
    CORBA::ORB_var orb;

    ///
    PortableServer::POA_var poa;

    ///
    Body::Executive_var exec;

};

#endif //CORBA_UNIT_MANAGER
