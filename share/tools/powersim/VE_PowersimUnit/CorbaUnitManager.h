
#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER

// --- VE_PowersimUnit Includes --- //
class Body_Unit_i;
class CMainDlg;
class SIPParser;

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

class CorbaUnitManager
{
public:
    ///
    CorbaUnitManager( CMainDlg* mainDialog );

    ///
    ~CorbaUnitManager();

    ///
    void SetComputerNameUnitNameAndPort(
        const ATL::CString& workingDir,
        const ATL::CString& computerName,
        const ATL::CString& computerPort,
        const ATL::CString& unitName );

    ///
    void SetRunORBFlag( bool run );

    ///
    void RunORB();

    ///
    void DestroyORB();

    ///
    Body_Unit_i* GetUnitObject();

    ///
    void CheckCORBAWork();

    ///
    SIPParser* CreateParser();

    ///
    bool unit_i_instantiated;

    ///
    bool CleanUp();

protected:

private:
    ATL::CString m_workingDir;
    ATL::CString m_computerName;
    ATL::CString m_computerPort;
    ATL::CString m_unitName;

    Body_Unit_i* unit_i;
    CORBA::ORB_var orb;
    PortableServer::POA_var poa;
    CMainDlg* parent;
    Body::Executive_var exec;

};

#endif //CORBA_UNIT_MANAGER
