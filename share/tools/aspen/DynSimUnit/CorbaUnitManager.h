#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER
#include <ves/open/moduleS.h>
#include <string>
#include "AspenUnit_i.h"
#include "DynSimUnit.h"
#include "DynSimUnitDlg.h"
#include <vpr/Thread/Thread.h>

class CorbaUnitManager
{
public:
    CorbaUnitManager(CDynSimUnitDlg *);
   ~CorbaUnitManager();
   void SetComputerNameUnitNameAndPort( CString dir, CString name, CString port, CString uname );
   void SetRunORBFlag( bool run );
   void RunORB();
   void DestroyORB(  );
   AspenUnit_i* GetUnitObject(  );
   void CheckCORBAWorkThread(  );
   void CheckCORBAWork(  );
   //BKPParser * CreateParser( void );

   bool unit_i_instantiated;
   bool CleanUp( );
   Body::Executive_ptr GetExecutive(  );

private:
   CString workingDir;
   CString computerName;
   CString computerPort;
   CString unitName;
   AspenUnit_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   CDynSimUnitDlg * parent;
   Body::Executive_var exec;
    vpr::Thread* m_thread;
};
#endif
