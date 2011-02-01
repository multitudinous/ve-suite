#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER
#include <ves/open/moduleS.h>
#include <string>
#include "VEPSI_i.h"
#include "VE_PSI.h"
#include "VE_PSIDlg.h"
#include "AspenPlus.h"
#include "AspenDynamics.h"
#include "DynSim.h"
#include <vpr/Thread/Thread.h>

class CorbaUnitManager
{
public:
    CorbaUnitManager(VE_PSIDlg *);
   ~CorbaUnitManager();
   void SetComputerNameUnitNameAndPort( std::string dir, std::string name, std::string port, std::string uname );
   void SetRunORBFlag( bool run );
   void RunORB( void );
   void DestroyORB( void );
   VEPSI_i* GetUnitObject( void );
   void CheckCORBAWork( void );
   AspenPlus * CreateParser( void );
   void CheckCORBAWorkThread(  );

   bool unit_i_instantiated;
   bool CleanUp( );
   Body::Executive_ptr GetExecutive(  );

private:
   std::string workingDir;
   std::string computerName;
   std::string computerPort;
   std::string unitName;
   VEPSI_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   VE_PSIDlg * parent;
   Body::Executive_var exec;
   vpr::Thread* m_thread;
};
#endif
