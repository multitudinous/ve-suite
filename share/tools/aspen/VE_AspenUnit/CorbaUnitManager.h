#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER
#include <ves/open/moduleS.h>
#include <string>
#include "AspenUnit_i.h"
#include "VE_AspenUnit.h"
#include "VE_AspenUnitDlg.h"
#include "BKPParser.h"
#include "DynParser.h"

class CorbaUnitManager
{
public:
    CorbaUnitManager(CVE_AspenUnitDlg *);
   ~CorbaUnitManager();
   void SetComputerNameUnitNameAndPort( std::string dir, std::string name, std::string port, std::string uname );
   void SetRunORBFlag( bool run );
   void RunORB( void );
   void DestroyORB( void );
   AspenUnit_i* GetUnitObject( void );
   void CheckCORBAWork( void );
   BKPParser * CreateParser( void );

   bool unit_i_instantiated;
   bool CleanUp( );
   Body::Executive_ptr GetExecutive(  );

private:
   std::string workingDir;
   std::string computerName;
   std::string computerPort;
   std::string unitName;
   AspenUnit_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   CVE_AspenUnitDlg * parent;
   Body::Executive_var exec;
};
#endif
