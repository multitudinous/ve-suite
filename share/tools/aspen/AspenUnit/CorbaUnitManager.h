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
   void SetComputerNameUnitNameAndPort( CString dir, CString name, CString port, CString uname );
   void SetRunORBFlag( bool run );
   void RunORB( void );
   void DestroyORB( void );
   Body_Unit_i* GetUnitObject( void );
   void CheckCORBAWork( void );
   BKPParser * CreateParser( void );
   //DynParser * CreateParser( void );
   //void CreateParser( void );
   bool unit_i_instantiated;
   bool CleanUp( );

private:
   CString workingDir;
   CString computerName;
   CString computerPort;
   CString unitName;
   Body_Unit_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   CVE_AspenUnitDlg * parent;
   Body::Executive_var exec;
};
#endif
