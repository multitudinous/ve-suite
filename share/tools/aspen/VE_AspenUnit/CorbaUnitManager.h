#ifndef CORBA_UNIT_MANAGER
#define CORBA_UNIT_MANAGER
#include <ves/open/moduleS.h>
#include <string>
#include "AspenUnit_i.h"
#include "VE_AspenUnit.h"
#include "VE_AspenUnitDlg.h"
#include "bkpparser.h"

class CorbaUnitManager
{
public:
	CorbaUnitManager(CVE_AspenUnitDlg *);
   ~CorbaUnitManager(){;}
   void SetComputerNameUnitNameAndPort( CString name, CString port, CString uname );
   void SetRunORBFlag( bool run );
   void RunORB( void );
   void DestroyORB( void );
   Body_Unit_i* GetUnitObject( void );
   void CheckCORBAWork( void );
   BKPParser * CreateParser( void );
   //void CreateParser( void );
   bool unit_i_instantiated;

private:
   CString computerName;
   CString computerPort;
   CString unitName;
   Body_Unit_i* unit_i;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   CVE_AspenUnitDlg * parent;
};
#endif
