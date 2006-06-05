#ifndef CORBA_SERVICE_LIST
#define CORBA_SERVICE_LIST

#include <orbsvcs/CosNamingC.h>
#include "VE_Open/skel/moduleC.h"
#include "VE_Open/skel/VjObsC.h"
#include "VE_Conductor/Framework/UI_i.h"

#include <vector>
#include <string>

class PEThread;
class AppFrame;
class Body_UI_i;

namespace VE_XML
{
   class Command;
}

namespace VE_Conductor
{
class CORBAServiceList //: public wxDialog
{
public:
   ///Constructor
   CORBAServiceList( AppFrame* frame );
   ///Destructor
   ~CORBAServiceList( void );

   ///Set a naming context
   ///\param naming_context
   void SetNamingContext( CosNaming::NamingContext_ptr naming_context );
   ///Return the list of services that are connected to the name server
   std::vector< std::string > GetListOfServices( void );
   
   ///Function to tell whether we are connected to xplorer
   bool IsConnectedToXplorer( void );
   ///Function to tell whether we are connected to ce
   bool IsConnectedToCE( void );
   ///Connect to xplorer
   bool ConnectToXplorer( void );
   ///connect to ce
   bool ConnectToCE( void );
   ///Disconnect to xplorer
   bool DisconnectFromXplorer( void );
   ///Disconnect to ce
   bool DisconnectFromCE( void );
   ///Set xplorer command string 
   ///\param command string containing command
   bool SendCommandStringToXplorer(  VE_XML::Command* veCommand  );
   ///Set ce network string 
   ///\param network string containing network
   bool SendNetworkStringToCE( std::string network );
   ///Keep the orb running and check for corba commands to be processed
   void CheckORBWorkLoad( void );
   ///Connect to the CORBA naming service
   bool ConnectToNamingService( void );
   ///Connect to the CORBA naming service
   bool IsConnectedToNamingService( void );
   ///Return the pointer to the xplorer corba object
   VjObs_ptr GetXplorerPointer( void );
   ///Return log pointer
   PEThread* GetMessageLog( void );

private:
   void CreateCORBAModule( void );
      
   std::vector< std::string > serviceList;
   // CORBA var
   CosNaming::BindingList_var bindList;
   CosNaming::Name_var nameList;
   CosNaming::NamingContext_var namingContext;
   CORBA::ORB_var orb;
   PortableServer::POA_var poa;
   PortableServer::POA_var poa_root;
   CosNaming::NamingContext_var naming_context;
   VjObs_var vjobs;
   Body::Executive_var module;
   Body::VEEngine_var veXplorer;
   Body::Executive_var veCE;
   Body_UI_i* p_ui_i;
   PEThread* pelog;
   AppFrame* frame;
};
}
#endif
