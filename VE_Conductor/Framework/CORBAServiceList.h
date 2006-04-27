#ifndef CORBA_SERVICE_LIST
#define CORBA_SERVICE_LIST

#include <orbsvcs/CosNamingC.h>

namespace VE_Conductor
{
class CORBAServiceList //: public wxDialog
{
public:
   ///Constructor
   CORBAServiceList( void );
   ///Destructor
   virtual ~CORBAServiceList( void );

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
   bool SendCommandStringToXplorer( std::string command );
   ///Set ce network string 
   ///\param network string containing network
   bool SendNetworkStringToCE( std::string network );

private:
   std::vector< std::string > serviceList;
   // CORBA var
   CosNaming::BindingList_var bindList;
   CosNaming::Name_var nameList;
   CosNaming::NamingContext_var namingContext;
   Body::VEEngine_var vexplorer;
   Body::Executive_var vexplorer;
};
}
#endif
