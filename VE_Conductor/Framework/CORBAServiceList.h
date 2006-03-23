#ifndef CORBA_SERVICE_LIST
#define CORBA_SERVICE_LIST

#include <orbsvcs/CosNamingC.h>

namespace VE_Conductor
{
class CORBAServiceList : public wxDialog
{
public:
   CORBAServiceList( void );
   virtual ~CORBAServiceList( void );

   void SetNamingContext( CosNaming::NamingContext_ptr naming_context );
   std::vector< std::string > GetListOfServices( void );
   
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
