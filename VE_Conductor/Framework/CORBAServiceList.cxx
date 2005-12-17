#include "VE_Conductor/Framework/CORBAServiceList.h"

#include "VE_Installer/installer/installerImages/ve_xplorer_banner.xpm"


/////////////////////////////////////////////////////////////
CORBAServiceList::CORBAServiceList( void )
:wxDialog(NULL,-1, wxString("CORBA Service List Pane") )
{
   this->SetIcon( wxIcon( ve_xplorer_banner_xpm ) );
}
/////////////////////////////////////////////////////////////
CORBAServiceList::~CORBAServiceList( void )
{
}
/////////////////////////////////////////////////////////////
void CORBAServiceList::SetNamingContext( CosNaming::NamingContext_ptr naming_context )
{
   namingContext = naming_context;
}
/////////////////////////////////////////////////////////////
std::vector< std::string > CORBAServiceList::GetListOfServices( void )
{
   unsigned long numServices;
   namingContext.list( numServices, bindList, nameList );
   //Need to look at CORBA book for for loop
   return serviceList;
}   
