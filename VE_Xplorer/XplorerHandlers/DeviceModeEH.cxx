#include <string>

#include "VE_Xplorer/XplorerHandlers/DeviceModeEH.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"


#include "VE_Open/XML/XMLObject.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <boost/filesystem/operations.hpp>   //includes boost/filesystem/path.hpp
#include <boost/filesystem/path.hpp>

#ifdef WIN32
#include <direct.h>
#else
#include <unistd.h>
#endif

using namespace VE_EVENTS;

////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::DeviceModeEventHandler()
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::DeviceModeEventHandler( const DeviceModeEventHandler& rhs )
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler::~DeviceModeEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceModeEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceModeEventHandler::Execute( VE_XML::XMLObject* veXMLObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
   
   unsigned int mode;
   command->GetDataValuePair( "Mode" )->GetData( mode );

   VE_Xplorer::DeviceHandler::instance()->SetDeviceMode( mode );
}
////////////////////////////////////////////////////////////////////////////////
DeviceModeEventHandler& DeviceModeEventHandler::operator=( const DeviceModeEventHandler& rhs )
{
   if( this != &rhs )
   {
      VE_EVENTS::EventHandler::operator=( rhs );
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
