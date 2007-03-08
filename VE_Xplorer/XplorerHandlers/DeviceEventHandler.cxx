#include <string>

#include "VE_Xplorer/XplorerHandlers/DeviceEventHandler.h"

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
DeviceEventHandler::DeviceEventHandler()
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler::DeviceEventHandler( const DeviceEventHandler& rhs )
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler::~DeviceEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceEventHandler::SetGlobalBaseObject( VE_Xplorer::cfdGlobalBase* modelHandler )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceEventHandler::Execute( VE_XML::XMLObject* veXMLObject )
{
   VE_XML::Command* command = dynamic_cast< VE_XML::Command* >( veXMLObject );
   std::string device;
   

   if( command->GetCommandName() == "CHANGE_DEVICE_MODE" )
   {
      unsigned int mode;
      command->GetDataValuePair( "Mode" )->GetData( mode );

      VE_Xplorer::DeviceHandler::instance()->SetDeviceMode( mode );
   }

   else if( command->GetCommandName() == "CHANGE_DEVICE" )
   {
      std::string device;
      command->GetDataValuePair( "Device" )->GetData( device );

      VE_Xplorer::DeviceHandler::instance()->SetActiveDevice( device );
   }
 
}
////////////////////////////////////////////////////////////////////////////////
DeviceEventHandler& DeviceEventHandler::operator=( const DeviceEventHandler& rhs )
{
   if( this != &rhs )
   {
      VE_EVENTS::EventHandler::operator=( rhs );
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
