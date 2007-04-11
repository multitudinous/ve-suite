#include <string>

#include "VE_Xplorer/XplorerHandlers/KeyboardMouseEH.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

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
KeyboardMouseEventHandler::KeyboardMouseEventHandler()
:
VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler::KeyboardMouseEventHandler(const KeyboardMouseEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler::~KeyboardMouseEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouseEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouseEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   VE_XML::Command* command=dynamic_cast<VE_XML::Command*>(veXMLObject);
   VE_XML::DataValuePair* animateDVP=command->GetDataValuePair("AnimateID");

   unsigned int animate;
   animateDVP->GetData(animate);

   if( animateDVP )
   {
      if( animate == 0 )
		{
         static_cast< VE_Xplorer::KeyboardMouse* >( VE_Xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->Animate( false );
      }

      else if( animate == 1 )
		{
         static_cast< VE_Xplorer::KeyboardMouse* >( VE_Xplorer::DeviceHandler::instance()->GetDevice( "KeyboardMouse" ) )->Animate( true );
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouseEventHandler& KeyboardMouseEventHandler::operator=(const KeyboardMouseEventHandler& rhs)
{
   if(this!=&rhs){
      VE_EVENTS::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
