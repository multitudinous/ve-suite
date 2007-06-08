#include <string>

#include "VE_Xplorer/XplorerHandlers/ViewEventHandler.h"

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
ViewEventHandler::ViewEventHandler()
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler::ViewEventHandler(const ViewEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler::~ViewEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void ViewEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void ViewEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
    VE_XML::Command* command=dynamic_cast<VE_XML::Command*>(veXMLObject);
    VE_XML::DataValuePair* viewDVP=command->GetDataValuePair("ViewID");

    unsigned int view;
    viewDVP->GetData(view);

    if( viewDVP )
    {
        return;
    }

    if( view == 0 )
    {
        static_cast< VE_Xplorer::KeyboardMouse* >( 
            VE_Xplorer::DeviceHandler::instance()->
            GetDevice( "KeyboardMouse" ) )->FrameAll();
    }
    else if( view == 1 )
    {
        ;
    }
    else if( view == 2 )
    {
        static_cast< VE_Xplorer::KeyboardMouse* >( 
            VE_Xplorer::DeviceHandler::instance()->
            GetDevice( "KeyboardMouse" ) )->ResetTransforms();
    }
}
////////////////////////////////////////////////////////////////////////////////
ViewEventHandler& ViewEventHandler::operator=(const ViewEventHandler& rhs)
{
   if(this!=&rhs){
      VE_EVENTS::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
