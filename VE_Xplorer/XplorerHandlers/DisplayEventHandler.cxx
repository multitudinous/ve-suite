#include <string>

#include "VE_Xplorer/XplorerHandlers/DisplayEventHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/DisplayInformation.h"

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
DisplayEventHandler::DisplayEventHandler()
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler::DisplayEventHandler(const DisplayEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler::~DisplayEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DisplayEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   VE_XML::Command* command=dynamic_cast<VE_XML::Command*>(veXMLObject);
   VE_XML::DataValuePair* DVP;
   unsigned int value;

   if(command->GetDataValuePair("FrameRateID"))
	{
      DVP=command->GetDataValuePair("FrameRateID");
      DVP->GetData(value);
   
      VE_Xplorer::cfdEnvironmentHandler::instance()->GetDisplayInformation()->SetFrameRateFlag( value );
   }

   else if(command->GetDataValuePair("CoordSysID"))
	{
      DVP=command->GetDataValuePair("CoordSysID");
      DVP->GetData(value);
   
      VE_Xplorer::cfdEnvironmentHandler::instance()->GetDisplayInformation()->SetCoordSysFlag( value );
   }
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler& DisplayEventHandler::operator=(const DisplayEventHandler& rhs)
{
   if(this!=&rhs)
	{
      VE_EVENTS::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
