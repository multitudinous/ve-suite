#include <string>

#include "VE_Xplorer/XplorerHandlers/DisplayEventHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"

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
   VE_XML::DataValuePair* FrameRateDVP=command->GetDataValuePair("FrameRateID");

   unsigned int frame_rate;
   FrameRateDVP->GetData(frame_rate);

   if(FrameRateDVP){
      if(frame_rate==0){
         VE_Xplorer::cfdEnvironmentHandler::instance()->SetDisplayFrameRate(false);
      }
      else if(frame_rate==1){
         VE_Xplorer::cfdEnvironmentHandler::instance()->SetDisplayFrameRate(true);
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
DisplayEventHandler& DisplayEventHandler::operator=(const DisplayEventHandler& rhs)
{
   if(this!=&rhs){
      VE_EVENTS::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
