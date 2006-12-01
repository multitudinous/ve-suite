#include <string>

#include "VE_Xplorer/XplorerHandlers/TrackballEventHandler.h"

#include "VE_Xplorer/XplorerHandlers/cfdGlobalBase.h"
#include "VE_Xplorer/XplorerHandlers/DeviceHandler.h"

#include "VE_Xplorer/XplorerHandlers/Trackball.h"

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
TrackballEventHandler::TrackballEventHandler()
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
TrackballEventHandler::TrackballEventHandler(const TrackballEventHandler& rhs)
:VE_EVENTS::EventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
TrackballEventHandler::~TrackballEventHandler()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void TrackballEventHandler::SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler)
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void TrackballEventHandler::Execute(VE_XML::XMLObject* veXMLObject)
{
   VE_XML::Command* command=dynamic_cast<VE_XML::Command*>(veXMLObject);
   VE_XML::DataValuePair* animateDVP=command->GetDataValuePair("AnimateID");

   unsigned int animate;
   animateDVP->GetData(animate);

   if(animateDVP){
      if(animate==0){
         VE_Xplorer::DeviceHandler::instance()->GetTrackball()->Animate(false);
      }
      else if(animate==1){
         VE_Xplorer::DeviceHandler::instance()->GetTrackball()->Animate(true);
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
TrackballEventHandler& TrackballEventHandler::operator=(const TrackballEventHandler& rhs)
{
   if(this!=&rhs){
      VE_EVENTS::EventHandler::operator=(rhs);
   }

   return *this;
}
////////////////////////////////////////////////////////////////////////////////
