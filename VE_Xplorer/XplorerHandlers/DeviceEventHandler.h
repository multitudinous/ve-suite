#ifndef DEVICE_EVENT_HANDLER_H
#define DEVICE_EVENT_HANDLER_H

/*!\file DeviceEventHandler.h
  DeviceEventHandler API
  */
/*!\class DeviceEventHandler
 * Class for changing trackball properties in xplorer
 */

#include "VE_Xplorer/XplorerHandlers/EventHandler.h"

namespace VE_XML
{
   class XMLObject;
}

namespace VE_Xplorer
{
   class cfdGlobalBase;
}

namespace VE_EVENTS
{
   class DeviceEventHandler:public EventHandler
   {
      public:
         //Constructor
         DeviceEventHandler();

         //Copy Constructor
         DeviceEventHandler(const DeviceEventHandler& ceh);

         //Destructor
         virtual ~DeviceEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         DeviceEventHandler& operator=(const DeviceEventHandler& rhs);
   
      protected:

   };
}

#endif//DEVICE_EVENT_HANDLER_H