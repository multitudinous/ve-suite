#ifndef DISPLAY_EVENT_HANDLER_H
#define DISPLAY_EVENT_HANDLER_H
/*!\file DisplayEventHandler.h
  DisplayEventHandler API
  */
/*!\class DisplayEventHandler
 * Class for changing trackball properties in xplorer
 */
#include "VE_Xplorer/XplorerHandlers/EventHandler.h"

namespace VE_XML
{
   class XMLObject;
}

namespace VE_SceneGraph
{
   class cfdDCS;
}

namespace VE_Xplorer
{
   class cfdGlobalBase;
}

namespace VE_EVENTS
{
   class DisplayEventHandler:public EventHandler
   {
      public:
         //Constructor
         DisplayEventHandler();

         //Copy Constructor
         DisplayEventHandler(const DisplayEventHandler& ceh);

         //Destructor
         virtual ~DisplayEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         DisplayEventHandler& operator=(const DisplayEventHandler& rhs);
   
      private:

   };
}

#endif//DISPLAY_EVENT_HANDLER_H
