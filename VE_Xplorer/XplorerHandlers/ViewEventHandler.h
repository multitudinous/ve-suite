#ifndef VIEW_EVENT_HANDLER_H
#define VIEW_EVENT_HANDLER_H

/*!\file ViewEventHandler.h
  ViewEventHandler API
  */
/*!\class ViewEventHandler
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
   class ViewEventHandler:public EventHandler
   {
      public:
         //Constructor
         ViewEventHandler();

         //Copy Constructor
         ViewEventHandler(const ViewEventHandler& ceh);

         //Destructor
         virtual ~ViewEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         ViewEventHandler& operator=(const ViewEventHandler& rhs);
   
      protected:

   };
}

#endif//TRACKBALL_EVENT_HANDLER_H