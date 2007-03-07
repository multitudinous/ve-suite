#ifndef KEYBOARD_MOUSE_EVENT_HANDLER_H
#define KEYBOARD_MOUSE_EVENT_HANDLER_H

/*!\file KeyboardMouseEH.h
  KeyboardMouseEventHandler API
  */
/*!\class KeyboardMouseEventHandler
 * Class for changing KeyboardMouse properties in xplorer
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
   class KeyboardMouseEventHandler:public EventHandler
   {
      public:
         //Constructor
         KeyboardMouseEventHandler();

         //Copy Constructor
         KeyboardMouseEventHandler(const KeyboardMouseEventHandler& ceh);

         //Destructor
         virtual ~KeyboardMouseEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         KeyboardMouseEventHandler& operator=(const KeyboardMouseEventHandler& rhs);
   
      protected:

   };
}

#endif//KEYBOARD_MOUSE_EVENT_HANDLER_H