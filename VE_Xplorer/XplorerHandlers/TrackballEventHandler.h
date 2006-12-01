#ifndef TRACKBALL_EVENT_HANDLER_H
#define TRACKBALL_EVENT_HANDLER_H

/*!\file TrackballEventHandler.h
  TrackballEventHandler API
  */
/*!\class TrackballEventHandler
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
   class TrackballEventHandler:public EventHandler
   {
      public:
         //Constructor
         TrackballEventHandler();

         //Copy Constructor
         TrackballEventHandler(const TrackballEventHandler& ceh);

         //Destructor
         virtual ~TrackballEventHandler();

         //Set the cfdModel
         //param model The cfdModelHandler to execute the Command on
         void SetGlobalBaseObject(VE_Xplorer::cfdGlobalBase* modelHandler);
   
         //Exectute the event
         //param xmlObject The current xmlObject event.
         void Execute(VE_XML::XMLObject* command); 

         //Equal operator
         TrackballEventHandler& operator=(const TrackballEventHandler& rhs);
   
      protected:

   };
}

#endif//TRACKBALL_EVENT_HANDLER_H