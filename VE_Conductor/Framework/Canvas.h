#ifndef CANVAS_H
#define CANVAS_H
/*!\file Canvas.h
Canvas API
*/
/*!\class Canvas
* 
*/

#include "VE_Conductor/Framework/Network.h"
#include "VE_Open/XML/Model/Tag.h"
#include "VE_Open/XML/Model/TagPtr.h"
#include "VE_Open/XML/User.h"
#include "VE_Open/XML/UserPtr.h"

#include <wx/event.h>
#include <wx/scrolwin.h>
#include <wx/textdlg.h>
#include <wx/menu.h>
#include <wx/thread.h>
#include <vector>
#include <map>
#include <iostream>
#include <wx/dcclient.h>

class AppFrame;

class Canvas : public wxScrolledWindow
{
public:
   Canvas(){;}
   Canvas(wxWindow* parent, int id );
   virtual ~Canvas();
   void OnPaint( wxPaintEvent &event );
   Network * GetActiveNetwork();
   void SetActiveNetwork(std::string id);
   void PopulateNetworks( std::string xmlNetwork );
   ///User scale
   /// first = x scale
   /// second = y scale
   std::pair< double, double > userScale;
   std::string Save( std::string fileName );
   void New( bool promptClearXplorer );
   ///Get the correct size for sub dialogs
   wxRect GetAppropriateSubDialogSize();

protected:

private:
   std::map < std::string, Network * > networks;
   std::string activeId;
   std::string previousId;
   void DrawNetwork(wxDC &dc, std::string id);
   DECLARE_EVENT_TABLE() // no semicolon needed
};

#endif
