#ifndef DefaultPlugin_H
#define DefaultPlugin_H

#include "VE_Conductor/GUIPlugin/Plugin_base.h"
#include <wx/image.h>

class DefaultPlugin : public REI_Plugin
{
   DECLARE_DYNAMIC_CLASS( DefaultPlugin )

public:
   DefaultPlugin();
   virtual ~DefaultPlugin();

   virtual double GetVersion();
   //Return the version number of the module

   virtual void DrawIcon(wxDC* dc);
   //This call return a window to be displayed on the framework

   virtual UIDialog* UI(wxWindow* parent);
   //This returns the UI dialog of the module

   virtual wxString GetName();
   //This returns the name of the module

   virtual wxString GetDesc();
   //This returns the description of the module, This should be a short description

protected:
   wxBitmap* my_icon;
   int icon_w, icon_h;
};

#endif
