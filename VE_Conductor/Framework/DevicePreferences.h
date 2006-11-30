#ifndef DEVICES_PREFERENCES
#define DEVICES_PREFERENCES

/*!\file DevicePreferences.h
DevicePreferences API
*/
/*!\class DevicePreferences
* 
*/

#include <wx/dialog.h>

class wxSplitterWindow;

enum DEVICE_IDS 
{
   DEVICE_SPLITTERWINDOW,
   DEVICE_LISTBOX,
   DEVICE_PANEL,
   ANIMATE_CHECKBOX,
};

class DevicePreferences:public wxDialog 
{
   public:
      DevicePreferences();

      virtual ~DevicePreferences();

      void BuildGUI();

   private:
      wxSplitterWindow* device_splitter;

      DECLARE_EVENT_TABLE()
};

#endif