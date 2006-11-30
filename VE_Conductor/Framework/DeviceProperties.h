#ifndef DEVICES_PROPERTIES
#define DEVICES_PROPERTIES

/*!\file DeviceProperties.h
DeviceProperties API
*/
/*!\class DeviceProperties
* 
*/

#include <wx/dialog.h>

class wxSplitterWindow;

enum DEVICE_IDS 
{
   DEVICE_SPLITTERWINDOW,
   DEVICE_LISTBOX,
   DEVICE_TRACKBALL_PANEL,
   DEVICE_WAND_PANEL,

   ANIMATE_CHECKBOX,
};

class DeviceProperties:public wxDialog 
{
   public:
      DeviceProperties();

      virtual ~DeviceProperties();

   protected:
      void BuildGUI();

      wxSplitterWindow* device_splitter;

      void OnAnimate(wxCommandEvent& event);

      DECLARE_EVENT_TABLE()
};

#endif