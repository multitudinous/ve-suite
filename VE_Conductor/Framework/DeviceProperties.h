#ifndef DEVICES_PROPERTIES
#define DEVICES_PROPERTIES

/*!\file DeviceProperties.h
DeviceProperties API
*/
/*!\class DeviceProperties
* 
*/

#include <vector>
#include <string>

#include <wx/dialog.h>

class wxSplitterWindow;
class wxCheckBox;

namespace VE_XML
{
   class DataValuePair;
}

namespace VE_Conductor
{
   class CORBAServiceList;
}

class DeviceProperties:public wxDialog 
{
   public:
      DeviceProperties();

      virtual ~DeviceProperties();

   enum DEVICE_IDS 
   {
      DEVICE_SPLITTERWINDOW,
      DEVICE_LISTBOX,
      DEVICE_TRACKBALL_PANEL,
      DEVICE_WAND_PANEL,

      ANIMATE_CHECKBOX,
   };

   protected:
      void BuildGUI();

      wxSplitterWindow* device_splitter;
      wxCheckBox* animate_check_box;

      void OnAnimate(wxCommandEvent& event);

      bool animate;

      std::vector<VE_XML::DataValuePair*> instructions;        //The DataValuePairs for the current command

      void SendCommandsToXplorer();
      void ClearInstructions();

      VE_Conductor::CORBAServiceList* serviceList;

      DECLARE_EVENT_TABLE()
};
#endif
