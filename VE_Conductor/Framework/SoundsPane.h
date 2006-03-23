#ifndef _VE_UI_SOUNDS_TAB_H_
#define _VE_UI_SOUNDS_TAB_H_

#include "VE_Open/skel/VjObsC.h"

#include <wx/panel.h>
#include <wx/dialog.h>

#include <xercesc/dom/DOM.hpp>
XERCES_CPP_NAMESPACE_USE

#include <vector>

class wxNotebook;
class wxButton;
class wxCheckListBox;
class wxSizer;

namespace VE_XML
{
   class Command;
   class DOMDocumentManager;
}

enum SOUNDS_TAB_IDS{
   SOUND_CBOX,
   SOUND_UPDATE_BUTTON
};

class SoundsPane : public wxDialog
{
public:
   SoundsPane( VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn );
   virtual ~SoundsPane(){;}
   void SetCommInstance( VjObs_ptr veEngine );
   //void SetDOMManager( VE_XML::DOMDocumentManager* domManagerIn );
   void SendCommandsToXplorer( void );
protected:
   void _buildPage();
   
   //the controls
   wxCheckListBox* _soundCBox;
   wxButton* _updateButton;

   std::vector< VE_XML::Command* > commands;
   VjObs_ptr xplorerPtr;
   int cId, cIso_value;
   short num_sounds;
   VjObs::scalar_p_var   soundNameArray;
   DOMDocument* doc;
   VE_XML::DOMDocumentManager* domManager;
   std::string dataValueName;

   //event handlers
   void _onSounds(wxCommandEvent& event);  
   void _onUpdate(wxCommandEvent& event);  

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_SOUNDS_TAB_H_
