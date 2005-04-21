#ifndef _VE_UI_SOUNDS_TAB_H_
#define _VE_UI_SOUNDS_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum SOUNDS_TAB_IDS{
   SOUND_CBOX,
   SOUND_UPDATE_BUTTON
};

class UI_SoundTab : public wxPanel{
public:
   UI_SoundTab(wxNotebook* tControl);
protected:
   void _buildPage();
   
   wxNotebook* _parent;
   //the controls
   wxCheckListBox* _soundCBox;
   wxButton* _updateButton;

   //event handlers
   void _onSounds(wxCommandEvent& event);  
   void _onUpdate(wxCommandEvent& event);  

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_SOUNDS_TAB_H_
