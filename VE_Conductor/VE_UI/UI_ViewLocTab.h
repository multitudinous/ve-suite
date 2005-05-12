#ifndef _VE_UI_VIEWLOC_TAB_H_
#define _VE_UI_VIEWLOC_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif

#include <wx/wx.h>

enum VIEWLOC_TAB_IDS{
   VIEWLOC_RBOX,
   VIEWLOC_LOAD_BUTTON,
   VIEWLOC_WRITE_BUTTON,
   VIEWLOC_READ_BUTTON,
   //VIEWLOC_REMOVE_BUTTON,
   VIEWLOC_MOVE_BUTTON
};


class UI_ViewLocTab : public wxPanel{
public:
   UI_ViewLocTab(wxNotebook* tControl);
   ~UI_ViewLocTab( void );

protected:
   void _buildPage();
   
   wxNotebook* _parent;
   //the controls
   wxRadioBox* _locationsRBox;
   wxButton* _loadButton;
   //wxButton* _removeButton;
   wxButton* _writeButton;
   wxButton* _readButton;
   wxButton* _moveButton;
   //wxButton* _applynameButton;
   //wxTextCtrl* _viewpointName;
   //event handlers
   void _onViewLoc(wxCommandEvent& event);
   void _onLoad(wxCommandEvent& event);
   void _onWrite(wxCommandEvent& event);
   void _onRead(wxCommandEvent& event);
   //void _onDetach(wxCommandEvent& event);
   void _onMove(wxCommandEvent& event);

   wxString* _defaultName;

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_VIEWLOC_TAB_H_
