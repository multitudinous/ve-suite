#ifndef _VE_UI_DESIGNPAR_TAB_H_
#define _VE_UI_DESIGNPAR_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>
#include <wx/notebook.h>
//#include "controlIds.h"
enum DESGINPAR_TAB_IDS{
   DESIGNPAR_UPDATE_BUTTON
};



class UI_DesignParTab : public wxPanel{
public:
   UI_DesignParTab(wxNotebook* tControl);
protected:
   void _buildPage();
   
   wxNotebook* _parent;
   //the controls
   wxTextCtrl* _param1;
   wxTextCtrl* _param2;
   wxTextCtrl* _param3;
   wxTextCtrl* _param4;
   wxTextCtrl* _param5;
   wxTextCtrl* _param6;
   wxTextCtrl* _param7;
   wxTextCtrl* _param8;
   wxButton* _updateButton;

   //event handlers
   void _onDesignPar(wxCommandEvent& event);  
   void _onUpdate(wxCommandEvent& event);  

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_DESIGNPAR_TAB_H_
