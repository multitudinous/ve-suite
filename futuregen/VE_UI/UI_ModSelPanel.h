#ifndef _VE_UI_MOD_SEL_PANEL_H_
#define _VE_UI_MOD_SEL_PANEL_H_
#ifdef WIN32
#include <winsock2.h>
#endif



#include <wx/wx.h>
#include <wx/notebook.h>
#include <stdlib.h>
#include <vector>


using namespace std;
class UI_Frame;
class UI_ModelData;

enum MODSEL_PANEL_IDS{
   RBOX_MODEL_SELECT
};
class UI_ModSelScroll: public wxScrolledWindow{
public:
   UI_ModSelScroll(wxWindow* parent);
   ~UI_ModSelScroll();
   wxString* _models;
   wxRadioBox* _modelSelBox;


   DECLARE_EVENT_TABLE()
};



class UI_ModSelPanel: public wxPanel{
public:
   UI_ModSelPanel(wxWindow* parent, UI_ModelData* _model);
   ~UI_ModSelPanel();

   UI_ModSelScroll* _modselScroll;

   UI_ModelData* _modelData;
  
   void _onModSelect();

   DECLARE_EVENT_TABLE()
};

#endif //_VE_UI_MOD_SEL_PANEL_H_
