#ifndef _VE_UI_GEOMETRY_TAB_H_
#define _VE_UI_GEOMETRY_TAB_H_
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum GEOMETRY_TAB_IDS
{
   GEOMETRY_RBOX,
   GEOMETRY_CBOX,
   GEOMETRY_UPDATE_BUTTON,
   GEOMETRY_OPACITY_SLIDER,
   GEOMETRY_LOD_SLIDER
};



class UI_GeometryTab : public wxPanel{
public:
   UI_GeometryTab(wxNotebook* tControl);
protected:
   void _buildPage();
   
   //the controls
   wxRadioBox* _geometryRBox;
   wxCheckListBox* _geometryCBox;
   wxButton*   _updateButton;
   wxNotebook* _parent;
   wxSlider* geomOpacitySlider;
   wxSlider* geomLODSlider;

   //event handlers
   void _onGeometry( wxScrollEvent& event );
   void _onUpdate(wxCommandEvent& event);

   DECLARE_EVENT_TABLE()
};
#endif //_VE_UI_GEOMETRY_TAB_H_

