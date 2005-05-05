#ifndef UI_ADVECTION_PANEL_H
#define UI_ADVECTION_PANEL_H
#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
#ifdef WIN32
#include <winsock2.h>
#endif
#include <wx/wx.h>

enum ADVECTION_IDS
{
   X_DYE_POS,
   Y_DYE_POS,
   Z_DYE_POS,
   DYE_GROUP,
   DYE_SLIDER_GROUP,
   DYE_CHECK,
   MATERIAL_GROUP,
   MATERIAL_COMBO,
   MATERIAL_DENSITY,
   MATERIAL_INJECTION,
   MATERIAL_DECAY,
   ENABLE_CHECK,
   BBOX_CHECK
};
class UI_AdvectionPanel: public wxPanel{
public:
   UI_AdvectionPanel(wxNotebook* tControl);
   ~UI_AdvectionPanel();
protected:
   void _buildPage();
   void _setGroupVisibility(bool onOff = true);
   void _setDyeVisibility(bool onOff);
   void _setMaterialVisibility(bool onOff);

   void _onSlider(wxScrollEvent& event);
   void _onEnableCheck(wxCommandEvent& event);
   void _onShowBBoxCheck(wxCommandEvent& event);
   void _onMaterialSwitch(wxCommandEvent& event);

   wxCheckBox* _enableCheck; 
   wxCheckBox* _enableBBox;

   wxStaticBox* _dyeGroup;
   wxStaticBox* _dyeSliderBox;  
   wxCheckBox* _dyeInjectCheck;
   wxSlider* _xDyeLocation;
   wxSlider* _yDyeLocation;
   wxSlider* _zDyeLocation;

   wxStaticBox* _materialGroup;
   wxComboBox* _materialCBox;
   wxSlider* _noiseDensityCtrl;
   wxSlider* _injectionStrengthCtrl;
   wxSlider* _decayStrengthCtrl;

   DECLARE_EVENT_TABLE()
};
#endif //CFD_USE_SHADERS
#endif //VE_PATENTED
#endif //UI_ADVECTION_PANEL_H