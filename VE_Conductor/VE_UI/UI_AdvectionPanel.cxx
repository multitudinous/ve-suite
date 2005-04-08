#ifdef VE_PATENTED
#ifdef CFD_USE_SHADERS
#include "cfdEnum.h"
#include "UI_Tabs.h"
#include "UI_AdvectionPanel.h"
#include "wx/string.h"
#include "wx/notebook.h"
#include <iostream>

BEGIN_EVENT_TABLE(UI_AdvectionPanel,wxPanel)
   EVT_COMMAND_SCROLL(X_DYE_POS, _onSlider)
   EVT_COMMAND_SCROLL(Y_DYE_POS, _onSlider)
   EVT_COMMAND_SCROLL(Z_DYE_POS, _onSlider)
   EVT_COMMAND_SCROLL(MATERIAL_DENSITY, _onSlider)
   EVT_COMMAND_SCROLL(MATERIAL_INJECTION, _onSlider)
   EVT_COMMAND_SCROLL(MATERIAL_DECAY, _onSlider)
   EVT_CHECKBOX(ENABLE_CHECK,_onEnableCheck)
   EVT_CHECKBOX(BBOX_CHECK,_onShowBBoxCheck)
END_EVENT_TABLE()

//////////////////////////////////////////////////////////
//Constructor                                           //
//////////////////////////////////////////////////////////
UI_AdvectionPanel::UI_AdvectionPanel(wxNotebook* tControl)
:wxPanel(tControl)
{
   _buildPage();
}
///////////////////////////////////////
UI_AdvectionPanel::~UI_AdvectionPanel()
{

}
////////////////////////////////////
//build the advection panel tab   //
////////////////////////////////////
void UI_AdvectionPanel::_buildPage()
{
   //set up the grouping boxes
   //dye group
   _dyeGroup = new wxStaticBox(this, DYE_GROUP,"Dye Emmiter");
   _dyeSliderBox = new wxStaticBox(this, DYE_SLIDER_GROUP,"Position");
   _dyeInjectCheck = new wxCheckBox(this, DYE_CHECK, "Inject");
   wxStaticText* xLabel = new wxStaticText(this, -1, wxT("X"));
   wxStaticText* yLabel = new wxStaticText(this, -1, wxT("Y"));
   wxStaticText* zLabel = new wxStaticText(this, -1, wxT("Z"));
   
   wxBoxSizer* xSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* ySizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* zSizer = new wxBoxSizer(wxHORIZONTAL);
   
   _xDyeLocation = new wxSlider(this,
                             X_DYE_POS, 0, 
			                     0,100,
                             wxDefaultPosition, wxDefaultSize,
                             wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                             wxSL_LABELS|wxSL_RIGHT);

   xSizer->Add(xLabel,0,wxALIGN_LEFT|wxEXPAND);
   xSizer->Add(_xDyeLocation,1,wxALIGN_LEFT|wxEXPAND);

   _yDyeLocation = new wxSlider(this,
                             Y_DYE_POS, 0,
                             0,100,
                             wxDefaultPosition, wxDefaultSize,
                             wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                             wxSL_LABELS|wxSL_RIGHT );

   ySizer->Add(yLabel,0,wxALIGN_LEFT|wxEXPAND);
   ySizer->Add(_yDyeLocation,1,wxALIGN_LEFT|wxEXPAND);

   _zDyeLocation = new wxSlider(this,
                            Z_DYE_POS, 0, 
                            0,100,
                            wxDefaultPosition, wxDefaultSize,
                            wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                            wxSL_LABELS|wxSL_RIGHT );

   zSizer->Add(zLabel,0,wxALIGN_LEFT|wxEXPAND);
   zSizer->Add(_zDyeLocation,1,wxALIGN_LEFT|wxEXPAND);

   wxStaticBoxSizer* dyeSliderSizer = new wxStaticBoxSizer(_dyeSliderBox,wxVERTICAL);
   dyeSliderSizer->Add(xSizer,1,wxALIGN_CENTER|wxEXPAND);
   dyeSliderSizer->Add(ySizer,1,wxALIGN_CENTER|wxEXPAND);
   dyeSliderSizer->Add(zSizer,1,wxALIGN_CENTER|wxEXPAND);

   wxStaticBoxSizer* dyeSGroupSizer = new wxStaticBoxSizer(_dyeGroup,wxVERTICAL);
   dyeSGroupSizer->Add(_dyeInjectCheck,1,wxALIGN_CENTER|wxEXPAND);
   dyeSGroupSizer->Add(dyeSliderSizer,2,wxALIGN_CENTER|wxEXPAND);
   
   //material group
   _materialGroup = new wxStaticBox(this, MATERIAL_GROUP,"Injection Materials");
   wxString materialColors[2] = {"Green","Blue"};
   _materialCBox = new wxComboBox(this, 
                               MATERIAL_COMBO, 
                               "", 
                               wxDefaultPosition,
                               wxDefaultSize, 
                               2,materialColors, 
                               wxCB_READONLY, 
                               wxDefaultValidator, 
                               "Injection Color");
   _materialCBox->SetSelection(0);
   wxStaticText* nLabel = new wxStaticText(this, -1, wxT("Injection Density"));
   wxStaticText* iLabel = new wxStaticText(this, -1, wxT("Injection Strength"));
   wxStaticText* dLabel = new wxStaticText(this, -1, wxT("Decay Factor"));
   
   wxBoxSizer* nSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* iSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* dSizer = new wxBoxSizer(wxHORIZONTAL);
   _noiseDensityCtrl = new wxSlider(this,MATERIAL_DENSITY,
                                 100, 
                                 0,100,
                                 wxDefaultPosition, wxDefaultSize,
                                 wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                                 wxSL_LABELS|wxSL_RIGHT );
   nSizer->Add(nLabel,0,wxALIGN_LEFT|wxEXPAND);
   nSizer->Add(_noiseDensityCtrl,1,wxALIGN_LEFT|wxEXPAND);

   _injectionStrengthCtrl = new wxSlider(this,MATERIAL_INJECTION,
                                     20,0,100,
                                     wxDefaultPosition, wxDefaultSize,
                                     wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                                     wxSL_LABELS|wxSL_RIGHT );
   iSizer->Add(iLabel,0,wxALIGN_LEFT|wxEXPAND);
   iSizer->Add(_injectionStrengthCtrl,1,wxALIGN_LEFT|wxEXPAND);

   _decayStrengthCtrl = new wxSlider(this,MATERIAL_DECAY,
                                  80,0,100,
                                  wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                                  wxSL_LABELS|wxSL_RIGHT );

   dSizer->Add(dLabel,0,wxALIGN_LEFT|wxEXPAND);
   dSizer->Add(_decayStrengthCtrl,1,wxALIGN_LEFT|wxEXPAND);

   wxStaticBoxSizer* materialGroupSizer = new wxStaticBoxSizer(_materialGroup,wxVERTICAL);
   materialGroupSizer->Add(_materialCBox,1,wxALIGN_CENTER|wxEXPAND);
   wxBoxSizer* matControlSizer = new wxBoxSizer(wxVERTICAL);
   matControlSizer->Add(nSizer,1,wxALIGN_CENTER|wxEXPAND);
   matControlSizer->Add(iSizer,1,wxALIGN_CENTER|wxEXPAND);
   matControlSizer->Add(dSizer,1,wxALIGN_CENTER|wxEXPAND);
   materialGroupSizer->Add(matControlSizer,3,wxALIGN_CENTER|wxEXPAND);

   _enableCheck = new wxCheckBox(this, ENABLE_CHECK, "Enable Advection");
   _enableBBox = new wxCheckBox(this,BBOX_CHECK, "Display Bounds");
   _enableBBox->SetValue(true);
   //craziness w/ sizers!!!
   wxBoxSizer* advectionPanelGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* enableSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* groupSizer = new wxBoxSizer(wxVERTICAL);

   enableSizer->Add(_enableCheck,0,wxEXPAND|wxALL);
   enableSizer->Add(_enableBBox,0,wxEXPAND|wxALL);
   groupSizer->Add(dyeSGroupSizer,1,wxEXPAND|wxALIGN_CENTER);
   groupSizer->Add(materialGroupSizer,1,wxEXPAND|wxALIGN_CENTER);

   advectionPanelGroup->Add(groupSizer, 3, wxEXPAND|wxALIGN_CENTER);
   advectionPanelGroup->Add(enableSizer, 0,wxALIGN_BOTTOM|wxALIGN_CENTER );
   SetSize( GetSize() );

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   SetSizer(advectionPanelGroup);
   _setGroupVisibility(false);
   Enable(false);
}
///////////////////////////////////////////////////////
void UI_AdvectionPanel::_setGroupVisibility(bool onOff)
{
   if(_dyeGroup){
      _dyeGroup->Enable(onOff);
   }
   if(_materialGroup){
      _materialGroup->Enable(onOff);
   }
   
   if(_dyeSliderBox)
   {
      _dyeSliderBox->Enable(onOff);
   } 
   if(_dyeInjectCheck)
   {
      _dyeInjectCheck->Enable(onOff);
   } 
   if(_xDyeLocation)
   {
      _xDyeLocation->Enable(onOff);
   }
   if(_yDyeLocation)
   {
      _yDyeLocation->Enable(onOff);
   }
   if(_zDyeLocation)
   {
      _zDyeLocation->Enable(onOff);
   }
   if(_materialGroup)
   {
      _materialGroup->Enable(onOff);
   }
   if(_materialCBox)
   {
      _materialCBox->Enable(onOff);
   }
   if(_noiseDensityCtrl)
   {
      _noiseDensityCtrl->Enable(onOff);
   }
   if(_injectionStrengthCtrl)
   {
      _injectionStrengthCtrl->Enable(onOff);
   }
   if(_decayStrengthCtrl)
   {
      _decayStrengthCtrl->Enable(onOff);
   }
}
//////////////////////////////////////////////////////////////
//event callbacks                                           //
//////////////////////////////////////////////////////////////
void UI_AdvectionPanel::_onEnableCheck(wxCommandEvent& event)
{
   event.GetInt();
   if(_enableCheck->GetValue())
   {
      _setGroupVisibility(true);
      ((UI_Tabs *)GetParent())->cId = ADVECTION_SHADER;
      ((UI_Tabs *)GetParent())->sendDataArrayToServer();
   }else{
      _setGroupVisibility(false);
      ((UI_Tabs *)GetParent())->cId = VOLUME_SHADER;
      ((UI_Tabs *)GetParent())->sendDataArrayToServer();
   }

}
///////////////////////////////////////////////////////////////
void UI_AdvectionPanel::_onShowBBoxCheck(wxCommandEvent& event)
{
   event.GetInt();
   if(_enableBBox)
   {
      ((UI_Tabs *)GetParent())->cId = SHOW_TEXTURE_BBOX;
      ((UI_Tabs *)GetParent())->cIso_value = _enableBBox->GetValue();
      ((UI_Tabs *)GetParent())->sendDataArrayToServer();
   }
}
///////////////////////////////////////////////////////
void UI_AdvectionPanel::_onSlider(wxScrollEvent& event)
{
   event.GetInt();
   switch(event.GetId()){
      case X_DYE_POS:
      case Y_DYE_POS: 
      case Z_DYE_POS:   
         ((UI_Tabs*)GetParent())->cIso_value = DYE_TRANSLATION;
         ((UI_Tabs*)GetParent())->cSc = _xDyeLocation->GetValue();;         
         ((UI_Tabs*)GetParent())->cMin = _yDyeLocation->GetValue();
         ((UI_Tabs*)GetParent())->cMax = _zDyeLocation->GetValue();
         break;
      case MATERIAL_DENSITY:
         ((UI_Tabs*)GetParent())->cIso_value = NOISE_SCALE;
         ((UI_Tabs*)GetParent())->cSc = _noiseDensityCtrl->GetValue();         
         ((UI_Tabs*)GetParent())->cMin = _materialCBox->GetSelection();
         break;
      case MATERIAL_DECAY:
      case MATERIAL_INJECTION:
         ((UI_Tabs*)GetParent())->cIso_value = WEIGHT;
         ((UI_Tabs*)GetParent())->cSc = _decayStrengthCtrl->GetValue();
         ((UI_Tabs*)GetParent())->cMin = _injectionStrengthCtrl->GetValue();
         ((UI_Tabs*)GetParent())->cMax = _materialCBox->GetSelection();
         break;
   };
   
   ((UI_Tabs*)GetParent())->cId = ADVECTION_SHADER;
   ((UI_Tabs*)GetParent())->sendDataArrayToServer();
}
#endif
#endif