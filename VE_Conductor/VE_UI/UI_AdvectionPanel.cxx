#include "VE_Xplorer/cfdEnum.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/VE_UI/UI_AdvectionPanel.h"

#include <wx/string.h>
#include <wx/notebook.h>

#include <iostream>

BEGIN_EVENT_TABLE(UI_AdvectionPanel,wxPanel)
#ifdef WIN32
   EVT_COMMAND_SCROLL_ENDSCROLL(MATERIAL_DENSITY, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(MATERIAL_INJECTION, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(MATERIAL_DECAY, UI_AdvectionPanel::_onSlider)
#else
   EVT_COMMAND_SCROLL(MATERIAL_DENSITY, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL(MATERIAL_INJECTION, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL(MATERIAL_DECAY, UI_AdvectionPanel::_onSlider)
#endif
   EVT_COMMAND_SCROLL(X_DYE_POS, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL(Y_DYE_POS, UI_AdvectionPanel::_onSlider)
   EVT_COMMAND_SCROLL(Z_DYE_POS, UI_AdvectionPanel::_onSlider)
   EVT_CHECKBOX(ENABLE_CHECK,UI_AdvectionPanel::_onEnableCheck)
   EVT_CHECKBOX(BBOX_CHECK,UI_AdvectionPanel::_onShowBBoxCheck)
   EVT_COMBOBOX(MATERIAL_COMBO,UI_AdvectionPanel::_onMaterialSwitch)
END_EVENT_TABLE()

//////////////////////////////////////////////////////////
//Constructor                                           //
//////////////////////////////////////////////////////////
UI_AdvectionPanel::UI_AdvectionPanel(wxNotebook* tControl)
:wxPanel(tControl)
{
   _enableCheck =0; 
   _enableBBox=0;

   _dyeGroup=0;
   _dyeSliderBox=0;  
   _dyeInjectCheck=0;
   _xDyeLocation=0;
   _yDyeLocation=0;
   _zDyeLocation=0;

   _materialGroup=0;
   _materialCBox=0;
   _noiseDensityCtrl=0;
   _injectionStrengthCtrl=0;
   _decayStrengthCtrl=0;
   _buildPage();
}
///////////////////////////////////////
UI_AdvectionPanel::~UI_AdvectionPanel()
{
   if(_dyeGroup)
   {
      delete _dyeGroup;
      _dyeGroup = 0;
   }
   if(_materialGroup)
   {
      delete _materialGroup;
      _materialGroup = 0;
   }
   
   if(_dyeSliderBox)
   {
      delete _dyeSliderBox;
      _dyeSliderBox = 0;
   } 
   if(_dyeInjectCheck)
   {
      delete _dyeInjectCheck;
      _dyeInjectCheck = 0;
   } 
   if(_xDyeLocation)
   {
      delete _xDyeLocation;
      _xDyeLocation = 0;
   }
   if(_yDyeLocation)
   {
      delete _yDyeLocation;
      _yDyeLocation = 0;
   }
   if(_zDyeLocation)
   {
      delete _zDyeLocation;
      _zDyeLocation = 0;
   }
   if(_materialGroup)
   {
      delete _materialGroup;
      _materialGroup = 0;
   }
   if(_noiseDensityCtrl)
   {
      delete _noiseDensityCtrl;
      _noiseDensityCtrl = 0;
   }
   if(_noiseDensityCtrl)
   {
      delete _noiseDensityCtrl;
      _noiseDensityCtrl = 0;
   }
   if(_injectionStrengthCtrl)
   {
      delete _injectionStrengthCtrl;
      _injectionStrengthCtrl = 0;
   }
   if(_decayStrengthCtrl)
   {
      delete _decayStrengthCtrl;
      _decayStrengthCtrl = 0;
   }
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
   //_dyeInjectCheck = new wxCheckBox(this, DYE_CHECK, "Inject");
   wxStaticText* xLabel = new wxStaticText(this, -1, wxT("X"));
   wxStaticText* yLabel = new wxStaticText(this, -1, wxT("Y"));
   wxStaticText* zLabel = new wxStaticText(this, -1, wxT("Z"));
   
   wxBoxSizer* xSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* ySizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* zSizer = new wxBoxSizer(wxVERTICAL);
   
   _xDyeLocation = new wxSlider(this,
                             X_DYE_POS, 0, 
			                     0,100,
                             wxDefaultPosition, wxDefaultSize,
                             wxSL_VERTICAL| wxSL_AUTOTICKS|
                             wxSL_LABELS|wxSL_RIGHT);

   xSizer->Add(xLabel,0,wxALIGN_CENTER|wxEXPAND);
   xSizer->Add(_xDyeLocation,1,wxALIGN_CENTER|wxEXPAND);

   _yDyeLocation = new wxSlider(this,
                             Y_DYE_POS, 0,
                             0,100,
                             wxDefaultPosition, wxDefaultSize,
                             wxSL_VERTICAL| wxSL_AUTOTICKS|
                             wxSL_LABELS|wxSL_RIGHT );

   ySizer->Add(yLabel,0,wxALIGN_CENTER|wxEXPAND);
   ySizer->Add(_yDyeLocation,1,wxALIGN_CENTER|wxEXPAND);

   _zDyeLocation = new wxSlider(this,
                            Z_DYE_POS, 0, 
                            0,100,
                            wxDefaultPosition, wxDefaultSize,
                            wxSL_VERTICAL| wxSL_AUTOTICKS|
                            wxSL_LABELS|wxSL_RIGHT );

   zSizer->Add(zLabel,0,wxALIGN_CENTER|wxEXPAND);
   zSizer->Add(_zDyeLocation,1,wxALIGN_CENTER|wxEXPAND);

   wxStaticBoxSizer* dyeSliderSizer = new wxStaticBoxSizer(_dyeSliderBox,wxHORIZONTAL);
   dyeSliderSizer->Add(xSizer,1,wxALIGN_CENTER|wxEXPAND);
   dyeSliderSizer->Add(ySizer,1,wxALIGN_CENTER|wxEXPAND);
   dyeSliderSizer->Add(zSizer,1,wxALIGN_CENTER|wxEXPAND);

   wxStaticBoxSizer* dyeSGroupSizer = new wxStaticBoxSizer(_dyeGroup,wxVERTICAL);
   //dyeSGroupSizer->Add(_dyeInjectCheck,1,wxALIGN_CENTER|wxEXPAND);
   dyeSGroupSizer->Add(dyeSliderSizer,2,wxALIGN_CENTER|wxEXPAND);
   
   //material group
   _materialGroup = new wxStaticBox(this, MATERIAL_GROUP,"Injection Materials");
   wxString materialColors[3] = {"Red (Dye)","Gray (Smoke)","Clear (Holes)"};
   _materialCBox = new wxComboBox(this, 
                               MATERIAL_COMBO, 
                               "", 
                               wxDefaultPosition,
                               wxDefaultSize, 
                               3,materialColors, 
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
                                 wxSL_LABELS );
   nSizer->Add(nLabel,0,wxALIGN_LEFT|wxEXPAND);
   nSizer->Add(_noiseDensityCtrl,1,wxALIGN_LEFT|wxEXPAND);

   _injectionStrengthCtrl = new wxSlider(this,MATERIAL_INJECTION,
                                     20,0,100,
                                     wxDefaultPosition, wxDefaultSize,
                                     wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                                     wxSL_LABELS );
   iSizer->Add(iLabel,0,wxALIGN_LEFT|wxEXPAND);
   iSizer->Add(_injectionStrengthCtrl,1,wxALIGN_LEFT|wxEXPAND);

   _decayStrengthCtrl = new wxSlider(this,MATERIAL_DECAY,
                                  80,0,100,
                                  wxDefaultPosition, wxDefaultSize,
                                  wxSL_HORIZONTAL| wxSL_AUTOTICKS|
                                  wxSL_LABELS );

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
   wxBoxSizer* groupSizer = new wxBoxSizer(wxHORIZONTAL);

   enableSizer->Add(_enableCheck,0,wxEXPAND|wxALL);
   enableSizer->Add(_enableBBox,0,wxEXPAND|wxALL);
   groupSizer->Add(materialGroupSizer,1,wxEXPAND|wxALIGN_CENTER);
   groupSizer->Add(dyeSGroupSizer,1,wxEXPAND|wxALIGN_CENTER);
   

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
   _setDyeVisibility(onOff);
   _setMaterialVisibility(onOff);

}
/////////////////////////////////////////////////////
void UI_AdvectionPanel::_setDyeVisibility(bool onOff)
{
   if(_dyeGroup){
      _dyeGroup->Enable(onOff);
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
}
//////////////////////////////////////////////////////////
void UI_AdvectionPanel::_setMaterialVisibility(bool onOff)
{
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
void UI_AdvectionPanel::_onEnableCheck(wxCommandEvent& WXUNUSED(event))
{
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
////////////////////////////////////////////////////////////////
void UI_AdvectionPanel::_onMaterialSwitch(wxCommandEvent& WXUNUSED(event))
{
   if(_materialCBox->GetSelection() == 0)
   {
      _setDyeVisibility(true);
      _noiseDensityCtrl->Enable(false);
      _injectionStrengthCtrl->Enable(false);
   }else{
      _setDyeVisibility(false);
      _noiseDensityCtrl->Enable(true);
      _injectionStrengthCtrl->Enable(true);

   }
}
///////////////////////////////////////////////////////////////
void UI_AdvectionPanel::_onShowBBoxCheck(wxCommandEvent& WXUNUSED(event))
{
   if(_enableBBox)
   {
      ((UI_Tabs *)GetParent())->cId = SHOW_TEXTURE_BBOX;
      ((UI_Tabs *)GetParent())->cIso_value = _enableBBox->GetValue();
      ((UI_Tabs *)GetParent())->sendDataArrayToServer();
   }
}
///////////////////////////////////////////////////////
void UI_AdvectionPanel::_onSlider(wxScrollEvent& event )
{
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
