#include "UI_VecTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"

BEGIN_EVENT_TABLE(UI_VectorTab, wxPanel)
   EVT_BUTTON(VECTOR_UPDATE_BUTTON,       UI_VectorTab::_onUpdate)
   EVT_CHECKBOX(SCALE_VEC_MAG_CHK,        UI_VectorTab::_onCheck)
   EVT_COMMAND_SCROLL(SCALE_SLIDER,       UI_VectorTab::_onvScaleSlider)
   EVT_COMMAND_SCROLL(RATIO_SLIDER,       UI_VectorTab::_onvRatioSlider)
   EVT_COMMAND_SCROLL(MAX_THRESH_SLIDER,  UI_VectorTab::_onThresholdSlider)
   EVT_COMMAND_SCROLL(MIN_THRESH_SLIDER,  UI_VectorTab::_onThresholdSlider)
END_EVENT_TABLE()
/////////////////////////////////////////////
//Constructor                              //
/////////////////////////////////////////////
UI_VectorTab::UI_VectorTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _vThresholdMinSlider = 0;
   _vThresholdMaxSlider = 0;
   _vRatioSlider = 0;
   _vScaleSlider = 0;
   _scaleVecMagChk = 0;
   _updateButton = 0;

   _parent = tControl;

   _buildPage();
}
//////////////////////////
//Build the vector page //
//////////////////////////
void UI_VectorTab::_buildPage()
{
   //The names of the radio box choices
   //_updateButton = new wxButton(this, VECTOR_UPDATE_BUTTON, wxT("Update"));

   //Three static boxes for the sliders
   wxStaticBox* vectorThreshold = 0;
   wxStaticText* vectorRatio = 0;
   wxStaticText* vectorScale = 0;
   wxStaticBox* vectorControls = 0;

   vectorThreshold = new wxStaticBox(this,-1, wxT("Vector Threshold"));
   vectorRatio = new wxStaticText(this,-1, wxT("Vector Ratio"));
   vectorScale = new wxStaticText(this,-1, wxT("Vector Scale"));
   vectorControls = new wxStaticBox(this,-1, wxT("Vector Controls"));

   //the sliders for the threshold group

   //labels for these sliders 
   //the labels for the sliders
   wxStaticText* minLabel = new wxStaticText(this, -1, wxT("Min%"));
   wxStaticText* maxLabel = new wxStaticText(this, -1, wxT("Max%"));

   //min threshold slider
   _vThresholdMinSlider = new wxSlider(this, MIN_THRESH_SLIDER,0,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );

   //max threshold slider
   _vThresholdMaxSlider = new wxSlider(this, MAX_THRESH_SLIDER,100,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );
   
   //two sizers to group the sliders and their lables
   wxBoxSizer* minGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* maxGroup = new wxBoxSizer( wxVERTICAL );

   minGroup->Add(minLabel,0,wxALIGN_LEFT|wxEXPAND);
   minGroup->Add(_vThresholdMinSlider,1,wxALIGN_LEFT|wxEXPAND);

   maxGroup->Add(maxLabel,0,wxALIGN_LEFT|wxEXPAND);
   maxGroup->Add(_vThresholdMaxSlider,1,wxALIGN_LEFT|wxEXPAND);

   //ratio slider
   //wxSize slide2size(1, 0);

   _vRatioSlider = new wxSlider(this, RATIO_SLIDER,15,1,30,
                                wxDefaultPosition,wxDefaultSize,/* slidesize,*/
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
   wxBoxSizer* ratioGroup = new wxBoxSizer( wxVERTICAL );
   ratioGroup->Add(vectorRatio,0,wxALIGN_LEFT|wxEXPAND);
   ratioGroup->Add(_vRatioSlider,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //scale slider
   _vScaleSlider = new wxSlider(this, SCALE_SLIDER,0,-100,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|wxSL_AUTOTICKS|
                                wxSL_LABELS| wxSL_TOP|wxSL_RIGHT );
   wxBoxSizer* scaleGroup = new wxBoxSizer( wxVERTICAL );
   scaleGroup->Add(vectorScale,0,wxALIGN_LEFT|wxEXPAND);
   scaleGroup->Add(_vScaleSlider,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //check box
   _scaleVecMagChk = new wxCheckBox(this,SCALE_VEC_MAG_CHK,wxT("Scale by Vector Mag"));
   
   //the four groupings
   wxStaticBoxSizer* vThreshGroup = new wxStaticBoxSizer(vectorThreshold,wxVERTICAL);
   vThreshGroup->Add(maxGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   vThreshGroup->Add(minGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   wxStaticBoxSizer* vectorControlsGroup = new wxStaticBoxSizer(vectorControls,wxVERTICAL);
   //first column

   //second column
   vectorControlsGroup->Add(vThreshGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   //third column
   vectorControlsGroup->Add(scaleGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   //fourth column
   vectorControlsGroup->Add(ratioGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   
   //the main sizer
   wxBoxSizer* vecPanelGroup = new wxBoxSizer(wxVERTICAL);
   vecPanelGroup->Add(vectorControlsGroup, 1, wxEXPAND|wxALL, 5 );
   vecPanelGroup->Add(_scaleVecMagChk, 0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5 );
   //vListGroup->Add(_updateButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

    //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(vecPanelGroup);
   
   // Update current gui states on VE-Xplorer side
   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_MASK_RATIO;
   ((UI_Tabs *)_parent)->cIso_value = _vRatioSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   

   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_SCALE;
   ((UI_Tabs *)_parent)->cIso_value = _vScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   

   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_THRESHOLD;
   ((UI_Tabs *)_parent)->cMin       = _vThresholdMinSlider->GetValue();
   ((UI_Tabs *)_parent)->cMax       = _vThresholdMaxSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   

   ((UI_Tabs *)_parent)->cId        = SCALE_BY_VECTOR_MAGNITUDE;
   ((UI_Tabs *)_parent)->cIso_value = _vScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}
///////////////////////////////////////////////////
//Event callbacks                                // 
///////////////////////////////////////////////////

///////////////////////////////////////////////////
void UI_VectorTab::_onUpdate(wxCommandEvent& event)
{
   //((UI_Tabs *)_parent)->cId        = Y_VECTOR;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

///////////////////////////////////////////////////
void UI_VectorTab::_onvRatioSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_MASK_RATIO;
   ((UI_Tabs *)_parent)->cIso_value = _vRatioSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

///////////////////////////////////////////////////
void UI_VectorTab::_onvScaleSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_SCALE;
   ((UI_Tabs *)_parent)->cIso_value = _vScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

///////////////////////////////////////////////////
void UI_VectorTab::_onThresholdSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId        = CHANGE_VECTOR_THRESHOLD;
   ((UI_Tabs *)_parent)->cMin       = _vThresholdMinSlider->GetValue();
   ((UI_Tabs *)_parent)->cMax       = _vThresholdMaxSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

//////////////////////////////////////////////////
void UI_VectorTab::_onCheck(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId        = SCALE_BY_VECTOR_MAGNITUDE;
   ((UI_Tabs *)_parent)->cIso_value = _scaleVecMagChk->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}
