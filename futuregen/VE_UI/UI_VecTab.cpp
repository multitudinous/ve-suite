#include "UI_VecTab.h"
#include "UI_Tabs.h"

BEGIN_EVENT_TABLE(UI_VectorTab, wxPanel)
   EVT_RADIOBOX(VECTOR_RAD_BOX,           UI_VectorTab::_onUpdate)
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
   _vectorRBox = 0;
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
   ///////////////////////////////
   //Design the Vectors radiobox//
   ///////////////////////////////

   //The names of the radio box choices
   wxString vectorName[] = {wxT("default vector")};

   //Create the radio box w/ the list of scalars
   _vectorRBox = new wxRadioBox(this, VECTOR_RAD_BOX, wxT("Vectors"),
                                wxDefaultPosition, wxDefaultSize, 1,
                                                     vectorName, 1,
                                                     wxRA_SPECIFY_COLS);
   //The "Update Visualization" button
   _updateButton = new wxButton(this, VECTOR_UPDATE_BUTTON, wxT("Update"));

   //Three static boxes for the sliders
   wxStaticBox* vtGroup = 0;
   wxStaticBox* vrGroup = 0;
   wxStaticBox* vsGroup = 0;

   vtGroup = new wxStaticBox(this,-1, wxT("Vector Threshold"));
   vrGroup = new wxStaticBox(this,-1, wxT("Vector Ratio"));
   vsGroup = new wxStaticBox(this,-1, wxT("Vector Scale"));

   //the sliders for the threshold group
   //Size of the slider
   wxSize slidesize(50, 300);

   //labels for these sliders 
   //the labels for the sliders
   wxStaticText* minLabel = new wxStaticText(this, -1, wxT("Min%"));
   wxStaticText* maxLabel = new wxStaticText(this, -1, wxT("Max%"));

   //min threshold slider
   _vThresholdMinSlider = new wxSlider(this, MIN_THRESH_SLIDER,0,0,100,
                                       wxDefaultPosition, slidesize,
                                       wxSL_VERTICAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );

   //max threshold slider
   _vThresholdMaxSlider = new wxSlider(this, MAX_THRESH_SLIDER,100,0,100,
                                       wxDefaultPosition, slidesize,
                                       wxSL_VERTICAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );
   
   //two sizers to group the sliders and their lables
   wxBoxSizer* minGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* maxGroup = new wxBoxSizer(wxVERTICAL);

   minGroup->Add(minLabel,0,wxALIGN_CENTER_HORIZONTAL);
   minGroup->Add(_vThresholdMinSlider,1,wxALIGN_CENTER_HORIZONTAL);

   maxGroup->Add(maxLabel,0,wxALIGN_CENTER_HORIZONTAL);
   maxGroup->Add(_vThresholdMaxSlider,1,wxALIGN_CENTER_HORIZONTAL);

   //ratio slider
   wxSize slide2size(1, 0);

   _vRatioSlider = new wxSlider(this, RATIO_SLIDER,15,1,15,
                                wxDefaultPosition,wxDefaultSize,/* slidesize,*/
                                wxSL_VERTICAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   //scale slider
   _vScaleSlider = new wxSlider(this, SCALE_SLIDER,0,-100,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_VERTICAL| wxSL_RIGHT );

   //check box
   _scaleVecMagChk = new wxCheckBox(this,SCALE_VEC_MAG_CHK,wxT("Scale by Vector Mag"));
   
   //the four groupings
   wxBoxSizer* vListGroup = new wxBoxSizer(wxVERTICAL);
   wxStaticBoxSizer* vThreshGroup = new wxStaticBoxSizer(vtGroup,wxHORIZONTAL);
   wxStaticBoxSizer* vRatioGroup = new wxStaticBoxSizer(vrGroup,wxHORIZONTAL);
   wxStaticBoxSizer* vScaleGroup = new wxStaticBoxSizer(vsGroup,wxVERTICAL);

   //first column
   vListGroup->Add(_vectorRBox,6,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   vListGroup->Add(_updateButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //second column
   vThreshGroup->Add(minGroup,1,wxALIGN_LEFT|wxEXPAND);
   vThreshGroup->Add(maxGroup,1,wxALIGN_RIGHT|wxEXPAND);

   //third column
   vRatioGroup->Add(_vRatioSlider,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //fourth column
   vScaleGroup->Add(_vScaleSlider,6,wxALIGN_CENTER_HORIZONTAL);
   vScaleGroup->Add(_scaleVecMagChk,0,wxALIGN_CENTER_HORIZONTAL);

   
   //the main sizer
   wxBoxSizer* vecPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   vecPanelGroup->Add(vListGroup,1,wxEXPAND);
   vecPanelGroup->Add(vThreshGroup,1,wxEXPAND);
   vecPanelGroup->Add(vRatioGroup,1,wxEXPAND);
   vecPanelGroup->Add(vScaleGroup,1,wxEXPAND);

    //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(vecPanelGroup);

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
   ((UI_Tabs *)_parent)->cIso_value = _vScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}
