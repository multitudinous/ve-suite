#include "UI_VecTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"

BEGIN_EVENT_TABLE(UI_VectorTab, wxPanel)
   EVT_BUTTON(VECTOR_UPDATE_BUTTON,       UI_VectorTab::_onUpdate)
   EVT_CHECKBOX(SCALE_VEC_MAG_CHK,        UI_VectorTab::_onCheck)
   EVT_COMMAND_SCROLL_ENDSCROLL(SCALE_SLIDER,       UI_VectorTab::_onvScaleSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(RATIO_SLIDER,       UI_VectorTab::_onvRatioSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(MAX_THRESH_SLIDER,  UI_VectorTab::_onThresholdSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(MIN_THRESH_SLIDER,  UI_VectorTab::_onThresholdSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(WARP_SCALE_SLIDER,  UI_VectorTab::OnContourSliders)
   EVT_COMMAND_SCROLL_ENDSCROLL(CONTOUR_OPACITY_SLIDER,   UI_VectorTab::OnContourSliders)
   EVT_COMMAND_SCROLL_ENDSCROLL(CONTOUR_LOD_SLIDER,       UI_VectorTab::OnContourSliders)
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
   wxStaticBox* contourControls = 0;

   vectorThreshold = new wxStaticBox(this,-1, wxT("Vector Threshold"));

   vectorRatio = new wxStaticText(this,-1, wxT("Vector Ratio"));
   wxStaticText* vectorRatioLeft = new wxStaticText(this,-1, wxT("Dense"));
   wxStaticText* vectorRatioRight = new wxStaticText(this,-1, wxT("Scarce"));

   vectorScale = new wxStaticText(this,-1, wxT("Vector Scale"));
   wxStaticText* vectorScaleLeft = new wxStaticText(this,-1, wxT("Decrease"));
   wxStaticText* vectorScaleRight = new wxStaticText(this,-1, wxT("Increase"));

   vectorControls = new wxStaticBox(this,-1, wxT("Vector Controls"));
   contourControls = new wxStaticBox(this,-1, wxT("Contour Controls"));

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
   wxBoxSizer* ratioGroupBottom = new wxBoxSizer( wxHORIZONTAL );

   ratioGroup->Add(vectorRatio,0,wxALIGN_LEFT|wxEXPAND );
   ratioGroup->Add(_vRatioSlider,1,wxALIGN_LEFT|wxEXPAND );
   ratioGroupBottom->Add(vectorRatioLeft,6,wxALIGN_LEFT|wxEXPAND);
   ratioGroupBottom->Add(vectorRatioRight,0,wxALIGN_RIGHT|wxEXPAND);
   ratioGroup->Add(ratioGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //scale slider
   _vScaleSlider = new wxSlider(this, SCALE_SLIDER,0,-100,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   wxBoxSizer* scaleGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* scaleGroupBottom = new wxBoxSizer( wxHORIZONTAL );

   scaleGroup->Add(vectorScale,0,wxALIGN_LEFT|wxEXPAND);
   scaleGroup->Add(_vScaleSlider,1,wxALIGN_LEFT|wxEXPAND );
   scaleGroupBottom->Add(vectorScaleLeft,6,wxALIGN_LEFT|wxEXPAND);
   scaleGroupBottom->Add(vectorScaleRight,0,wxALIGN_RIGHT|wxEXPAND);
   scaleGroup->Add(scaleGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //check box
   _scaleVecMagChk = new wxCheckBox(this,SCALE_VEC_MAG_CHK,wxT("Scale by Vector Mag"));
   
   //the four groupings
   wxStaticBoxSizer* vThreshGroup = new wxStaticBoxSizer(vectorThreshold,wxVERTICAL);
   vThreshGroup->Add(maxGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   vThreshGroup->Add(minGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   wxStaticBoxSizer* vectorControlsGroup = new wxStaticBoxSizer(vectorControls,wxVERTICAL);
   //first column

   //second column
   vectorControlsGroup->Add(vThreshGroup,6,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   //third column
   vectorControlsGroup->Add(scaleGroup,3,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   //fourth column
   vectorControlsGroup->Add(ratioGroup,3,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   
   //fifth column
   vectorControlsGroup->Add(_scaleVecMagChk,0,wxALIGN_LEFT|wxALL, 5 );

   // Contour controls
   wxStaticBoxSizer* contourControlsGroup = new wxStaticBoxSizer(contourControls,wxVERTICAL);

   // first row
   wxStaticText* contourOpacityText = new wxStaticText(this,-1, wxT("Contour Opacity"));
   wxStaticText* contourOpacityTextLeft = new wxStaticText(this,-1, wxT("Transparent"));
   wxStaticText* contourOpacityTextRight = new wxStaticText(this,-1, wxT("Opaque"));

   contourOpacitySlider = new wxSlider(this, CONTOUR_OPACITY_SLIDER,100,0,100,
                                wxDefaultPosition,wxDefaultSize,/* slidesize,*/
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
   wxBoxSizer* contourOpacityGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* contourOpacityLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   contourOpacityGroup->Add(contourOpacityText,0,wxALIGN_LEFT|wxEXPAND);
   contourOpacityGroup->Add(contourOpacitySlider,1,wxALIGN_LEFT|wxEXPAND );
   contourOpacityLabelBottom->Add(contourOpacityTextLeft,6,wxALIGN_LEFT|wxEXPAND);
   contourOpacityLabelBottom->Add(contourOpacityTextRight,0,wxALIGN_RIGHT|wxEXPAND);
   contourOpacityGroup->Add(contourOpacityLabelBottom,1,wxALIGN_LEFT|wxEXPAND);

   
   // second row
   wxStaticText* warpScaleText = new wxStaticText(this,-1, wxT("Warped Contour Scale"));
   wxStaticText* warpScaleTextLeft = new wxStaticText(this,-1, wxT("Lower"));
   wxStaticText* warpScaleTextRight = new wxStaticText(this,-1, wxT("Higher"));

   wrapContourScaleSlider = new wxSlider(this, WARP_SCALE_SLIDER,50,1,100,
                                wxDefaultPosition,wxDefaultSize,/* slidesize,*/
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   wxBoxSizer* warpScaleGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* warpScaleGroupBottom = new wxBoxSizer( wxHORIZONTAL );

   warpScaleGroup->Add(warpScaleText,0,wxALIGN_LEFT|wxEXPAND);
   warpScaleGroup->Add(wrapContourScaleSlider,1,wxALIGN_LEFT|wxEXPAND );
   warpScaleGroupBottom->Add(warpScaleTextLeft,6,wxALIGN_LEFT|wxEXPAND);
   warpScaleGroupBottom->Add(warpScaleTextRight,0,wxALIGN_RIGHT|wxEXPAND);
   warpScaleGroup->Add(warpScaleGroupBottom,1,wxALIGN_LEFT|wxEXPAND);

   // third row
   wxStaticText* lodText = new wxStaticText(this,-1, wxT("Contour LOD"));
   wxStaticText* lodTextLeft = new wxStaticText(this,-1, wxT("Lower"));
   wxStaticText* lodTextRight = new wxStaticText(this,-1, wxT("Higher"));
   
   contourLODSlider = new wxSlider(this, CONTOUR_LOD_SLIDER,0,0,99,
                                wxDefaultPosition,wxDefaultSize,/* slidesize,*/
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   wxBoxSizer* lodGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* lodLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   lodGroup->Add(lodText,0,wxALIGN_LEFT|wxEXPAND);
   lodGroup->Add(contourLODSlider,1,wxALIGN_LEFT|wxEXPAND );  
   lodLabelBottom->Add(lodTextLeft,6,wxALIGN_LEFT|wxEXPAND);
   lodLabelBottom->Add(lodTextRight,0,wxALIGN_RIGHT|wxEXPAND);
   lodGroup->Add(lodLabelBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );   

   // Add to contour static sizer
   contourControlsGroup->Add(contourOpacityGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   contourControlsGroup->Add(warpScaleGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );
   contourControlsGroup->Add(lodGroup,1,wxEXPAND|wxALIGN_LEFT|wxALL, 5 );

   //the main sizer
   wxBoxSizer* vecPanelGroup = new wxBoxSizer(wxVERTICAL);
   vecPanelGroup->Add(vectorControlsGroup, 3, wxEXPAND|wxALL, 5 );
   vecPanelGroup->Add(contourControlsGroup, 2,wxEXPAND|wxALL, 5 );
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

   ((UI_Tabs *)_parent)->cId        = CHANGE_CONTOUR_SETTINGS;
   ((UI_Tabs *)_parent)->cIso_value = contourOpacitySlider->GetValue();
   ((UI_Tabs *)_parent)->cMin       = wrapContourScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->cMax       = contourLODSlider->GetValue();
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

//////////////////////////////////////////////////
void UI_VectorTab::OnContourSliders( wxScrollEvent& event )
{
   ((UI_Tabs *)_parent)->cId        = CHANGE_CONTOUR_SETTINGS;
   ((UI_Tabs *)_parent)->cIso_value = contourOpacitySlider->GetValue();
   ((UI_Tabs *)_parent)->cMin       = wrapContourScaleSlider->GetValue();
   ((UI_Tabs *)_parent)->cMax       = contourLODSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

