#include "UI_StreamTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"

BEGIN_EVENT_TABLE(UI_StreamlineTab, wxPanel)
   EVT_RADIOBOX      (CURSOR_SELECT_RBOX,    UI_StreamlineTab::_onDirection)
   EVT_RADIOBOX      (DIR_RBOX,              UI_StreamlineTab::_onDirection)
   EVT_RADIOBOX      (INTEGRATE_DIR_RBOX,    UI_StreamlineTab::_onIntegrateDir)
   EVT_BUTTON        (COMP_STREAMLINE_BUTTON,UI_StreamlineTab::_onCompStreamline)
   EVT_BUTTON        (PARTICLE_TRACK_BUTTON, UI_StreamlineTab::_onParticleTrack)
   EVT_CHECKBOX      (SEED_POINTS_CHK,       UI_StreamlineTab::_onCheck)
   EVT_COMMAND_SCROLL(NUM_PTS_SLIDER,        UI_StreamlineTab::_onnPointsSlider)
   EVT_COMMAND_SCROLL(SIZE_SLIDER,           UI_StreamlineTab::_onnPointsSlider)
   EVT_COMMAND_SCROLL(PROP_SLIDER,           UI_StreamlineTab::_onPropSlider)
   EVT_COMMAND_SCROLL(INT_STEP_SLIDER,       UI_StreamlineTab::_oniStepSlider)
   EVT_COMMAND_SCROLL(STEP_SLIDER,           UI_StreamlineTab::_onStepSlider)
   EVT_COMMAND_SCROLL(DIAMETER_SLIDER,       UI_StreamlineTab::_onDiameterSlider)
END_EVENT_TABLE()

////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
UI_StreamlineTab::UI_StreamlineTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _propSlider = 0;
   _iStepSlider = 0;
   _stepSlider = 0;
   _nPtsSlider = 0;
   _sizePerSlider = 0;
   _cursorRBox = 0;
   _directionRBox = 0;
   _integrationDirRBox = 0;
   _compStreamButton = 0;
   _parTrackingButton = 0;
   _lastSeedPtChk = 0;
   _diameterSlider = NULL;

   _parent = tControl;

   _buildPage();
}

///////////////////////////////////
//Build the Streamlines Tab      //
///////////////////////////////////
void UI_StreamlineTab::_buildPage()
{
   //the box for the first group
   //wxBoxSizer* sliderGroup = new wxBoxSizer(wxHORIZONTAL);

   //the two sliders for this group
/*   wxSize slidesize(300,50);
   _nPtsSlider = new wxSlider(this, NUM_PTS_SLIDER,2,1,10,
                                wxDefaultPosition, slidesize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _sizePerSlider = new wxSlider(this, SIZE_SLIDER,20,0,100,
                                wxDefaultPosition, slidesize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
*/
   //sizers for the two sliders and labels
   //wxBoxSizer* leftGroup = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* rightGroup = new wxBoxSizer(wxVERTICAL);

   //add the items now
   /*leftGroup->Add(npLabel,0,wxALIGN_CENTER_HORIZONTAL);
   leftGroup->Add(_nPtsSlider,1,wxALIGN_CENTER_HORIZONTAL);

   rightGroup->Add(sizeLabel,0,wxALIGN_CENTER_HORIZONTAL);
   rightGroup->Add(_sizePerSlider,1,wxALIGN_CENTER_HORIZONTAL);

   sliderGroup->Add(leftGroup,1,wxALIGN_LEFT|wxEXPAND);
   sliderGroup->Add(rightGroup,1,wxALIGN_RIGHT|wxEXPAND);*/

   //create the 3 radio boxes
   //cursor radio box
   wxString cursorName[] = {wxT("none"),
                            wxT("point"),
                            wxT("line"),
                            wxT("plane")};

   _cursorRBox = new wxRadioBox(this,CURSOR_SELECT_RBOX,
                                wxT("Cursor Selection"),
                                wxDefaultPosition, wxDefaultSize,
                                4, cursorName, 1,
                                wxRA_SPECIFY_COLS);
   //direction radio box
   wxString dirName[] = {wxT("X"),
                         wxT("Y"),
                         wxT("Z")};

   _directionRBox = new wxRadioBox(this,DIR_RBOX,
                                wxT("Direction"),
                                wxDefaultPosition, wxDefaultSize,
                                3, dirName, 1,
                                wxRA_SPECIFY_COLS);

   //integration direction radio box
   wxString dirIntegrateName[] = {wxT("backward"),
                                  wxT("forward"),
                                  wxT("both directions")};

   _integrationDirRBox = new wxRadioBox(this,INTEGRATE_DIR_RBOX,
                                wxT("Integration Direction"),
                                wxDefaultPosition, wxDefaultSize,
                                3, dirIntegrateName, 1,
                                wxRA_SPECIFY_COLS);
   
   //The static box for the sliders
   wxStaticBox* sGroupLabel = new wxStaticBox(this, -1, wxT("Streamline Controls"));

   //need a sizer for this box
   //The items will be placed  next (vertically) to other 
   //rather than on top of each other(horizontally)
   wxStaticBoxSizer* streamControllerBoxSizer = new wxStaticBoxSizer(sGroupLabel,wxVERTICAL);

   //the other three sliders

   //the labels for the sliders 
   wxStaticText* pLabel = new wxStaticText(this,-1,wxT("Propagation Time"));
   wxStaticText* iLabel = new wxStaticText(this,-1,wxT("Integration Step"));
   wxStaticText* sLabel = new wxStaticText(this,-1,wxT("Step"));
   wxStaticText* npLabel         = new wxStaticText(this,-1,wxT("Number of Points"));
   wxStaticText* sizeLabel       = new wxStaticText(this,-1,wxT("Size(%)"));
   wxStaticText* diameterLabel   = new wxStaticText(this,-1,wxT("Line Diameter"));

   //the two sliders for this group
   _propSlider = new wxSlider(this, PROP_SLIDER,100,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _iStepSlider = new wxSlider(this, INT_STEP_SLIDER,100,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _stepSlider = new wxSlider(this, STEP_SLIDER,1,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _nPtsSlider = new wxSlider(this, NUM_PTS_SLIDER,2,1,20,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _sizePerSlider = new wxSlider(this, SIZE_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _diameterSlider = new wxSlider(this, DIAMETER_SLIDER,0,-100,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   //group the sliders and the labels together
   wxBoxSizer* propGroup      = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* intGroup       = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* stepGroup      = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* sizePointsGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* numPointsGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* diameterGroup  = new wxBoxSizer(wxVERTICAL);

   //the prop slider
   propGroup->Add(pLabel,0,wxALIGN_LEFT);
   propGroup->Add(_propSlider,1,wxALIGN_LEFT|wxEXPAND);

   //the int step slider
   intGroup->Add(iLabel,0,wxALIGN_LEFT);
   intGroup->Add(_iStepSlider,1,wxALIGN_LEFT|wxEXPAND);

   //the step slider
   stepGroup->Add(sLabel,0,wxALIGN_LEFT);
   stepGroup->Add(_stepSlider,1,wxALIGN_LEFT|wxEXPAND);

   //the numPoints Slider
   numPointsGroup->Add(npLabel,0,wxALIGN_LEFT);
   numPointsGroup->Add(_nPtsSlider,1,wxALIGN_LEFT|wxEXPAND);

   // The plane size slider
   sizePointsGroup->Add(sizeLabel,0,wxALIGN_LEFT);
   sizePointsGroup->Add(_sizePerSlider,1,wxALIGN_LEFT|wxEXPAND);

   // The streamline diameter slider
   diameterGroup->Add(diameterLabel,0,wxALIGN_LEFT);
   diameterGroup->Add(_diameterSlider,1,wxALIGN_LEFT|wxEXPAND);

   //the buttons and check box for the bottom of the UI
   _compStreamButton = new wxButton(this, COMP_STREAMLINE_BUTTON,
                                    wxT("Compute Streamlines"));
   _parTrackingButton = new wxButton(this, PARTICLE_TRACK_BUTTON,
                                    wxT("Particle Tracking"));
   _lastSeedPtChk = new wxCheckBox(this, SEED_POINTS_CHK,
                                    wxT("Use Last Seedpoints"));
   //the layout
   //heirarchy
   //main group
   //   row 1
   //        5 groups - slider group, radio box group, 3 sliders
   //   row 2    
   //        1 group - 3 buttons 
   
   //the main group
   wxBoxSizer* streamPanelGroup = new wxBoxSizer(wxVERTICAL);  

   //the two rows
   wxBoxSizer* row1 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row2 = new wxBoxSizer(wxHORIZONTAL);

   //we can add the buttons to the last row now 
   row2->Add(_compStreamButton,1,wxALL|wxALIGN_CENTER_HORIZONTAL,5);
   row2->Add(_parTrackingButton,1,wxALL|wxALIGN_CENTER_HORIZONTAL,5);
   row2->Add(_lastSeedPtChk,1,wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   //the radio box group
   wxBoxSizer* radioBoxGroup = new wxBoxSizer(wxVERTICAL); 
   radioBoxGroup->Add(_cursorRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);
   radioBoxGroup->Add(_directionRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);
   radioBoxGroup->Add(_integrationDirRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);

   //the group boxes for the other sliders
   wxBoxSizer* sGroup = new wxBoxSizer(wxVERTICAL);
   sGroup->Add(propGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   sGroup->Add(intGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   sGroup->Add(stepGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   sGroup->Add(sizePointsGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   sGroup->Add(numPointsGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   sGroup->Add(diameterGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);

   // Add to the static box sizer
   streamControllerBoxSizer->Add(sGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);

   //now add the groups to the first row
   //row1->Add(sliderGroup,1,wxEXPAND|wxALIGN_RIGHT);
   row1->Add(radioBoxGroup,1,wxEXPAND|wxALIGN_LEFT,5);
   row1->Add(streamControllerBoxSizer,2,wxALL|wxEXPAND|wxALIGN_RIGHT,5);

  
   //add the rows to the main panel
   streamPanelGroup->Add(row1,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5); 
   streamPanelGroup->Add(row2,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5); 

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(streamPanelGroup);   
   
   // Send intial data to VE-Xplorer
   this->ConstructCommandId();
   
   ((UI_Tabs *)_parent)->cId  = CHANGE_INT_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _iStepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

   ((UI_Tabs *)_parent)->cId  = CHANGE_PROPAGATION_TIME;
   ((UI_Tabs *)_parent)->cIso_value = _propSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

   ((UI_Tabs *)_parent)->cId  = CHANGE_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _stepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

   if ( _integrationDirRBox->GetSelection() == 0 )
   {
      ((UI_Tabs *)_parent)->cId = BACKWARD_INTEGRATION;
   }
   else if ( _integrationDirRBox->GetSelection() == 1 )
   {
      ((UI_Tabs *)_parent)->cId = FORWARD_INTEGRATION;
   }
   else if ( _integrationDirRBox->GetSelection() == 2 )
   {
      ((UI_Tabs *)_parent)->cId = TWO_DIRECTION_INTEGRATION;
   }
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   

   ((UI_Tabs *)_parent)->cId  = STREAMLINE_DIAMETER;
   ((UI_Tabs *)_parent)->cIso_value = _diameterSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
//////////////////////
//Event handling    //
//////////////////////

//////////////////////////////////////////////////////////////
void UI_StreamlineTab::_oniStepSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_INT_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _iStepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

///////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onPropSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_PROPAGATION_TIME;
   ((UI_Tabs *)_parent)->cIso_value = _propSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////////////////
void UI_StreamlineTab::_onStepSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _stepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onIntegrateDir(wxCommandEvent& event)
{
   if ( _integrationDirRBox->GetSelection() == 0 )
   {
      ((UI_Tabs *)_parent)->cId = BACKWARD_INTEGRATION;
   }
   else if ( _integrationDirRBox->GetSelection() == 1 )
   {
      ((UI_Tabs *)_parent)->cId = FORWARD_INTEGRATION;
   }
   else if ( _integrationDirRBox->GetSelection() == 2 )
   {
      ((UI_Tabs *)_parent)->cId = TWO_DIRECTION_INTEGRATION;
   }
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

//////////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onParticleTrack(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = ANIMATED_STREAMLINES;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

////////////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onCompStreamline(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = STREAMLINES;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

///////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onDiameterSlider(wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId  = STREAMLINE_DIAMETER;
   ((UI_Tabs *)_parent)->cIso_value = _diameterSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onCheck(wxCommandEvent& event)
{
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onnPointsSlider(wxScrollEvent& event)
{
   ConstructCommandId();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onDirection( wxCommandEvent& event )
{
   ConstructCommandId();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::ConstructCommandId( void )
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_STREAMLINE_CURSOR;
   ((UI_Tabs *)_parent)->cMin = _nPtsSlider->GetValue();
   ((UI_Tabs *)_parent)->cMax = _sizePerSlider->GetValue();

   if ( _cursorRBox->GetSelection() == 0 )
   {
      ((UI_Tabs *)_parent)->cIso_value = NO_CURSOR;
   }
   else if ( _cursorRBox->GetSelection() == 1 )
   {   
      ((UI_Tabs *)_parent)->cIso_value = POINT_CURSOR;
   }
   else if ( _cursorRBox->GetSelection() == 2 )
   {

      if ( _directionRBox->GetSelection() == 0 )
      {
         ((UI_Tabs *)_parent)->cIso_value = X_LINE_CURSOR;
      }
      else if ( _directionRBox->GetSelection() == 1 )
      {
         ((UI_Tabs *)_parent)->cIso_value = Y_LINE_CURSOR;
      }
      else if ( _directionRBox->GetSelection() == 2 )
      {
         ((UI_Tabs *)_parent)->cIso_value = Z_LINE_CURSOR;
      }
   }
   else if ( _cursorRBox->GetSelection() == 3 )
   {

      if ( _directionRBox->GetSelection() == 0 )
      {
         ((UI_Tabs *)_parent)->cIso_value = X_PLANE_CURSOR;
      }
      else if ( _directionRBox->GetSelection() == 1 )
      {
         ((UI_Tabs *)_parent)->cIso_value = Y_PLANE_CURSOR;
      }
      else if ( _directionRBox->GetSelection() == 2 )
      {
         ((UI_Tabs *)_parent)->cIso_value = Z_PLANE_CURSOR;
      }
   }
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
