#include "UI_StreamTab.h"
#include "UI_Tabs.h"

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

   _parent = tControl;

   _buildPage();
}

///////////////////////////////////
//Build the Streamlines Tab      //
///////////////////////////////////
void UI_StreamlineTab::_buildPage()
{
   //the box for the first group
   wxBoxSizer* sliderGroup = new wxBoxSizer(wxHORIZONTAL);

   //the labels for the sliders in the first group
   wxStaticText* npLabel = new wxStaticText(this,-1,wxT("numPts"));
   wxStaticText* sizeLabel = new wxStaticText(this,-1,wxT("Size(%)"));

   //the two sliders for this group
   wxSize slidesize(50,300);
   _nPtsSlider = new wxSlider(this, NUM_PTS_SLIDER,2,1,6,
                                wxDefaultPosition, slidesize,
                                wxSL_VERTICAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   _sizePerSlider = new wxSlider(this, SIZE_SLIDER,20,0,100,
                                wxDefaultPosition, slidesize,
                                wxSL_VERTICAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   //sizers for the two sliders and labels
   wxBoxSizer* leftGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* rightGroup = new wxBoxSizer(wxVERTICAL);

   //add the items now
   leftGroup->Add(npLabel,0,wxALIGN_CENTER_HORIZONTAL);
   leftGroup->Add(_nPtsSlider,1,wxALIGN_CENTER_HORIZONTAL);

   rightGroup->Add(sizeLabel,0,wxALIGN_CENTER_HORIZONTAL);
   rightGroup->Add(_sizePerSlider,1,wxALIGN_CENTER_HORIZONTAL);

   sliderGroup->Add(leftGroup,1,wxALIGN_LEFT|wxEXPAND);
   sliderGroup->Add(rightGroup,1,wxALIGN_RIGHT|wxEXPAND);

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
   
   //the other three sliders

   //the labels for the sliders 
   wxStaticText* pLabel = new wxStaticText(this,-1,wxT("Propag'n"));
   wxStaticText* iLabel = new wxStaticText(this,-1,wxT("In Step?"));
   wxStaticText* sLabel = new wxStaticText(this,-1,wxT("Step"));

   //the two sliders for this group
   _propSlider = new wxSlider(this, PROP_SLIDER,50,0,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_VERTICAL|
                                wxSL_RIGHT );

   _iStepSlider = new wxSlider(this, INT_STEP_SLIDER,50,0,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_VERTICAL|
                                wxSL_RIGHT );

   _stepSlider = new wxSlider(this, STEP_SLIDER,50,0,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_VERTICAL|
                                wxSL_RIGHT );

   //group the sliders and the labels together
   wxBoxSizer* propGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* intGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* stepGroup = new wxBoxSizer(wxVERTICAL);

   //the prop slider
   propGroup->Add(pLabel,0,wxALIGN_CENTER_HORIZONTAL);
   propGroup->Add(_propSlider,1,wxALIGN_CENTER_HORIZONTAL);

   //the int step slider
   intGroup->Add(iLabel,0,wxALIGN_CENTER_HORIZONTAL);
   intGroup->Add(_iStepSlider,1,wxALIGN_CENTER_HORIZONTAL);

   //the step slider
   stepGroup->Add(sLabel,0,wxALIGN_CENTER_HORIZONTAL);
   stepGroup->Add(_stepSlider,1,wxALIGN_CENTER_HORIZONTAL);

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
   row2->Add(_compStreamButton,1,wxALIGN_CENTER_HORIZONTAL);
   row2->Add(_parTrackingButton,1,wxALIGN_CENTER_HORIZONTAL);
   row2->Add(_lastSeedPtChk,1,wxALIGN_CENTER_HORIZONTAL);

   //the radio box group
   wxBoxSizer* radioBoxGroup = new wxBoxSizer(wxVERTICAL); 
   radioBoxGroup->Add(_cursorRBox,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   radioBoxGroup->Add(_directionRBox,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   radioBoxGroup->Add(_integrationDirRBox,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //the group boxes for the other sliders
   wxBoxSizer* sGroup = new wxBoxSizer(wxHORIZONTAL);
   sGroup->Add(propGroup,1,wxALIGN_LEFT|wxEXPAND);
   sGroup->Add(intGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   sGroup->Add(stepGroup,1,wxALIGN_RIGHT|wxEXPAND);

   //now add the groups to the first row
   row1->Add(sliderGroup,1,wxEXPAND|wxALIGN_LEFT);
   row1->Add(radioBoxGroup,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   row1->Add(sGroup,1,wxEXPAND|wxALIGN_RIGHT);

  
   //add the rows to the main panel
   streamPanelGroup->Add(row1,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
   streamPanelGroup->Add(row2,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(streamPanelGroup);   
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
