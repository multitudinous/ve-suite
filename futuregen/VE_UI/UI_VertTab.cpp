#include "UI_VertTab.h"
#include "UI_Tabs.h"

BEGIN_EVENT_TABLE(UI_VertTab, wxPanel)
   EVT_RADIOBOX      (PARTICLE_OPTIONS_RBOX,    UI_VertTab::_onParticleOption)
   EVT_BUTTON        (DISPLAY_PARTICLE_BUTTON,  UI_VertTab::_onDisplayParticle)
   EVT_COMMAND_SCROLL(SPHERE_POINT_SIZE_SLIDER, UI_VertTab::_onSpherePointSizeSlider)
END_EVENT_TABLE()

////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
UI_VertTab::UI_VertTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _particleOptionRBox = 0;
   _displayParticlesButton = 0;
   _spherePointSizeSlider = 0;   
   _parent = tControl;
   _buildPage();
}

///////////////////////////////////
//Build the Vertex Data Tab      //
///////////////////////////////////
void UI_VertTab::_buildPage()
{
   //the box for the first group
   wxBoxSizer* sliderGroup = new wxBoxSizer(wxHORIZONTAL);

   //the labels for the Particle Option Slider
   wxStaticText* sliderLabel = new wxStaticText(this,-1,wxT("Sphere/Point Size"));

   //making the slider
   wxSize slidesize(150,300);
   _spherePointSizeSlider = new wxSlider(this, NUM_PTS_SLIDER,50,0,100,
                                wxDefaultPosition, slidesize,
                                wxSL_VERTICAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );

   //sizers for the slider and label
   wxBoxSizer* leftGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* rightGroup = new wxBoxSizer(wxVERTICAL);

   //add the slider and slider label now
   leftGroup->Add(sliderLabel,0,wxALIGN_CENTER_HORIZONTAL);
   leftGroup->Add(_spherePointSizeSlider,1,wxALIGN_CENTER_HORIZONTAL);

   sliderGroup->Add(leftGroup,1,wxALIGN_LEFT|wxEXPAND);
   sliderGroup->Add(rightGroup,1,wxALIGN_RIGHT|wxEXPAND);

   //create the radio box for the Particle Options
   //Particle Option radio box
   wxString particleOption[] = {wxT("View as a point cloud"),
                            wxT("View as variably sized spheres")};

   _particleOptionRBox = new wxRadioBox(this,CURSOR_SELECT_RBOX,
                                wxT("Cursor Selection"),
                                wxDefaultPosition, wxDefaultSize,
                                2, particleOption, 1,
                                wxRA_SPECIFY_COLS);

   //the buttons and check box for the bottom of the UI
   _displayParticlesButton = new wxButton(this, COMP_STREAMLINE_BUTTON,
                                    wxT("Display Particles"));

   //group the sliders and the labels together
   wxBoxSizer* propGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* intGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* stepGroup = new wxBoxSizer(wxVERTICAL);

   ////////////////////////////////////////////////////////////////////
   //the layout                                                      //
   //heirarchy                                                       //
   //main group                                                      //
   //   row 1                                                        //
   //        2 groups - slider group, radio box group                //
   //   row 2                                                        // 
   //        1 group - 1 button                                      //
   ////////////////////////////////////////////////////////////////////

   //the main group
   wxBoxSizer* streamPanelGroup = new wxBoxSizer(wxVERTICAL);  

   //the two rows
   wxBoxSizer* row1 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row2 = new wxBoxSizer(wxHORIZONTAL);

   //we can add the button to the last row now 
   row2->Add(_displayParticlesButton,1,wxALIGN_CENTER_HORIZONTAL);

   //the radio box group
   wxBoxSizer* radioBoxGroup = new wxBoxSizer(wxVERTICAL); 
   radioBoxGroup->Add(_particleOptionRBox,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //now add the groups to the first row
   row1->Add(sliderGroup,1,wxEXPAND|wxALIGN_LEFT);
   row1->Add(radioBoxGroup,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
  
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
void UI_VertTab::_onParticleOption(wxCommandEvent& event)
{
/*
   ((UI_Tabs *)_parent)->cId  = CHANGE_INT_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _iStepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
*/
}

///////////////////////////////////////////////////////////
void  UI_VertTab::_onDisplayParticle(wxCommandEvent& event)
{
/*
   ((UI_Tabs *)_parent)->cId  = CHANGE_PROPAGATION_TIME;
   ((UI_Tabs *)_parent)->cIso_value = _propSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
*/
}

//////////////////////////////////////////////////////////////
void UI_VertTab::_onSpherePointSizeSlider(wxScrollEvent& event)
{
/*
   ((UI_Tabs *)_parent)->cId  = CHANGE_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _stepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
*/
}
/*
///////////////////////////////////////////////////////
void  UI_VertTab::ConstructCommandId( void )
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
*/
