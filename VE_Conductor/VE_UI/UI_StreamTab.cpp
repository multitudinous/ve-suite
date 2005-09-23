/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: filename,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_StreamTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"

#include "VE_Xplorer/cfdEnum.h"

#include <wx/slider.h>
#include <wx/radiobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/sizer.h>

BEGIN_EVENT_TABLE(UI_StreamlineTab, wxPanel)
   EVT_RADIOBOX      ( CURSOR_SELECT_RBOX,    UI_StreamlineTab::_onDirection)
   EVT_RADIOBOX      ( DIR_RBOX,              UI_StreamlineTab::_onDirection)
   EVT_RADIOBOX      ( INTEGRATE_DIR_RBOX,    UI_StreamlineTab::_onIntegrateDir)
   EVT_BUTTON        ( COMP_STREAMLINE_BUTTON,UI_StreamlineTab::_onCompStreamline)
   EVT_BUTTON        ( PARTICLE_TRACK_BUTTON, UI_StreamlineTab::_onParticleTrack)
   EVT_CHECKBOX      ( SEED_POINTS_CHK,       UI_StreamlineTab::_onCheck)
   EVT_CHECKBOX      ( ARROW_POINTS_CHK,      UI_StreamlineTab::OnArrowCheck )
   EVT_COMMAND_SCROLL( NUM_PTS_SLIDER,        UI_StreamlineTab::_onnPointsSlider)
   EVT_COMMAND_SCROLL( SIZE_SLIDER,           UI_StreamlineTab::_onnPointsSlider)
   EVT_COMMAND_SCROLL( SPHERE_SCALE_SLIDER,   UI_StreamlineTab::onScaleSlider)
#ifdef WIN32
   EVT_COMMAND_SCROLL_ENDSCROLL(PROP_SLIDER,           UI_StreamlineTab::_onPropSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(INT_STEP_SLIDER,       UI_StreamlineTab::_oniStepSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(STEP_SLIDER,           UI_StreamlineTab::_onStepSlider)
   EVT_COMMAND_SCROLL_ENDSCROLL(DIAMETER_SLIDER,       UI_StreamlineTab::_onDiameterSlider)
#else
   EVT_COMMAND_SCROLL(PROP_SLIDER,           UI_StreamlineTab::_onPropSlider)
   EVT_COMMAND_SCROLL(INT_STEP_SLIDER,       UI_StreamlineTab::_oniStepSlider)
   EVT_COMMAND_SCROLL(STEP_SLIDER,           UI_StreamlineTab::_onStepSlider)
   EVT_COMMAND_SCROLL(DIAMETER_SLIDER,       UI_StreamlineTab::_onDiameterSlider)
#endif
END_EVENT_TABLE()

////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
UI_StreamlineTab::UI_StreamlineTab(wxNotebook* tControl)
:wxScrolledWindow(tControl, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxSUNKEN_BORDER )
{
   int nUnitX=20;
   int nUnitY=10;
   //int nPixX = 5;
   //int nPixY = 10;
   SetScrollRate(nUnitX, nUnitY);

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
   sphereScaleSlider = 0;

   _parent = tControl;
   particleControls = 0;

   _buildPage();
}

///////////////////////////////////
//Build the Streamlines Tab      //
///////////////////////////////////
void UI_StreamlineTab::_buildPage()
{
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
   wxStaticText* pLabel = new wxStaticText(this,-1,wxT("Propagation (Total) Time"));
   wxStaticText* pLabelLeft = new wxStaticText(this,-1,wxT("Shorter"));
   wxStaticText* pLabelRight = new wxStaticText(this,-1,wxT("Longer"));

   wxStaticText* iLabel = new wxStaticText(this,-1,wxT("Integration Step"));
   wxStaticText* iLabelLeft = new wxStaticText(this,-1,wxT("Smaller"));
   wxStaticText* iLabelRight = new wxStaticText(this,-1,wxT("Larger"));

   wxStaticText* sLabel = new wxStaticText(this,-1,wxT("Step"));
   wxStaticText* sLabelLeft = new wxStaticText(this,-1,wxT("Finer"));
   wxStaticText* sLabelRight = new wxStaticText(this,-1,wxT("Coarser"));

   wxStaticText* npLabel         = new wxStaticText(this,-1,wxT("Number of Points (Per Direction if Plane)"));
   wxStaticText* sizeLabel       = new wxStaticText(this,-1,wxT("Size(%)"));
   wxStaticText* scaleLabel       = new wxStaticText(this,-1,wxT("Sphere/Arrow/Particle Scale"));

   wxStaticText* diameterLabel   = new wxStaticText(this,-1,wxT("Line Diameter"));
   wxStaticText* diameterLabelLeft   = new wxStaticText(this,-1,wxT("Decrease Size"));
   wxStaticText* diameterLabelRight   = new wxStaticText(this,-1,wxT("Increase Size"));

   //the two sliders for this group
   _propSlider = new wxSlider(this, PROP_SLIDER,100,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   _iStepSlider = new wxSlider(this, INT_STEP_SLIDER,1000,1,5000,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   _stepSlider = new wxSlider(this, STEP_SLIDER,1,1,5000,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   _nPtsSlider = new wxSlider(this, NUM_PTS_SLIDER,2,2,20,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   _sizePerSlider = new wxSlider(this, SIZE_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   _diameterSlider = new wxSlider(this, DIAMETER_SLIDER,0,-100,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   sphereScaleSlider = new wxSlider(this, SPHERE_SCALE_SLIDER, 50, 1, 100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_LABELS );

   //group the sliders and the labels together
   wxBoxSizer* propGroup      = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* propGroupBottom      = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* intGroup       = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* intGroupBottom       = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* stepGroup      = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* stepGroupBottom       = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* sizePointsGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* numPointsGroup = new wxBoxSizer(wxVERTICAL);

   wxBoxSizer* diameterGroup  = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* diameterGroupBottom  = new wxBoxSizer(wxHORIZONTAL);

   wxBoxSizer* scaleGroup  = new wxBoxSizer(wxVERTICAL);

   //the prop slider
   propGroup->Add(pLabel,0,wxALIGN_LEFT);
   propGroup->Add(_propSlider,1,wxALIGN_RIGHT|wxEXPAND );
   propGroupBottom->Add(pLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   propGroupBottom->Add(pLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   propGroup->Add(propGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5);

   //the int step slider
   intGroup->Add(iLabel,0,wxALIGN_LEFT);
   intGroup->Add(_iStepSlider,1,wxALIGN_RIGHT|wxEXPAND );
   intGroupBottom->Add(iLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   intGroupBottom->Add(iLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   intGroup->Add(intGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //the step slider
   stepGroup->Add(sLabel,0,wxALIGN_LEFT);
   stepGroup->Add(_stepSlider,1,wxALIGN_RIGHT|wxEXPAND );
   stepGroupBottom->Add(sLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   stepGroupBottom->Add(sLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   stepGroup->Add(stepGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //the numPoints Slider
   numPointsGroup->Add(npLabel,0,wxALIGN_LEFT);
   numPointsGroup->Add(_nPtsSlider,1,wxALIGN_RIGHT|wxEXPAND);

   // The plane size slider
   sizePointsGroup->Add(sizeLabel,0,wxALIGN_LEFT);
   sizePointsGroup->Add(_sizePerSlider,1,wxALIGN_RIGHT|wxEXPAND);

   // The plane size slider
   scaleGroup->Add(scaleLabel,0,wxALIGN_LEFT);
   scaleGroup->Add(sphereScaleSlider,1,wxALIGN_RIGHT|wxEXPAND);

   // The streamline diameter slider
   diameterGroup->Add(diameterLabel,0,wxALIGN_LEFT);
   diameterGroup->Add(_diameterSlider,1,wxALIGN_RIGHT|wxEXPAND );
   diameterGroupBottom->Add(diameterLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   diameterGroupBottom->Add(diameterLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   diameterGroup->Add(diameterGroupBottom,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //the buttons and check box for the bottom of the UI
   _compStreamButton = new wxButton(this, COMP_STREAMLINE_BUTTON,
                                    wxT("Compute Streamlines"));
   _parTrackingButton = new wxButton(this, PARTICLE_TRACK_BUTTON,
                                    wxT("Particle Tracking"));
   _lastSeedPtChk = new wxCheckBox(this, SEED_POINTS_CHK,
                                    wxT("Use Last Seedpoints"));
   _lastSeedPtChk->SetValue( false );
   arrowPointsChk = new wxCheckBox(this, ARROW_POINTS_CHK,
                                    wxT("Stream Arrows"));
   arrowPointsChk->SetValue( false );
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
   row2->Add(arrowPointsChk,1,wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   //the radio box group
   wxBoxSizer* radioBoxGroup = new wxBoxSizer(wxVERTICAL); 
   radioBoxGroup->Add(_cursorRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);
   radioBoxGroup->Add(_directionRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);
   radioBoxGroup->Add(_integrationDirRBox,1,wxALL|wxALIGN_CENTER_HORIZONTAL|wxEXPAND,5);

   //the group boxes for the other sliders
   //The static box for the sliders
   wxStaticBox* sGroupLabel = new wxStaticBox(this, -1, wxT("Streamline Controls"));

   //need a sizer for this box
   //The items will be placed  next (vertically) to other 
   //rather than on top of each other(horizontally)
   wxStaticBoxSizer* streamControllerBoxSizer = new wxStaticBoxSizer(sGroupLabel,wxVERTICAL);
   //wxBoxSizer* streamControllerBoxSizer = new wxBoxSizer(wxVERTICAL);
   streamControllerBoxSizer->Add(propGroup,        1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(intGroup,         1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(stepGroup,        1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(sizePointsGroup,  1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(numPointsGroup,   1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(scaleGroup,       1, wxEXPAND|wxALIGN_RIGHT );
   streamControllerBoxSizer->Add(diameterGroup,    1, wxEXPAND|wxALIGN_RIGHT );

   // Add to the static box sizer
   //streamControllerBoxSizer->Add(sGroup,1,wxEXPAND|wxALL, 5);

   //now add the groups to the first row
   //row1->Add(sliderGroup,1,wxEXPAND|wxALIGN_RIGHT);
   row1->Add(streamControllerBoxSizer, 3, wxEXPAND|wxALIGN_RIGHT|wxALL, 5);
   row1->Add(radioBoxGroup, 1, wxEXPAND|wxALIGN_LEFT );
  
   //add the rows to the main panel
   streamPanelGroup->Add(row1, 1, wxALIGN_CENTER_HORIZONTAL|wxEXPAND ); 
   streamPanelGroup->Add(row2, 0, wxALIGN_CENTER_HORIZONTAL|wxEXPAND ); 

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
void UI_StreamlineTab::_oniStepSlider(wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_INT_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _iStepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

///////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onPropSlider(wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_PROPAGATION_TIME;
   ((UI_Tabs *)_parent)->cIso_value = _propSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////////////////
void UI_StreamlineTab::_onStepSlider(wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_STEP_LENGTH;
   ((UI_Tabs *)_parent)->cIso_value = _stepSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onIntegrateDir(wxCommandEvent& WXUNUSED(event))
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
void  UI_StreamlineTab::_onParticleTrack(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = ANIMATED_STREAMLINES;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   

   if ( particleControls )
   {
      delete particleControls;
      particleControls = 0;
   }

   particleControls = new UI_TransientDialog(19, this,PARTICLE_DIALOG );
   particleControls->SetTitle(wxString("Particle Controls"));
   particleControls->SetTabControl( ((UI_Tabs*)_parent) );
   particleControls->Show();
}

////////////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onCompStreamline(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = STREAMLINES;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();   
}

///////////////////////////////////////////////////////////
void  UI_StreamlineTab::_onDiameterSlider(wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = STREAMLINE_DIAMETER;
   ((UI_Tabs *)_parent)->cIso_value = _diameterSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onCheck(wxCommandEvent& WXUNUSED(event))
{
}

//////////////////////////////////////////////////////////////////////
void UI_StreamlineTab::OnArrowCheck( wxCommandEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cId = STREAMLINE_ARROW;
   ((UI_Tabs *)_parent)->cIso_value = arrowPointsChk->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onnPointsSlider(wxScrollEvent& WXUNUSED(event))
{
   ConstructCommandId();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::_onDirection( wxCommandEvent& WXUNUSED(event) )
{
   ConstructCommandId();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::onScaleSlider( wxScrollEvent& WXUNUSED(event) )
{
   ConstructCommandId();
}

///////////////////////////////////////////////////////
void  UI_StreamlineTab::ConstructCommandId( void )
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_STREAMLINE_CURSOR;
   ((UI_Tabs *)_parent)->cMin = _nPtsSlider->GetValue()-1;           
   ((UI_Tabs *)_parent)->cMax = _sizePerSlider->GetValue();
   ((UI_Tabs *)_parent)->cSc  = sphereScaleSlider->GetValue();

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
