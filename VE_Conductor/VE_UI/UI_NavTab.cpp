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
#include "VE_Conductor/VE_UI/UI_NavTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>
#include "VE_Conductor/Framework/Nav_Bitmaps/x_left.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/x_right.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/z_up.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/z_down.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/y_up.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/y_down.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/pitch_down.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/pitch_up.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/ccw_roll.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/cw_roll.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/yaw_ccw.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/yaw_cw.xpm"
#include "VE_Conductor/Framework/Nav_Bitmaps/coordinates.xpm"

#include <wx/checkbox.h>
#include <wx/slider.h>
#include <wx/button.h>
#include <wx/window.h>
#include <wx/sizer.h>

BEGIN_EVENT_TABLE(UI_NavigateScroll, wxScrolledWindow)
END_EVENT_TABLE()
UI_NavigateScroll::UI_NavigateScroll(wxWindow* parent)
:wxScrolledWindow(parent, -1, wxDefaultPosition, wxDefaultSize,
          wxHSCROLL | wxVSCROLL)
{
   int nUnitX=20;
   int nUnitY=10;
   int nPixX = 5;
   int nPixY = 10;
   SetScrollbars( nPixX, nPixY, nUnitX, nUnitY );

   //The static box for the buttons
   wxStaticBox* buttonStaticBox = new wxStaticBox(this, -1, wxT("Navigation Controls"));

   //need a sizer for this box
   //The items will be placed  next (vertically) to other 
   //rather than on top of each other(horizontally)
   wxStaticBoxSizer* buttonStaticBoxSizer = new wxStaticBoxSizer( buttonStaticBox, wxVERTICAL);

   wxBoxSizer* navCol = new wxBoxSizer(wxVERTICAL);

   wxGridSizer* topSizer = new wxGridSizer(11,4);

   //************Loading up the bitmaps
   _image1 = new wxImage(x_left_xpm);
   _image2 = new wxImage(x_right_xpm);
   _image3 = new wxImage(z_up_xpm);
   _image4 = new wxImage(z_down_xpm);
   _image5 = new wxImage(y_up_xpm);
   _image6 = new wxImage(y_down_xpm);
   _image7 = new wxImage(pitch_down_xpm);
   _image8 = new wxImage(pitch_up_xpm);
   _image9 = new wxImage(ccw_roll_xpm);
   _image10 = new wxImage(cw_roll_xpm);
   _image11 = new wxImage(yaw_ccw_xpm);
   _image12 = new wxImage(yaw_cw_xpm);
   _imagecoord = new wxImage(coordinates_xpm);

   _bitmap1 = new wxBitmap(_image1, -1);
   _bitmap2 = new wxBitmap(_image2, -1);
   _bitmap3 = new wxBitmap(_image3, -1);
   _bitmap4 = new wxBitmap(_image4, -1);
   _bitmap5 = new wxBitmap(_image5, -1);
   _bitmap6 = new wxBitmap(_image6, -1);
   _bitmap7 = new wxBitmap(_image7, -1);
   _bitmap8 = new wxBitmap(_image8, -1);
   _bitmap9 = new wxBitmap(_image9, -1);
   _bitmap10 = new wxBitmap(_image10, -1);
   _bitmap11 = new wxBitmap(_image11, -1);
   _bitmap12 = new wxBitmap(_image12, -1);
   _bitmapcoord = new wxBitmap(_imagecoord, -1);


   //Assign the bitmaps to the respective buttons
   _leftButton = new UI_NavButton(this, NAV_LEFT, wxBitmap(*_bitmap1));
   _rightButton = new UI_NavButton(this, NAV_RIGHT, wxBitmap(*_bitmap2));
   _upButton = new UI_NavButton(this, NAV_UP, wxBitmap(*_bitmap3));
   _downButton = new UI_NavButton(this, NAV_DOWN, wxBitmap(*_bitmap4));
   _forwardButton = new UI_NavButton(this, NAV_FWD, wxBitmap(*_bitmap5));
   _backButton = new UI_NavButton(this, NAV_BKWD, wxBitmap(*_bitmap6));
   _pitchdownButton = new UI_NavButton(this, PITCH_DOWN, wxBitmap(*_bitmap7));
   _pitchupButton = new UI_NavButton(this, PITCH_UP, wxBitmap(*_bitmap8));
   _rollccwButton = new UI_NavButton(this, ROLL_CCW, wxBitmap(*_bitmap9));
   _rollcwButton = new UI_NavButton(this, ROLL_CW, wxBitmap(*_bitmap10));
   _yawccwButton = new UI_NavButton(this, YAW_CCW, wxBitmap(*_bitmap11));
   _yawcwButton = new UI_NavButton(this, YAW_CW, wxBitmap(*_bitmap12));

   //Place holders to use in filling up empty holes in the grid sizer
   wxStaticText* blank1 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank2 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank3 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank4 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank5 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank6 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank7 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank8 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank9 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank10 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank11 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank12 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank13 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank14 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank15 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank16 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank17 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank18 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank19 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank20 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank21 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank22 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank23 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank24 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank25 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank26 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank27 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank28 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank29 = new wxStaticText(this, -1, ""); //just a place holder

   //The text headers for each of the three button groups
   wxStaticText* _xaxis = new wxStaticText(this, -1, "X-Axis"); 
   wxStaticText* _yaxis = new wxStaticText(this, -1, "Y-Axis"); 
   wxStaticText* _zaxis = new wxStaticText(this, -1, "Z-Axis"); 
   wxStaticText* _pitch = new wxStaticText(this, -1, "Pitch"); 
   wxStaticText* _roll = new wxStaticText(this, -1, "Roll"); 
   wxStaticText* _yaw = new wxStaticText(this, -1, "Yaw"); 

   //Pull together the text headers for proper alignment
   wxBoxSizer* topDesc1 = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* topDesc2 = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* topDesc3 = new wxBoxSizer(wxVERTICAL);
   topDesc1->Add(_xaxis,1,wxALIGN_CENTER_HORIZONTAL);
   topDesc1->Add(_pitch,1,wxALIGN_CENTER_HORIZONTAL);
   topDesc2->Add(_yaxis,1,wxALIGN_CENTER_HORIZONTAL);
   topDesc2->Add(_roll,1,wxALIGN_CENTER_HORIZONTAL);
   topDesc3->Add(_zaxis,1,wxALIGN_CENTER_HORIZONTAL);
   topDesc3->Add(_yaw,1,wxALIGN_CENTER_HORIZONTAL);

   //*******************************Filling the Gridsizer
   //first row of the grid
   topSizer->Add(blank22,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(topDesc1,1,wxALIGN_BOTTOM);
   topSizer->Add(blank23,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank24,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank25,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(topDesc2,1,wxALIGN_BOTTOM);
   topSizer->Add(blank26,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank27,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank28,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(topDesc3,1,wxALIGN_BOTTOM);
   topSizer->Add(blank29,1,wxALIGN_CENTER_HORIZONTAL);
   //first row of the grid
   topSizer->Add(blank1,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_pitchdownButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank2,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank3,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank4,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_forwardButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank5,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank6,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank7,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_upButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank8,1,wxALIGN_CENTER_HORIZONTAL);
   //second row of the grid
   topSizer->Add(_leftButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank9,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_rightButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank10,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_rollccwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank11,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_rollcwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank12,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_yawccwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank13,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_yawcwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //third row of the grid
   topSizer->Add(blank14,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_pitchupButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank15,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank16,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank17,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_backButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank18,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank19,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank20,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_downButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank21,1,wxALIGN_CENTER_HORIZONTAL);
   //***********************Done filling the gridsizer, now pull together the page

   navCol->Add(topSizer,5,wxALIGN_CENTER_HORIZONTAL|wxALL);

   wxStaticBitmap* coordpic = new wxStaticBitmap(this, -1,wxBitmap(*_imagecoord),wxDefaultPosition,
                                 wxSize(110,112),wxMINIMIZE_BOX|wxTHICK_FRAME); 
   wxGridSizer* picSizer = new wxGridSizer(1,1);
   picSizer->Add(coordpic,1,wxALIGN_CENTER_HORIZONTAL);

   // add step size sliders
   translationStepSize = new wxSlider(this, TRANS_STEP_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS);
   rotationStepSize = new wxSlider(this, ROT_STEP_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS );
   
   //the labels for the sliders 
   wxStaticText* transStepSizeLabel = new wxStaticText(this,-1,wxT("Translation Step Size"));
   wxStaticText* rotStepSizeLabel = new wxStaticText(this,-1,wxT("Rotation Step Size"));
   
   wxBoxSizer* stepSizeGroup = new wxBoxSizer(wxVERTICAL);
   stepSizeGroup->Add(transStepSizeLabel,0,wxALIGN_LEFT);
   stepSizeGroup->Add(translationStepSize,1,wxALIGN_LEFT|wxEXPAND);
   stepSizeGroup->Add(rotStepSizeLabel,0,wxALIGN_LEFT);
   stepSizeGroup->Add(rotationStepSize,1,wxALIGN_LEFT|wxEXPAND);
      
   // Misc buttons and check boxes
   wxBoxSizer* miscGroup = new wxBoxSizer(wxHORIZONTAL);
   headRotationChk = new wxCheckBox( this, HEAD_ROTATE_CHK,
                                    wxT("Rotate About Users Head"));
   headRotationChk->SetValue( true );
   miscGroup->Add( headRotationChk,1,wxALL|wxALIGN_LEFT, 5);

   resetNavPosition = new wxButton(this, RESET_NAV_POSITION,
                                    wxT("Reset Nav Position"));
   miscGroup->Add( resetNavPosition,1,wxALL|wxALIGN_LEFT, 5);
   miscGroup->Add( picSizer,1,wxALIGN_RIGHT);

   // Add everything to static box sizer
   buttonStaticBoxSizer->Add( navCol,4,wxALIGN_CENTER_HORIZONTAL);
   buttonStaticBoxSizer->Add( stepSizeGroup,2,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   buttonStaticBoxSizer->Add( miscGroup,2,wxALL|wxALIGN_LEFT, 5);

   SetSizer(buttonStaticBoxSizer);
}

///////////////////////////////////////
UI_NavigateScroll::~UI_NavigateScroll()
{
   if(_image1){
      delete _image1;
      _image1 = 0; 
   }
   if(_image2){
      delete _image2;
      _image2 = 0; 
   }
   if(_image3){
      delete _image3;
      _image3 = 0; 
   }
   if(_image4){
      delete _image4;
      _image4 = 0; 
   }
   if(_image5){
      delete _image5;
      _image5 = 0; 
   }
   if(_image6){
      delete _image6;
      _image6 = 0; 
   }
   if(_image7){
      delete _image7;
      _image7 = 0; 
   }
   if(_image8){
      delete _image8;
      _image8 = 0; 
   }
   if(_image9){
      delete _image9;
      _image9 = 0; 
   }
   if(_image10){
      delete _image10;
      _image10 = 0; 
   }
   if(_image11){
      delete _image11;
      _image11 = 0; 
   }
   if(_image12){
      delete _image12;
      _image12 = 0; 
   }
   if(_imagecoord){
      delete _imagecoord;
      _imagecoord = 0; 
   }
}



BEGIN_EVENT_TABLE(UI_NavigationTab, wxPanel)
   EVT_MOUSE_EVENTS(UI_NavigationTab::onMouse)
   EVT_COMMAND_SCROLL( TRANS_STEP_SLIDER, UI_NavigationTab::OnTransStepSlider)
   EVT_COMMAND_SCROLL( ROT_STEP_SLIDER, UI_NavigationTab::OnRotStepSlider)
   EVT_BUTTON        ( RESET_NAV_POSITION, UI_NavigationTab::OnResetNavPosition)
   EVT_CHECKBOX      ( HEAD_ROTATE_CHK,      UI_NavigationTab::OnHeadCheck )
  //EVT_LEFT_UP(UI_NavigationTab::onMouse)
END_EVENT_TABLE()

BEGIN_EVENT_TABLE(UI_NavButton, wxButton)
   //EVT_S(UI_NavButton::onMouse)
   EVT_LEFT_DOWN(UI_NavButton::onMouse)
   EVT_LEFT_UP(UI_NavButton::onMouseUp)
END_EVENT_TABLE()

////////////////////////////////////////////////////////  
UI_NavigationTab::UI_NavigationTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;

   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);

   _activeButton = NONE;

   navScroll = new UI_NavigateScroll(this);

   mainSizer->Add( navScroll,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   SetSizer( mainSizer );

   // Update VE-Xplorer data
   ((UI_Tabs *)_parent)->cId = CHANGE_TRANSLATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->translationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   ((UI_Tabs *)_parent)->cId = CHANGE_ROTATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->rotationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

   ((UI_Tabs *)_parent)->cId  = ROTATE_ABOUT_HEAD;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->headRotationChk->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
///////////////////////////////////////////////////
void UI_NavigationTab::onMouse(wxMouseEvent& mouse)
{
   std::cout<<"Mouse action from tab!!"<<std::endl;
   //if left button comes up 
   //specific button we need to 
   //tell cfdApp to stop moving 
   if(mouse.LeftUp()){
      std::cout<<"left is up from tab!"<<std::endl;
      //reset the active button
      setActiveButton(NONE);
      
      //relay info to cfdApp 
      updateParent(0,-1);
   }
   mouse.Skip();
}
////////////////////////////////////////////
//Constructors                            //
////////////////////////////////////////////
UI_NavButton::UI_NavButton(wxWindow* parent,
wxWindowID id, const wxBitmap& bitmap)
:wxBitmapButton(parent,id,bitmap,wxDefaultPosition,wxSize(35,40),wxBU_EXACTFIT)
{
  _buttonPushed = 0; 
}
///////////////////////////////////////////////////
void UI_NavButton::onMouseUp(wxMouseEvent& WXUNUSED(event))
{
   _buttonPushed = 0;
   std::cout<<"Mouse released from button: "<<GetId()<<std::endl;
   //if left button comes up 
   //specific button we need to 
   //tell cfdApp to stop moving 
   //reset the active button
   ((UI_NavigationTab*)((UI_NavigateScroll*)GetParent())->GetParent())->setActiveButton(NONE);
      
   //relay info to cfdApp 
   ((UI_NavigationTab*)((UI_NavigateScroll*)GetParent())->GetParent())->updateParent(0,-1);
}
///////////////////////////////////////////////
//only activate motion when left mouse is    //
//pressed over a specific navigation button  //          
///////////////////////////////////////////////
void UI_NavButton::onMouse(wxMouseEvent& mouse)
{
   int activeId = ((UI_NavigationTab*)((UI_NavigateScroll*)GetParent())->GetParent())->getActiveButton();

   //no button pushed yet
   if(activeId == NONE){     
      if(mouse.LeftIsDown()){
         std::cout<<"Mouse pushed on button: "<<GetId()<<std::endl;
         //set the active id to this button
         //if mouse is down
         _buttonPushed = 1;         

         //update the active button
         ((UI_NavigationTab*)((UI_NavigateScroll*)GetParent())->GetParent())->setActiveButton(GetId());

         //pass the nav info to cfdApp
         ((UI_NavigationTab*)((UI_NavigateScroll*)GetParent())->GetParent())->updateParent(_buttonPushed,GetId());              
      }
   }
}

///////////////////////////////////////////////////////
void UI_NavigationTab::updateParent(int pushed, int id)
{
   //if we released a button tell cfdApp to stop moving
   if ( !pushed )
   {
      ((UI_Tabs*)GetParent())->cId = -1;
      ((UI_Tabs*)GetParent())->cIso_value = -1;
      ((UI_Tabs*)GetParent())->sendDataArrayToServer();
   }
   else
   {
      //we pushed a button.
      //tell cfdApp to move appropriately
      ((UI_Tabs*)GetParent())->cId = GUI_NAV;
      ((UI_Tabs*)GetParent())->cIso_value = id;
      ((UI_Tabs*)GetParent())->sendDataArrayToServer();
   }
}

void UI_NavigationTab::OnTransStepSlider( wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_TRANSLATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->translationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnRotStepSlider( wxScrollEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_ROTATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->rotationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnResetNavPosition( wxCommandEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cId  = RESET_NAVIGATION_POSITION;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->translationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnHeadCheck( wxCommandEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cId  = ROTATE_ABOUT_HEAD;
   ((UI_Tabs *)_parent)->cIso_value = navScroll->headRotationChk->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
