/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: UI_VisTab.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Conductor/VE_UI/UI_VisTab.h"
#include "VE_Xplorer/cfdEnum.h"
#include "VE_Conductor/VE_UI/UI_Frame.h"
#include "VE_Conductor/VE_UI/UI_ModelData.h"
#include "VE_Conductor/VE_UI/UI_TransientDialog.h"
#include <iostream>

#include <wx/radiobut.h>
#include <wx/radiobox.h>
#include <wx/checkbox.h>
#include <wx/button.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/textctrl.h>
#include <wx/slider.h>
#include <wx/string.h>
#include <wx/notebook.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>

BEGIN_EVENT_TABLE(UI_VisualizationTab, wxPanel)
  EVT_RADIOBOX    (CATEGORY_RAD_BOX,         UI_VisualizationTab::_onCategory)
  EVT_RADIOBOX    (CONTOUR_RAD_BOX,          UI_VisualizationTab::_onContour)
  EVT_RADIOBOX    (DIRECTION_RBOX,           UI_VisualizationTab::_onDirection)
  EVT_RADIOBUTTON (PRE_COMP_SURF_BUTTON,     UI_VisualizationTab::_onPreComp)
  EVT_RADIOBUTTON (SINGLE_PLANE_BUTTON,      UI_VisualizationTab::_onSingle)
  EVT_CHECKBOX    (NEAREST_PLANE_CHECK_BOX,  UI_VisualizationTab::_onNearest)
  EVT_BUTTON      (UPDATE_BUTTON,            UI_VisualizationTab::_onUpdate)
  EVT_CHECKBOX    (SCALAR_BAR_CHECK_BOX,     UI_VisualizationTab::_onScalarBar)
  EVT_BUTTON      (RECORD_BUTTON,            UI_VisualizationTab::_onRecord)
  EVT_BUTTON      (EXIT_BUTTON,              UI_VisualizationTab::_onExit)
  EVT_BUTTON      (CLEAR_BUTTON,             UI_VisualizationTab::_onClear)
  EVT_BUTTON      (CUSTOM_VIS_BUTTON,        UI_VisualizationTab::_onCustomVis)
  EVT_CHECKBOX    (CFD_VIS_OPTION,           UI_VisualizationTab::_onTextureBasedVisual)
  EVT_CHECKBOX    (MIRROR_CHECK_BOX,         UI_VisualizationTab::_onMirrorVisualization)
  EVT_CHECKBOX    (TRANSIENT_CHECK_BOX,      UI_VisualizationTab::_onTransientChecked)
  EVT_CHECKBOX    (GEOM_PICK_CBOX,           UI_VisualizationTab::_onGeometryPickingChecked)
  EVT_COMMAND_SCROLL(VIS_SLIDER,UI_VisualizationTab::_onSlider)
END_EVENT_TABLE()

/////////////////////////////////////////////////////////////
//Constructor                                              //
/////////////////////////////////////////////////////////////
UI_VisualizationTab::UI_VisualizationTab(wxNotebook* tControl)
:wxScrolledWindow(tControl, -1, wxDefaultPosition, wxDefaultSize,
		    wxHSCROLL | wxVSCROLL | wxSUNKEN_BORDER )

{
   int nUnitX=20;
   int nUnitY=10;
   //int nPixX = 5;
   //int nPixY = 10;
   SetScrollRate(nUnitX, nUnitY);

   //initialize things
   _categoryRBox = 0;
   _contourRBox = 0;
   _directionRBox = 0;
   _pcsButton = 0;
   _spButton = 0;
   _cycleCBox = 0;
   _nearestCBox = 0;
   _slider = 0;
   _sliderUpdate = 0;
   _scalarBarCBox = 0;
   _recordButton = 0;
   _clearButton = 0;
   _exitButton = 0;
   _transientCheckBox = NULL;
   _visOptionCheckBox = 0;
   _transientControls = 0;
   _parent = tControl;

   //build the page
   _buildPage();
}
/////////////////////
//build the visPage//
/////////////////////
void UI_VisualizationTab::_buildPage()
{
   //create the panel for the visualization tab

   //////////////////////////////////
   //Design the "Category" radiobox//
   //////////////////////////////////

   //The names of the radio box choices
   wxString category[] = { wxT("Contour"),
                           wxT("Warped Contour"),
                           wxT("Vector"),
                           wxT("Isosurface"),
                           wxT("PIV_Image"),
                           wxT("Animated Contours"),
                           wxT("Polydata"),
                           wxT("Warped Polydata") };

   //Create the 3x2 box
   _categoryRBox = new wxRadioBox(this, CATEGORY_RAD_BOX, wxT("Category"),
                                  wxDefaultPosition, wxDefaultSize, 8,
                                  category, 3, wxRA_SPECIFY_COLS);

   /////////////////////////////////////
   //Design the Contour Type radio box//
   /////////////////////////////////////

   //The names of the radio box choices
   wxString conType[] = {wxT("Graduated"), wxT("Banded"), wxT("Lined")};

   //Create a vertical radio box
   _contourRBox = new wxRadioBox(this, CONTOUR_RAD_BOX, wxT("Contour Type"),
                                 wxDefaultPosition, wxDefaultSize,3,
                                 conType, 1, wxRA_SPECIFY_COLS);

   // Initialize parameters on cfdApp side
   ((UI_Tabs *)_parent)->cId = CHANGE_CONTOUR_FILL;
   ((UI_Tabs *)_parent)->cIso_value = 0;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

   //////////////////////////////////
   //Design the Direction radio box//
   //////////////////////////////////

   //The names of the radio box choices
   wxString direction[] = {wxT("X"), wxT("Y"), wxT("Z"), wxT("By wand")};

   //Create a vertical radio box
   _directionRBox = new wxRadioBox(this, DIRECTION_RBOX, wxT("Direction"),
                                   wxDefaultPosition, wxDefaultSize, 4,
                                   direction, 1, wxRA_SPECIFY_COLS);

   /////////////////////////////
   //Design the type Static box//
   /////////////////////////////

   //A static box to group the type choices
   wxStaticBox* typebox = new wxStaticBox(this, -1, wxT("Type"));

   //This box needs it's own grouping/size control since the button/boxes in it
   //aren't grouped together in a single control.
   wxStaticBoxSizer* typebox_sizer = new wxStaticBoxSizer(typebox, wxVERTICAL);

   //Precomputed surfaces button
   _pcsButton = new wxRadioButton(this, PRE_COMP_SURF_BUTTON,
                                  wxT("All precomputed surfaces"),
                                  wxDefaultPosition, wxDefaultSize, wxRB_GROUP);
  
   //Single plane radio button
   _spButton = new wxRadioButton(this, SINGLE_PLANE_BUTTON,
                                 wxT("Specify a single plane"));

   //Cycle precomputed surfaces check box
   _cycleCBox = new wxCheckBox(this, CYCLE_CHECK_BOX,
                               wxT("Cycle precomputed surfaces"));

   //Only enable if precomputed surfaces radio box is chosen
   //This is handled in the event callback for the cycleCBox
   _cycleCBox->Enable(false);
    
   //Nearest precompute plane check box
   _nearestCBox = new wxCheckBox(this, NEAREST_PLANE_CHECK_BOX,
                                 wxT("Use nearest precomputed plane"));
   _nearestCBox->Enable(false);

   //Place the buttons in our static box
   typebox_sizer->Add(_pcsButton, 1, wxALIGN_LEFT);
   typebox_sizer->Add(_cycleCBox, 0, wxALIGN_LEFT);
   typebox_sizer->Add(_spButton, 1, wxALIGN_LEFT);
   typebox_sizer->Add(_nearestCBox, 0, wxALIGN_LEFT);

   /////////////////////////
   //Now design the slider//
   /////////////////////////

   //Size of the slider
   wxSize slidesize(300, 50);

   //The horizontal slider
   _slider = new wxSlider(this, VIS_SLIDER, 0, 0, 100,
                          wxDefaultPosition, slidesize,
                          wxSL_HORIZONTAL|wxSL_LABELS);

   //A button to update info after UI input changes have been made
   _sliderUpdate = new wxButton(this, UPDATE_BUTTON, wxT("Update"));

   //Now layout the UI. There are basically 5 rows of controls.

   //The grouping for all controls
   wxSizer* visPanelGroup = new wxBoxSizer(wxVERTICAL);

   //The first row of controls contains two radio boxes
   wxBoxSizer* firstRow = new wxBoxSizer(wxHORIZONTAL);

   //catergory radio box
   firstRow->Add( _categoryRBox, 3, wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //contour type radio box
   firstRow->Add( _contourRBox, 1, wxALIGN_RIGHT|wxEXPAND|wxALL, 5 );

   //The second row of controls contains the direction and type boxes
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);

   //direction box
   secondRow->Add( _directionRBox, 1, wxALIGN_LEFT|wxEXPAND|wxALL, 5 );

   //The static box group we created ("Type")
   secondRow->Add( typebox_sizer, 4 ,wxALIGN_RIGHT|wxEXPAND|wxALL, 5 );

   //The slider and the update button belong in the third row
   wxBoxSizer* thirdRow = new wxBoxSizer(wxHORIZONTAL);

   thirdRow->Add(_slider,5, wxALIGN_LEFT|wxALIGN_CENTER_VERTICAL);
   //thirdRow->Add(_sliderUpdate,0,wxALIGN_RIGHT|wxALIGN_CENTER_VERTICAL);

   //This is the fourth row of buttons
   wxBoxSizer* forthRow = new wxBoxSizer(wxHORIZONTAL);
   _visOptionCheckBox = new wxCheckBox(this,CFD_VIS_OPTION,wxT("Texture Based Visualization"));
   _visOptionCheckBox->SetValue(false);
   forthRow->Add(_visOptionCheckBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,8);

   _transientCheckBox = new wxCheckBox(this, TRANSIENT_CHECK_BOX, wxT("Transient"));
   _transientCheckBox->SetValue(false);
   forthRow->Add(_transientCheckBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _scalarBarCBox = new wxCheckBox(this, SCALAR_BAR_CHECK_BOX, wxT("Scalar Bar"));
   _scalarBarCBox->SetValue(true);
   forthRow->Add(_scalarBarCBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   mirrorOptionCheckBox = new wxCheckBox(this, MIRROR_CHECK_BOX, wxT("Mirror Viz"));
   mirrorOptionCheckBox->SetValue(false);
   forthRow->Add(mirrorOptionCheckBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _geomPickingCBox = new wxCheckBox(this, GEOM_PICK_CBOX, wxT("Geometry Picking"));
   _geomPickingCBox->SetValue(false);
   forthRow->Add(_geomPickingCBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);
   wxBoxSizer* fifthRow = new wxBoxSizer(wxHORIZONTAL);

   _recordButton = new wxButton(this, RECORD_BUTTON, wxT("Record Scene"));
   fifthRow->Add(_recordButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,6);

   _clearButton = new wxButton(this, CLEAR_BUTTON, wxT("Clear All"));
   fifthRow->Add(_clearButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,6);

   _exitButton = new wxButton(this, EXIT_BUTTON, wxT("Exit"));
   fifthRow->Add(_exitButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,6);

   //Here is a new 6th row added for a custom visualization button
   wxBoxSizer* sixthRow = new wxBoxSizer(wxHORIZONTAL);

   //_customVisButton = new wxButton(this, CUSTOM_VIS_BUTTON,
   //                                wxT("Activate Custom Visualization"));
   //sixthRow ->Add(_customVisButton, 1, wxALIGN_CENTER_HORIZONTAL);
   sixthRow ->Add(_sliderUpdate, 1, wxALIGN_CENTER_HORIZONTAL|wxALL,6);

   //Add the rows to the main grouping
   visPanelGroup->Add(firstRow,  3, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(secondRow, 4, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(thirdRow,  2, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(forthRow,  1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(fifthRow,  1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(sixthRow,  1, wxALIGN_LEFT|wxEXPAND);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(visPanelGroup);
}  

////////////////////////////
//Event handling functions//
////////////////////////////
void UI_VisualizationTab::_onCategory(wxCommandEvent& WXUNUSED(event))
{
  //wxMessageBox(_categoryRBox->GetStringSelection(), _T("Category RadioBox!"));
}
//////////////////////////////////////////////////////////////////////////
void UI_VisualizationTab::_onGeometryPickingChecked(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = GEOMETRY_PICKING;
   ((UI_Tabs *)_parent)->cSc = (int)_geomPickingCBox->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
//////////////////////////////////////////////////////////////////////////////////
void UI_VisualizationTab::_onMirrorVisualization(wxCommandEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cId = MIRROR_VIS_DATA;
   ((UI_Tabs *)_parent)->cSc = (int)mirrorOptionCheckBox->GetValue();
std::cout << ((UI_Tabs *)_parent)->cSc << " : " << mirrorOptionCheckBox->GetValue() << std::endl;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onContour(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = CHANGE_CONTOUR_FILL;
   if ( _contourRBox->GetSelection() == 0 ) // X Plane 
   {
      ((UI_Tabs *)_parent)->cIso_value = 0;
   } 
   else if ( _contourRBox->GetSelection() == 1 ) // Y Plane 
   {
      ((UI_Tabs *)_parent)->cIso_value = 1;
   } 
   else if ( _contourRBox->GetSelection() == 2 ) // Z Plane 
   {
      ((UI_Tabs *)_parent)->cIso_value = 2;
   } 
   else 
   {
      std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
   }

   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onDirection(wxCommandEvent& WXUNUSED(event))
{
   //wxMessageBox(_directionRBox->GetStringSelection(), _T("RadioBox!"));
   std::cout << " Direction of Cutting plane is : "
               << _directionRBox->GetStringSelection() << std::endl;
  /* if ( _directionRBox->GetSelection() == 0 ) // X Plane 
   {
      ((UI_Tabs *)_parent)->cuttingDirection = X_DIRECTION;
   } 
   else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
   {
      ((UI_Tabs *)_parent)->cuttingDirection = Y_DIRECTION;
   } 
   else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
   {
      ((UI_Tabs *)_parent)->cuttingDirection = Z_DIRECTION;
   } 
   else if ( _directionRBox->GetSelection() == 3 ) // By wand 
   {
      // Doesn't work yet
      // Still needs to be implemented
   } 
   else 
   {
      std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
   }*/
}

/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onPreComp(wxCommandEvent& WXUNUSED(event))
{
   _nearestCBox->Enable(false);
/*  if (_pcsButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
}
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onSingle(wxCommandEvent& WXUNUSED(event))
{
   _nearestCBox->Enable(true);
/*  if (_spButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
}
//////////////////////////////////////////////////////////////////////
void UI_VisualizationTab::_onTextureBasedVisual(wxCommandEvent& WXUNUSED(event))
{
   if(_visOptionCheckBox){
      ((UI_Tabs *)_parent)->cId = VIS_OPTION;
      if(_visOptionCheckBox->GetValue() == true){
         //wxMessageBox(_T("texture based vis"), _T("CheckBox!"));
         ((UI_Tabs *)_parent)->cIso_value = TEXTURE_BASED_VISUALIZATION;;
      }else{
         //wxMessageBox(_T("Classic vis"), _T("CheckBox!"));
         ((UI_Tabs *)_parent)->cIso_value = CLASSIC_VISUALIZATION;
         
      }
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
}
//////////////////////////////////////////////////////////////////////
void UI_VisualizationTab::_onTransientChecked(wxCommandEvent& WXUNUSED(event))
{
   if(_transientControls){
      delete _transientControls;
      _transientControls = 0;
   }
   if(_transientCheckBox->GetValue() == true){
      //unsigned int nTimesteps = ((UI_Tabs*)GetParent())->_modelData->GetNubmerofDataSets(0);
      _transientControls = new UI_TransientDialog(19,this,TRANSIENT_DIALOG);
      _transientControls->SetTabControl(((UI_Tabs*)_parent));
      _transientControls->Show();
   }else{
      delete _transientControls;
      _transientControls = 0;
   }
}
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onNearest(wxCommandEvent& WXUNUSED(event))
{
/*  if (_nearestCBox->GetValue())
    wxMessageBox(_T("nearest plane box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("nearest plane box unchecked!"), _T("CheckBox!"));*/
}
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onUpdate(wxCommandEvent& WXUNUSED(event))
{
   this->createCommandId();
   if(_visOptionCheckBox->GetValue() == false){
      this->createTransientCommandId();
   }
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   std::cout << "Current Slider value: " << _slider->GetValue() << std::endl;
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onSlider(wxScrollEvent& WXUNUSED(event))
{
   if ( ( _categoryRBox->GetSelection() == 3 ) && 
         ( _visOptionCheckBox->GetValue() == true ) )
   {
      ((UI_Tabs *)_parent)->cId = ISOSURFACE;
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onScalarBar(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = SCALAR_BAR_TOGGLE;
   ((UI_Tabs *)_parent)->cIso_value = _scalarBarCBox->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
/*
   if (_scalarBarCBox->GetValue())
      wxMessageBox(_T("scalar bar box checked!"), _T("CheckBox!"));
   else
      wxMessageBox(_T("scalar bar box unchecked!"), _T("CheckBox!"));
*/
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onRecord(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = RECORD_SCENE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
  
   //wxMessageBox(_T("Record button clicked!"), _T("Button!"));
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onExit(wxCommandEvent& WXUNUSED(event))
{
   //wxMessageBox(_T("Exit button clicked!"), _T("Button!"));
   //return 
   ((UI_Tabs *)_parent)->cId = EXIT;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   if(((UI_Frame *)((UI_Tabs *)_parent)->GetParent())->_appParent != _T("Framework"))
      exit(0);
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onClear(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = CLEAR_ALL;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   //wxMessageBox(_T("Exit button clicked!"), _T("Button!"));
}

/////////////////////////////////////
void UI_VisualizationTab::createTransientCommandId( void )
{
   if ( _transientCheckBox->GetValue() )
   {
      // Move steady state vis request to sc var
      ((UI_Tabs *)_parent)->cSc = ((UI_Tabs *)_parent)->cId;

      // Set id to represent transient request
      ((UI_Tabs *)_parent)->cId = TRANSIENT_VIS_ACTIVE;
      // This are listed for reference only. Do NOT uncomment.
      // ((UI_Tabs *)_parent)->cPre_state = _nearestCBox->GetValue();
      // ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
   }
}

/////////////////////////////////////
void UI_VisualizationTab::createCommandId( void )
{
   if ( _categoryRBox->GetSelection() == 0 ) // Contour 
   {
      if ( _spButton->GetValue() ) 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_CONTOUR;
         }
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_CONTOUR;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_CONTOUR;
         } 
         else if ( _directionRBox->GetSelection() == 3 ) // By wand 
         {
            ((UI_Tabs *)_parent)->cId = CONTOUR;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      } 
      else // Means use all precomputed data 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_CONTOURS;
         } 
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_CONTOURS;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_CONTOURS;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      }
      ((UI_Tabs *)_parent)->cPre_state = _nearestCBox->GetValue();
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
      ((UI_Tabs *)_parent)->DisableAdvectionPage();
   } 
   else if ( _categoryRBox->GetSelection() == 1 ) // Warped Contour 
   {
      if ( _spButton->GetValue() ) 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_MOMENTUM;
         } 
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_MOMENTUM;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_MOMENTUM;
         } 
         else if ( _directionRBox->GetSelection() == 3 ) // By wand 
         {
            ((UI_Tabs *)_parent)->cId = MOMENTUM;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      } 
      else // Means use all precomputed data 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_MOMENTUMS;
         } 
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_MOMENTUMS;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_MOMENTUMS;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      }
      ((UI_Tabs *)_parent)->cPre_state = _nearestCBox->GetValue();
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
   } 
   else if ( _categoryRBox->GetSelection() == 2 ) 
   {
      if ( _spButton->GetValue() ) 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_VECTOR;
         } 
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_VECTOR;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_VECTOR;
         } 
         else if ( _directionRBox->GetSelection() == 3 ) // By wand 
         {
            ((UI_Tabs *)_parent)->cId = VECTOR;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      } 
      else // Means use all precomputed data 
      {
         if ( _directionRBox->GetSelection() == 0 ) // X Plane 
         {
            ((UI_Tabs *)_parent)->cId = X_VECTORS;
         } 
         else if ( _directionRBox->GetSelection() == 1 ) // Y Plane 
         {
            ((UI_Tabs *)_parent)->cId = Y_VECTORS;
         } 
         else if ( _directionRBox->GetSelection() == 2 ) // Z Plane 
         {
            ((UI_Tabs *)_parent)->cId = Z_VECTORS;
         } 
         else 
         {
            std::cout << "ERROR:There is something way wrong with the gui" << std::endl;
         }
      }
      ((UI_Tabs *)_parent)->cPre_state = _nearestCBox->GetValue();
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
      ((UI_Tabs *)_parent)->EnableAdvectionPage();
   } 
   else if ( _categoryRBox->GetSelection() == 3 ) // Iso-Surface Selection 
   {
      ((UI_Tabs *)_parent)->cId = ISOSURFACE;
      ((UI_Tabs *)_parent)->cPre_state = 0;
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
   } 
   else if ( _categoryRBox->GetSelection() == 5 ) // Animated Contours Selection 
   {
      ((UI_Tabs *)_parent)->cId = ANIMATED_IMAGES;
      ((UI_Tabs *)_parent)->cPre_state = 0;
      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
   } 
   else if ( _categoryRBox->GetSelection() == 6 ) // Polydata
   {
      ((UI_Tabs *)_parent)->cId = POLYDATA;
      ((UI_Tabs *)_parent)->cPre_state = 0;
   } 
   else if ( _categoryRBox->GetSelection() == 7 ) // Warped Polydata
   {
      ((UI_Tabs *)_parent)->cId = POLYDATA;
      ((UI_Tabs *)_parent)->cPre_state = 1;
   } 
   else 
   {
      std::cout << " ERROR:Function not implemented yet... " << std::endl;
   }
}

void UI_VisualizationTab::_onCustomVis(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = ACT_CUSTOM_VIZ;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
