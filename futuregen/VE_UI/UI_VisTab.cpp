#include "UI_VisTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include "UI_Frame.h"
#include <iostream>

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
END_EVENT_TABLE()

/////////////////////////////////////////////////////////////
//Constructor                                              //
/////////////////////////////////////////////////////////////
UI_VisualizationTab::UI_VisualizationTab(wxNotebook* tControl)
:wxPanel(tControl)
{
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
                           wxT("Polydata") };

   //Create the 3x2 box
   _categoryRBox = new wxRadioBox(this, CATEGORY_RAD_BOX, wxT("Category"),
                                  wxDefaultPosition, wxDefaultSize, 7,
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
                          wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   //A button to update info after UI input changes have been made
   _sliderUpdate = new wxButton(this, UPDATE_BUTTON, wxT("Update"));

   //Now layout the UI. There are basically 5 rows of controls.

   //The grouping for all controls
   wxSizer* visPanelGroup = new wxBoxSizer(wxVERTICAL);

   //The first row of controls contains two radio boxes
   wxBoxSizer* firstRow = new wxBoxSizer(wxHORIZONTAL);

   //catergory radio box
   firstRow->Add(_categoryRBox, 3, wxALIGN_LEFT);

   //contour type radio box
   firstRow->Add(_contourRBox, 1, wxALIGN_RIGHT);

   //The second row of controls contains the direction and type boxes
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);

   //direction box
   secondRow->Add(_directionRBox, 1, wxALIGN_LEFT);

   //The static box group we created ("Type")
   secondRow->Add(typebox_sizer,2,wxALIGN_RIGHT);

   //The slider and the update button belong in the third row
   wxBoxSizer* thirdRow = new wxBoxSizer(wxHORIZONTAL);

   thirdRow->Add(_slider,5, wxALIGN_LEFT);
   thirdRow->Add(_sliderUpdate,0,wxALIGN_RIGHT);

   //This is the fourth row of buttons
   wxBoxSizer* forthRow = new wxBoxSizer(wxHORIZONTAL);

   _transientCheckBox = new wxCheckBox(this, TRANSIENT_CHECK_BOX, wxT("Transient"));
   _transientCheckBox->SetValue(false);
   forthRow->Add(_transientCheckBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _scalarBarCBox = new wxCheckBox(this, SCALAR_BAR_CHECK_BOX, wxT("Scalar Bar"));
   _scalarBarCBox->SetValue(true);
   forthRow->Add(_scalarBarCBox, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _recordButton = new wxButton(this, RECORD_BUTTON, wxT("Record Scene"));
   forthRow->Add(_recordButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _clearButton = new wxButton(this, CLEAR_BUTTON, wxT("Clear All"));
   forthRow->Add(_clearButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   _exitButton = new wxButton(this, EXIT_BUTTON, wxT("Exit"));
   forthRow->Add(_exitButton, 1, wxALL|wxALIGN_CENTER_HORIZONTAL,5);

   //Here is a new 5th row added for a custom visualization button
   wxBoxSizer* fifthRow = new wxBoxSizer(wxHORIZONTAL);

	_customVisButton = new wxButton(this, CUSTOM_VIS_BUTTON,
                                   wxT("Activate Custom Visualization"));
   fifthRow ->Add(_customVisButton, 0, wxALIGN_CENTER_HORIZONTAL);

   //Add the rows to the main grouping
   visPanelGroup->Add(firstRow, 1, wxALIGN_LEFT|wxEXPAND); //
   visPanelGroup->Add(secondRow, 1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(thirdRow, 1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(forthRow, 1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(fifthRow, 1, wxALIGN_LEFT|wxEXPAND);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(visPanelGroup);
}  

////////////////////////////
//Event handling functions//
////////////////////////////
void UI_VisualizationTab::_onCategory(wxCommandEvent& event)
{
  //wxMessageBox(_categoryRBox->GetStringSelection(), _T("Category RadioBox!"));
}

/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onContour(wxCommandEvent& event)
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
void UI_VisualizationTab::_onDirection(wxCommandEvent& event)
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
void UI_VisualizationTab::_onPreComp(wxCommandEvent& event)
{
   _nearestCBox->Enable(false);
/*  if (_pcsButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
}
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onSingle(wxCommandEvent& event)
{
   _nearestCBox->Enable(true);
/*  if (_spButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
}
  
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onNearest(wxCommandEvent& event)
{
/*  if (_nearestCBox->GetValue())
    wxMessageBox(_T("nearest plane box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("nearest plane box unchecked!"), _T("CheckBox!"));*/
}
/////////////////////////////////////////////////////////
void UI_VisualizationTab::_onUpdate(wxCommandEvent& event)
{
   char s[256];
   createCommandId();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   std::cout << "Current Slider value: " << _slider->GetValue() << std::endl;
}
//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onSlider(wxCommandEvent& event)
{
   // This function changes the min and max of the current active scalar
   ((UI_Tabs *)_parent)->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Tabs *)_parent)->cMin = _slider->GetMin();
   ((UI_Tabs *)_parent)->cMax = _slider->GetMax();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onScalarBar(wxCommandEvent& event)
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
void UI_VisualizationTab::_onRecord(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = RECORD_SCENE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
  
   //wxMessageBox(_T("Record button clicked!"), _T("Button!"));
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onExit(wxCommandEvent& event)
{
   
   //wxMessageBox(_T("Exit button clicked!"), _T("Button!"));
   //return 
   ((UI_Tabs *)_parent)->cId = EXIT;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   if(((UI_Frame *)((UI_Tabs *)_parent)->GetParent())->_appParent != _T("Framework"))
      exit(0);
}

//////////////////////////////////////////////////////////
void UI_VisualizationTab::_onClear(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = CLEAR_ALL;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
   //wxMessageBox(_T("Exit button clicked!"), _T("Button!"));
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
   } 
   else if ( _categoryRBox->GetSelection() == 1 ) // Warped Contour 
   {  std::cout << _spButton->GetValue() << std::endl;
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
   else 
   {
      std::cout << " ERROR:Function not implemented yet... " << std::endl;
   }
}

void UI_VisualizationTab::_onCustomVis(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = ACT_CUSTOM_VIZ;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
