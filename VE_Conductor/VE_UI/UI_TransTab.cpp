#include "UI_TransTab.h"
#include "UI_Tabs.h"//remember to add trans tab callbacks to UI_Tabs.h
#include "cfdEnum.h"
#include <iostream>

BEGIN_EVENT_TABLE(UI_TransTab, wxPanel)
  EVT_RADIOBOX    (TRANS_CATEGORY_RAD_BOX,   UI_TransTab::_onCategory)
  EVT_RADIOBOX    (TRANS_DIRECTION_RAD_BOX,  UI_TransTab::_onDirection)   
//do not need event for this  EVT_GAUGE       (TIMEPROGRESS_GAUGE,       UI_TransTab::_onTimeProgress)//check 
  EVT_BUTTON      (RESET_BUTTON,             UI_TransTab::_onReset)
  EVT_BUTTON      (BACKWARD_BUTTON,          UI_TransTab::_onBackward)
  EVT_BUTTON      (START_BUTTON,             UI_TransTab::_onStart)
  EVT_BUTTON      (FORWARD_BUTTON,           UI_TransTab::_onForward)
  EVT_BUTTON      (PAUSE_BUTTON,             UI_TransTab::_onPause)
END_EVENT_TABLE()

/////////////////////////////////////////////////////////////
//Constructor                                              //
/////////////////////////////////////////////////////////////
UI_TransTab::UI_TransTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   //initialize things
   _categoryRBox = 0;
   _directionRBox = 0;
   _timeProgressGauge = 0; //wxStatusBar or wxGauge
   _resetButton = 0;
   _backwardButton = 0;
   _startButton = 0;
   _forwardButton = 0;
   _pauseButton = 0;

   // IRIX Compiler didn't like this for some reason
   //((UI_Tabs*)_parent) = (UI_Tabs*)tControl;
   _parent = tControl;

   //build the page
   _buildPage();
}
///////////////////////
//build the TransPage//
///////////////////////
void UI_TransTab::_buildPage()
{
   //create the panel for the visualization tab

   //////////////////////////////////
   //Design the "Category" radiobox//
   //////////////////////////////////

   //The names of the radio box choices
   wxString category[] = { wxT("Contours on a Plane"),
                           wxT("Contours and Vectors on a Plane"),
                           wxT("Vectors on a Plane"),
                           wxT("Droplets"),
                          };
   //Create the 2x2 box
   _categoryRBox = new wxRadioBox(this, TRANS_CATEGORY_RAD_BOX, wxT("Category"),
                                  wxDefaultPosition, wxDefaultSize, 4,
                                  category, 2, wxRA_SPECIFY_COLS);

   //////////////////////////////////
   //Design the Direction radio box//
   //////////////////////////////////

   //The names of the radio box choices
   wxString direction[] = {wxT("X"), wxT("Y"), wxT("Z")};

   //Create a vertical radio box
   _directionRBox = new wxRadioBox(this, TRANS_DIRECTION_RAD_BOX, wxT("Direction"),
                                   wxDefaultPosition, wxDefaultSize, 3,
                                   direction,1, wxRA_SPECIFY_COLS);

   //////////////////////////////////////
   //Now design the time progress gauge//
   //////////////////////////////////////
/*
   _timeProgressGauge = new wxGauge(wxWindow* parent, wxWindowID id, int range, 
                              const wxPoint&  pos = wxDefaultPosition, 
                              const wxSize& size = wxDefaultSize, 
                              long style = wxGA_HORIZONTAL, 
                              const wxValidator& validator = wxDefaultValidator,
                              const wxString& name = "gauge")

*/

   _timeProgressGauge = new wxGauge(this, TIMEPROGRESS_GAUGE, 100, 
                                    wxDefaultPosition, wxDefaultSize,
                                    wxGA_HORIZONTAL, wxDefaultValidator,
                                    wxT("Time Progress Gauge"));

//   gaugeLabel = new wxStaticText(this,-1,wxT("Sphere/Point Size")
   
   //"VCR" buttons at the bottom of the UI   
   _resetButton = new wxButton(this, RESET_BUTTON, wxT("Reset"));
   _backwardButton = new wxButton(this, BACKWARD_BUTTON, wxT("Backward"));
   _startButton = new wxButton(this, START_BUTTON, wxT("Start"));
   _forwardButton = new wxButton(this, FORWARD_BUTTON, wxT("Forward"));
   _pauseButton = new wxButton(this, PAUSE_BUTTON, wxT("Pause"));

   //Now layout the UI. There are basically 4 rows of controls.

   //The grouping for all controls
   wxSizer* visPanelGroup = new wxBoxSizer(wxVERTICAL);

   //The first row of controls
   wxBoxSizer* firstRow = new wxBoxSizer(wxHORIZONTAL);

   //Now add the specific controls(2 radio boxes) for this group

   //catergory radio box
   firstRow->Add(_categoryRBox, 1, wxALIGN_LEFT);

   //contour type radio box
//   firstRow->Add(_contourRBox, 1, wxALIGN_RIGHT);

   //The second row of controls
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);

   //Add the direction and type boxes to this group

   //direction box
   secondRow->Add(_directionRBox, 1, wxALIGN_LEFT);

   //The static box group we created ("Type")
//   secondRow->Add(typebox_sizer,2,wxALIGN_RIGHT);

   //The slider and the update button belong in the
   //third group
   wxBoxSizer* thirdRow = new wxBoxSizer(wxHORIZONTAL);
   thirdRow->Add(_timeProgressGauge,5, wxALIGN_LEFT);

   //This is the bottom row of buttons
   wxBoxSizer* forthRow = new wxBoxSizer(wxHORIZONTAL);
   forthRow->Add(_resetButton, 1, wxALIGN_CENTER_HORIZONTAL);
   forthRow->Add(_backwardButton, 1, wxALIGN_CENTER_HORIZONTAL);
   forthRow->Add(_startButton, 1, wxALIGN_CENTER_HORIZONTAL);
   forthRow->Add(_forwardButton, 1, wxALIGN_CENTER_HORIZONTAL);
   forthRow->Add(_pauseButton, 1, wxALIGN_CENTER_HORIZONTAL);
   //Add the rows to the main grouping

   visPanelGroup->Add(firstRow, 1, wxALIGN_LEFT|wxEXPAND); 
   visPanelGroup->Add(secondRow, 1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(thirdRow, 1, wxALIGN_LEFT|wxEXPAND);
   visPanelGroup->Add(forthRow, 1, wxALIGN_LEFT|wxEXPAND);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(visPanelGroup);
}  

////////////////////////////
//Event handling functions//
////////////////////////////
void UI_TransTab::_onCategory(wxCommandEvent& event)
{
  //wxMessageBox(_categoryRBox->GetStringSelection(), _T("Category RadioBox!"));
}

/////////////////////////////////////////////////////////
void UI_TransTab::_onDirection(wxCommandEvent& event)
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
//void UI_TransTab::_onTimeProgress(wxCommandEvent& event)
//{
/*  if (_pcsButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
//}

/////////////////////////////////////////////////////////
void UI_TransTab::_onReset(wxCommandEvent& event)
{
/*  if (_spButton->GetValue())
    wxMessageBox(_T("single plane selected!"), _T("RadioButton!"));
  else
    wxMessageBox(_T("single plane unselected!"), _T("RadioButton!"));*/
}
  
/////////////////////////////////////////////////////////
void UI_TransTab::_onBackward(wxCommandEvent& event)
{
/*  if (_nearestCBox->GetValue())
    wxMessageBox(_T("nearest plane box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("nearest plane box unchecked!"), _T("CheckBox!"));*/
}

//////////////////////////////////////////////////////////
void UI_TransTab::_onStart(wxCommandEvent& event)
{
/*
   // This function changes the min and max of the current active scalar
   ((UI_Tabs *)_parent)->cId  = CHANGE_SCALAR_RANGE;
   ((UI_Tabs *)_parent)->cMin = _slider->GetMin();
   ((UI_Tabs *)_parent)->cMax = _slider->GetMax();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
*/
}
//////////////////////////////////////////////////////////
void UI_TransTab::_onForward(wxCommandEvent& event)
{
}

//////////////////////////////////////////////////////////
void UI_TransTab::_onPause(wxCommandEvent& event)
{
/*
   ((UI_Tabs *)_parent)->cId = SCALAR_BAR_TOGGLE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
*/
/*  if (_scalarBarCBox->GetValue())
    wxMessageBox(_T("scalar bar box checked!"), _T("CheckBox!"));
  else
    wxMessageBox(_T("scalar bar box unchecked!"), _T("CheckBox!"));*/
}


/*
/////////////////////////////////////
void UI_TransTab::createCommandId( void )
{
   if ( _categoryRBox->GetSelection() == 0 ) // Contour 
   {
//      if ( _spButton->GetValue() ) 
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
//      ((UI_Tabs *)_parent)->cPre_state = _nearestCBox->GetValue();
//      ((UI_Tabs *)_parent)->cIso_value = _slider->GetValue();
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
*/
