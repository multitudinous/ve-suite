#include "Baffle_UI.h"

BEGIN_EVENT_TABLE(Baffle_UI_Dialog, UIDialog)
//apparently no event for the sliders
  EVT_RADIOBOX    (DIRECTION_RBOX,           Baffle_UI_Dialog::_onDirection)
  EVT_RADIOBOX    (ERROR_RBOX,               Baffle_UI_Dialog::_onError)
  EVT_BUTTON      (UPDATE_BUTTON,            Baffle_UI_Dialog::_onSliderUpdate)
  EVT_BUTTON      (EXIT_BUTTON,              Baffle_UI_Dialog::_onExit)
  EVT_BUTTON      (CLEAR_BUTTON,             Baffle_UI_Dialog::_onClear)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Baffle_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Baffle_UI_Dialog
::Baffle_UI_Dialog
(wxWindow* parent, int id,
  double* xLocation,
  double* yLocation,
  double* zLocation,
  double* height,
  double* width)
: UIDialog((wxWindow *) parent, id, "ENGINE/BAFFLE"),
  p_xLocation(xLocation),
  p_yLocation(yLocation),
  p_zLocation(zLocation),
  p_height(height),
  p_width(width)
{

_buildPage();
//put anything else you want in constructor here
}

void Baffle_UI_Dialog::_buildPage()
{

   //create the panel for the baffle tab

   //////////////////////////////////
   //Design the Direction radio box//
   //////////////////////////////////

   //The names of the radio box choices
//   wxString direction[] = {wxT("No Wind"), wxT("Cross Wind"), wxT("Tail Wind"), wxT("Other")};
   wxString direction[] = {wxT("No Wind"), wxT("Cross Wind From Left"), wxT("Tail Wind")};

   //Create a vertical radio box
   _directionRBox = new wxRadioBox(this, DIRECTION_RBOX, wxT("Wind Direction"),
                                         wxDefaultPosition, wxDefaultSize, 3, direction,1,
                                         wxRA_SPECIFY_COLS);


   wxString error[] = {wxT("< 5%"), wxT("< 10%"), wxT("< 20%")};

   //Create a vertical radio box
   _errorRBox = new wxRadioBox(this, ERROR_RBOX, wxT("Allowable Error"),
                                         wxDefaultPosition, wxDefaultSize, 3, error,1,
                                         wxRA_SPECIFY_COLS);

   //////////////////////////
   //Now design the sliders//
   //////////////////////////

   wxStaticBox* hGroup = 0;
   wxStaticBox* wGroup = 0;
   wxStaticBox* xGroup = 0;
   wxStaticBox* yGroup = 0;
   wxStaticBox* zGroup = 0;   

   //the input boxes
   _xLoc = new wxTextCtrl(this, 1, wxT("500"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _yLoc = new wxTextCtrl(this, -1, wxT("300"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _zLoc = new wxTextCtrl(this, -1, wxT("200"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);


   //Size of the slider
   wxSize slidesize(300, 50);

   //Steve added these five other sliders for the baffle GUI

   _heightSlider = new wxSlider(this, HEIGHT_SLIDER, 0, 0, 500,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   _widthSlider = new wxSlider(this, WIDTH_SLIDER, 0, 0, 500,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);
/*
   _xLocSlider = new wxSlider(this, X_LOCATION_SLIDER, 0, 0, 100,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   _yLocSlider = new wxSlider(this, Y_LOCATION_SLIDER, 0, 0, 100,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   _zLocSlider = new wxSlider(this, Z_LOCATION_SLIDER, 0, 0, 100,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);
*/
   hGroup = new wxStaticBox(this,-1, wxT("Baffle Height (mm Along Z Axis)"));
   wGroup = new wxStaticBox(this,-1, wxT("Baffle Width (mm Along Y Axis)"));
   xGroup = new wxStaticBox(this,-1, wxT("X Start Pos (mm)"));
   yGroup = new wxStaticBox(this,-1, wxT("Y Start Pos (mm)"));
   zGroup = new wxStaticBox(this,-1, wxT("Z Start Pos (mm)"));

   //A button to update info after UI input changes have been made   //More options at the bottom of the UI
   _sliderUpdate = new wxButton(this, wxID_OK, wxT("Update"));
//   _clearButton = new wxButton(this, CLEAR_BUTTON, wxT("Clear all"));
//  _exitButton = new wxButton(this, EXIT_BUTTON, wxT("Exit"));

   //Now layout the UI.
   //There are basically 6 rows of controls, 5 rows of slider bars and one row of buttons.

   //The grouping for all controls    OKAY
   wxBoxSizer* baffleGroup = new wxBoxSizer(wxVERTICAL);


   //add the sliders, text boxes to the static box sizers, like so~~~~~~~~~~~

   //height
   wxStaticBoxSizer* heightSliderGroup = new wxStaticBoxSizer(hGroup,wxHORIZONTAL);
   heightSliderGroup->Add(_heightSlider,5, wxALIGN_LEFT);
   //width
   wxStaticBoxSizer* widthSliderGroup = new wxStaticBoxSizer(wGroup,wxHORIZONTAL);
   widthSliderGroup->Add(_widthSlider,5, wxALIGN_LEFT); 
   //starting x  
   wxStaticBoxSizer* xLocGroup = new wxStaticBoxSizer(xGroup,wxVERTICAL);
//   xLocGroup->Add(_xLocSlider,5, wxALIGN_LEFT);
   xLocGroup->Add(_xLoc,5, wxALIGN_LEFT);
   //starting y
   wxStaticBoxSizer* yLocGroup = new wxStaticBoxSizer(yGroup,wxVERTICAL);
//   yLocGroup->Add(_yLocSlider,5, wxALIGN_LEFT);
   yLocGroup->Add(_yLoc,5, wxALIGN_LEFT);
   //starting z
   wxStaticBoxSizer* zLocGroup = new wxStaticBoxSizer(zGroup,wxVERTICAL);
//   zLocGroup->Add(_zLocSlider,5, wxALIGN_LEFT);
   zLocGroup->Add(_zLoc,5, wxALIGN_LEFT);

   wxBoxSizer* RowOne = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowTwo = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowThree = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowFour = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowFive = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowSix = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* thirdRow = new wxBoxSizer(wxHORIZONTAL);

   //specify the six rows OKAY 

   //direction box (taken from VisTab)
   secondRow->Add(_directionRBox, 1, wxALIGN_LEFT);
   thirdRow->Add(_errorRBox, 1, wxALIGN_LEFT);

   RowOne->Add(heightSliderGroup,1, wxALIGN_LEFT);
//   RowOne->Add(_heightSlider,5, wxALIGN_LEFT);

   RowTwo->Add(widthSliderGroup,1, wxALIGN_LEFT);  
//   RowTwo->Add(_widthSlider,5, wxALIGN_LEFT);

   RowThree->Add(xLocGroup,1, wxALIGN_LEFT); 
   RowThree->Add(yLocGroup,1, wxALIGN_LEFT);
   RowThree->Add(zLocGroup,1, wxALIGN_LEFT);
/*
   RowFour->Add(yLocGroup,1, wxALIGN_LEFT); 

   RowFive->Add(zLocGroup,1, wxALIGN_LEFT);
*/
 
   RowSix->Add(_sliderUpdate, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_clearButton, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_exitButton, 1, wxALIGN_CENTER_HORIZONTAL);
 
   //for my baffle tab                          OKAY
   baffleGroup->Add(secondRow, 1, wxALIGN_LEFT|wxEXPAND); 
   baffleGroup->Add(thirdRow, 1, wxALIGN_LEFT|wxEXPAND);   
   baffleGroup->Add(RowThree, 1, wxALIGN_LEFT|wxEXPAND); 
//   baffleGroup->Add(RowFour, 1, wxALIGN_LEFT|wxEXPAND);
//   baffleGroup->Add(RowFive, 1, wxALIGN_LEFT|wxEXPAND);
   baffleGroup->Add(RowOne, 1, wxALIGN_LEFT|wxEXPAND);
   baffleGroup->Add(RowTwo, 1, wxALIGN_LEFT|wxEXPAND);   
   baffleGroup->Add(RowSix, 1, wxALIGN_LEFT|wxEXPAND); 

   //set this flag and let wx handle alignment  OKAY
   SetAutoLayout(true);

   //assign the group to the panel              OKAY
   SetSizer(baffleGroup);
   baffleGroup->Fit(this);  


}

/////////////////////////////////////////////////////
Baffle_UI_Dialog
::~Baffle_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Baffle_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool Baffle_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void Baffle_UI_Dialog::Lock(bool l)
{
}

void Baffle_UI_Dialog::_onDirection(wxCommandEvent& event)
{
}

void Baffle_UI_Dialog::_onError(wxCommandEvent& event)
{
}

void Baffle_UI_Dialog::_onSliderUpdate(wxCommandEvent& event)   
{
}

void Baffle_UI_Dialog::_onClear(wxCommandEvent& event)
{
}

void Baffle_UI_Dialog::_onExit(wxCommandEvent& event)
{
}

