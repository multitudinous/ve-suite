#include "Exterior_UI.h"

BEGIN_EVENT_TABLE(Exterior_UI_Dialog, UIDialog)
//apparently no event for the sliders
  EVT_RADIOBOX    (DIRECTION_WIND_RBOX,      Exterior_UI_Dialog::_onDirection)
  EVT_BUTTON      (EXT_UPDATE_BUTTON,            Exterior_UI_Dialog::_onUpdate)
  EVT_BUTTON      (EXT_EXIT_BUTTON,              Exterior_UI_Dialog::_onExit)
  EVT_BUTTON      (EXT_CLEAR_BUTTON,             Exterior_UI_Dialog::_onClear)
END_EVENT_TABLE()

IMPLEMENT_DYNAMIC_CLASS(Exterior_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Exterior_UI_Dialog
::Exterior_UI_Dialog
(wxWindow* parent, int id,
  double* screenDiameter,
  long* directionWind)
: UIDialog((wxWindow *) parent, id, "Exterior"),
  p_screenDiameter(screenDiameter),
  p_directionWind(directionWind)
{
  _buildPage();
}

void Exterior_UI_Dialog::_buildPage()
{

   //create the panel for the baffle tab

   //////////////////////////////////
   //Design the Direction radio box//
   //////////////////////////////////

   //The names of the radio box choices
//   wxString direction[] = {wxT("No Wind"), wxT("Cross Wind"), wxT("Tail Wind"), wxT("Other")};
   wxString direction[] = {wxT("No Wind"), wxT("Cross Wind From Left"), wxT("Tail Wind")};

   //Create a vertical radio box
   _directionWindRBox = new wxRadioBox(this, DIRECTION_WIND_RBOX, wxT("Wind Direction"),
                                         wxDefaultPosition, wxDefaultSize, 3, direction,1,
                                         wxRA_SPECIFY_COLS);

   //////////////////////////
   //Now design the sliders//
   //////////////////////////

   wxStaticBox* screenSizeGroup = 0;

   //Size of the slider
   wxSize slidesize(300, 50);

   //Steve added these five other sliders for the baffle GUI

   _screenSizeSlider = new wxSlider(this, SCREEN_SIZE_SLIDER, 0, 0, 1000,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);


   screenSizeGroup = new wxStaticBox(this,-1, wxT("Rotary Screen Diameter (mm)"));

   //A button to update info after UI input changes have been made   //More options at the bottom of the UI
   _Update = new wxButton(this, wxID_OK, wxT("Update"));
//   _clearButton = new wxButton(this, EXT_CLEAR_BUTTON, wxT("Clear all"));
//   _exitButton = new wxButton(this, EXT_EXIT_BUTTON, wxT("Exit"));

   //Now layout the UI.
   //There are basically 6 rows of controls, 5 rows of slider bars and one row of buttons.

   //The grouping for all controls    OKAY
   wxBoxSizer* exteriorGroup = new wxBoxSizer(wxVERTICAL);


   //add the sliders, text boxes to the static box sizers, like so~~~~~~~~~~~

   //rotary screen size
   wxStaticBoxSizer* screenSizeSliderGroup = new wxStaticBoxSizer(screenSizeGroup,wxHORIZONTAL);
   screenSizeSliderGroup->Add(_screenSizeSlider,5, wxALIGN_LEFT);

   wxBoxSizer* RowOne = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowSix = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);


   //direction box (taken from VisTab)
   secondRow->Add(_directionWindRBox, 1, wxALIGN_LEFT);

   RowOne->Add(screenSizeSliderGroup,1, wxALIGN_LEFT);
 
   RowSix->Add(_Update, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_clearButton, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_exitButton, 1, wxALIGN_CENTER_HORIZONTAL);
 
   //for my baffle tab                          OKAY
   exteriorGroup->Add(secondRow, 1, wxALIGN_LEFT|wxEXPAND);    
   exteriorGroup->Add(RowOne, 1, wxALIGN_LEFT|wxEXPAND);
   exteriorGroup->Add(RowSix, 1, wxALIGN_LEFT|wxEXPAND); 

   //set this flag and let wx handle alignment  OKAY
   SetAutoLayout(true);

   //assign the group to the panel              OKAY
   SetSizer(exteriorGroup);
   exteriorGroup->Fit(this);

}  



/////////////////////////////////////////////////////
Exterior_UI_Dialog
::~Exterior_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Exterior_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool Exterior_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void Exterior_UI_Dialog::Lock(bool l)
{
}


void Exterior_UI_Dialog::_onDirection(wxCommandEvent& event)
{
}

void Exterior_UI_Dialog::_onUpdate(wxCommandEvent& event)   
{
}

void Exterior_UI_Dialog::_onClear(wxCommandEvent& event)
{
}

void Exterior_UI_Dialog::_onExit(wxCommandEvent& event)
{
}

