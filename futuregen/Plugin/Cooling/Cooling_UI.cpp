#include "Cooling_UI.h"

BEGIN_EVENT_TABLE(Cooling_UI_Dialog, UIDialog)
//apparently no event for the sliders
  EVT_RADIOBOX    (COOLANT_RBOX,           Cooling_UI_Dialog::_onCoolant)
  EVT_BUTTON      (UPDATE_COOL_BUTTON,     Cooling_UI_Dialog::_onUpdate)
  EVT_BUTTON      (EXIT_COOL_BUTTON,       Cooling_UI_Dialog::_onExit)
  EVT_BUTTON      (CLEAR_COOL_BUTTON,      Cooling_UI_Dialog::_onClear)
END_EVENT_TABLE()


IMPLEMENT_DYNAMIC_CLASS(Cooling_UI_Dialog, UIDialog);

//Here is the constructor with passed in pointers
Cooling_UI_Dialog
::Cooling_UI_Dialog
(wxWindow* parent, int id,
  long* coolantType,
  long* heightRad,
  long* widthRad,
  long* fanPitch)
: UIDialog((wxWindow *) parent, id, "Cooling"),
  p_coolantType(coolantType),
  p_heightRad(heightRad),
  p_widthRad(widthRad),
  p_fanPitch(fanPitch)
{
  _buildPage();
}

void Cooling_UI_Dialog::_buildPage()
{

   //create the panel for the baffle tab

   //////////////////////////////////
   //Design the Direction radio box//
   //////////////////////////////////

   //The names of the radio box choices
//   wxString direction[] = {wxT("No Wind"), wxT("Cross Wind"), wxT("Tail Wind"), wxT("Other")};
   wxString coolant[] = {wxT("50% Antifreeze"), wxT("60% Antifreeze"), wxT("70% Antifreeze")};

   //Create a vertical radio box
   _coolantRBox = new wxRadioBox(this, COOLANT_RBOX, wxT("Antifreeze Concentration"),
                                         wxDefaultPosition, wxDefaultSize, 3, coolant,1,
                                         wxRA_SPECIFY_COLS);

   //////////////////////////
   //Now design the sliders//
   //////////////////////////

   wxStaticBox* hRadGroup = 0;
   wxStaticBox* wRadGroup = 0;
   wxStaticBox* fPitchGroup = 0;

   //Size of the slider
   wxSize slidesize(300, 50);

   //Steve added these five other sliders for the baffle GUI

   _heightRadSlider = new wxSlider(this, HEIGHT_RAD_SLIDER, 0, 0, 600,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   _widthRadSlider = new wxSlider(this, WIDTH_RAD_SLIDER, 0, 0, 600,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   _fanPitchSlider = new wxSlider(this, WIDTH_RAD_SLIDER, 0, 0, 90,
                                  wxDefaultPosition, slidesize,
                                  wxSL_HORIZONTAL|wxSL_AUTOTICKS|wxSL_LABELS);

   hRadGroup = new wxStaticBox(this,-1, wxT("Radiator Height (mm Along Z Axis)"));
   wRadGroup = new wxStaticBox(this,-1, wxT("Radiator Width (mm Along Y Axis)"));
   fPitchGroup = new wxStaticBox(this,-1, wxT("Fan Pitch Angle (Degrees)"));

   //A button to update info after UI input changes have been made   //More options at the bottom of the UI
   _Update = new wxButton(this, wxID_OK, wxT("Update"));
//   _clearButton = new wxButton(this, CLEAR_COOL_BUTTON, wxT("Clear all"));
//   _exitButton = new wxButton(this, EXIT_COOL_BUTTON, wxT("Exit"));

   //Now layout the UI.
   //There are basically 6 rows of controls, 5 rows of slider bars and one row of buttons.

   //The grouping for all controls    OKAY
   wxBoxSizer* coolingGroup = new wxBoxSizer(wxVERTICAL);


   //add the sliders, text boxes to the static box sizers, like so~~~~~~~~~~~

   //height of Radiator
//height: Command not found.
   wxStaticBoxSizer* heightRadGroup = new wxStaticBoxSizer(hRadGroup,wxHORIZONTAL);
   heightRadGroup->Add(_heightRadSlider,5, wxALIGN_LEFT);
   //width of Radiator
   wxStaticBoxSizer* widthRadGroup = new wxStaticBoxSizer(wRadGroup,wxHORIZONTAL);
   widthRadGroup->Add(_widthRadSlider,5, wxALIGN_LEFT); 
   //fan pitch Angle
   wxStaticBoxSizer* fanPitchGroup = new wxStaticBoxSizer(fPitchGroup,wxHORIZONTAL);
   fanPitchGroup->Add(_fanPitchSlider,5, wxALIGN_LEFT); 


   wxBoxSizer* RowOne = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowTwo = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowThree = new wxBoxSizer(wxHORIZONTAL);
//   wxBoxSizer* RowFour = new wxBoxSizer(wxHORIZONTAL);
//   wxBoxSizer* RowFive = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* RowSix = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* secondRow = new wxBoxSizer(wxHORIZONTAL);

   //specify the six rows OKAY 

   //direction box (taken from VisTab)
   secondRow->Add(_coolantRBox, 1, wxALIGN_LEFT);

   RowOne->Add(heightRadGroup,1, wxALIGN_LEFT);
//   RowOne->Add(_heightSlider,5, wxALIGN_LEFT);

   RowTwo->Add(widthRadGroup,1, wxALIGN_LEFT);  
//   RowTwo->Add(_widthSlider,5, wxALIGN_LEFT);

   RowThree->Add(fanPitchGroup,1, wxALIGN_LEFT); 

 
   RowSix->Add(_Update, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_clearButton, 1, wxALIGN_CENTER_HORIZONTAL);
//   RowSix->Add(_exitButton, 1, wxALIGN_CENTER_HORIZONTAL);
 
   //for my baffle tab                          OKAY
   coolingGroup->Add(secondRow, 1, wxALIGN_LEFT|wxEXPAND);    
   coolingGroup->Add(RowOne, 1, wxALIGN_LEFT|wxEXPAND);
   coolingGroup->Add(RowTwo, 1, wxALIGN_LEFT|wxEXPAND); 
   coolingGroup->Add(RowThree, 1, wxALIGN_LEFT|wxEXPAND);   
   coolingGroup->Add(RowSix, 1, wxALIGN_LEFT|wxEXPAND); 

   //set this flag and let wx handle alignment  OKAY
   SetAutoLayout(true);

   //assign the group to the panel              OKAY
   SetSizer(coolingGroup);
   coolingGroup->Fit(this);

}  



/////////////////////////////////////////////////////
Cooling_UI_Dialog
::~Cooling_UI_Dialog()
{
}

/////////////////////////////////////////////////////

bool Cooling_UI_Dialog::TransferDataFromWindow()
{
  return true;
}

////////////////////////////////////////////////////
bool Cooling_UI_Dialog::TransferDataToWindow()
{
    return true;
}

void Cooling_UI_Dialog::Lock(bool l)
{
}

void Cooling_UI_Dialog::_onCoolant(wxCommandEvent& event)
{
}

void Cooling_UI_Dialog::_onUpdate(wxCommandEvent& event)   
{
}

void Cooling_UI_Dialog::_onClear(wxCommandEvent& event)
{
}

void Cooling_UI_Dialog::_onExit(wxCommandEvent& event)
{
}
