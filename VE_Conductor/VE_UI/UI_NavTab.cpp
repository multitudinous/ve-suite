#include "UI_NavTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>

BEGIN_EVENT_TABLE(UI_NavigationTab, wxPanel)
   EVT_MOUSE_EVENTS(UI_NavigationTab::onMouse)
   EVT_COMMAND_SCROLL( NAV_STEP_SLIDER, UI_NavigationTab::OnNavigationStepSlider)
   EVT_BUTTON        ( RESET_NAV_POSITION, UI_NavigationTab::OnResetNavPosition)
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

   //The static box for the buttons
   wxStaticBox* buttonStaticBox = new wxStaticBox(this, -1, wxT("Navigation Controls"));

   //need a sizer for this box
   //The items will be placed  next (vertically) to other 
   //rather than on top of each other(horizontally)
   wxStaticBoxSizer* buttonStaticBoxSizer = new wxStaticBoxSizer( buttonStaticBox, wxVERTICAL);

   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* col1 = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* navCol = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* col2 = new wxBoxSizer(wxVERTICAL);

   wxGridSizer* topSizer = new wxGridSizer(5, 3);

   _activeButton = NONE;

   _leftButton = new UI_NavButton(this, NAV_LEFT, "Left");
   _rightButton = new UI_NavButton(this, NAV_RIGHT, "Right");
   _upButton = new UI_NavButton(this, NAV_UP, "Up");
   _downButton = new UI_NavButton(this, NAV_DOWN, "Down");
   _forwardButton = new UI_NavButton(this, NAV_FWD, "Forward");
   _backButton = new UI_NavButton(this, NAV_BKWD, "Backward");
   _ccwButton = new UI_NavButton(this, NAV_CCW, "ROTATE +");
   _cwButton = new UI_NavButton(this, NAV_CW, "ROTATE -");


   wxStaticText* blank1 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank2 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank3 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank4 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank5 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank6 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank7 = new wxStaticText(this, -1, ""); //just a place holder
   //wxStaticText* blank8 = new wxStaticText(this, -1, ""); //just a place holder
 
   //first row of the grid
   topSizer->Add(_ccwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_forwardButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_cwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank1,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_upButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //second row of the grid
   topSizer->Add(_leftButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank2,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_rightButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank3,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank4,1,wxALIGN_CENTER_HORIZONTAL);
    //third row
   topSizer->Add(blank5,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_backButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank6,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank7,1,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_downButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
 
   navCol->Add(topSizer,1,wxALIGN_CENTER_HORIZONTAL|wxALL, 5);

   //buttonStaticBoxSizer->Add(col1,0,wxALIGN_CENTER_HORIZONTAL);
   buttonStaticBoxSizer->Add(navCol,4,wxALIGN_CENTER_HORIZONTAL);
   //buttonStaticBoxSizer->Add(col2,0,wxALIGN_CENTER_HORIZONTAL);
   
   // add navigation step size slider
   navigationStepSize = new wxSlider(this, NAV_STEP_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
   
   //the labels for the sliders 
   wxStaticText* stepSizeLabel = new wxStaticText(this,-1,wxT("Navigation Step Size"));
   
   wxBoxSizer* stepSizeGroup = new wxBoxSizer(wxVERTICAL);
   stepSizeGroup->Add(stepSizeLabel,0,wxALIGN_LEFT);
   stepSizeGroup->Add(navigationStepSize,1,wxALIGN_LEFT|wxEXPAND);
      
   // Misc buttons and check boxes
   wxBoxSizer* miscGroup = new wxBoxSizer(wxHORIZONTAL);
   headRotationChk = new wxCheckBox( this, HEAD_ROTATE_CHK,
                                    wxT("Rotate About Users Head"));
   miscGroup->Add( headRotationChk,1,wxALL|wxALIGN_LEFT, 5);

   resetNavPosition = new wxButton(this, RESET_NAV_POSITION,
                                    wxT("Reset Nav Position"));
   miscGroup->Add( resetNavPosition,1,wxALL|wxALIGN_LEFT, 5);

   // Add everything to static box sizer
   buttonStaticBoxSizer->Add( stepSizeGroup,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   buttonStaticBoxSizer->Add( miscGroup,1,wxALL|wxALIGN_LEFT, 5);

   mainSizer->Add( buttonStaticBoxSizer,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   SetSizer( mainSizer );

   // Update VE-Xplorer data
   ((UI_Tabs *)_parent)->cId = CHANGE_NAVIGATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navigationStepSize->GetValue();
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
wxWindowID id, const wxString& label)
:wxButton(parent,id,label,wxDefaultPosition,wxDefaultSize,wxBU_EXACTFIT)
{
  _buttonPushed = 0; 
}
///////////////////////////////////////////////////
void UI_NavButton::onMouseUp(wxMouseEvent& mouse)
{
   _buttonPushed = 0;
   std::cout<<"Mouse released from button: "<<GetId()<<std::endl;
   //if left button comes up 
   //specific button we need to 
   //tell cfdApp to stop moving 
   //reset the active button
   ((UI_NavigationTab*)GetParent())->setActiveButton(NONE);
      
   //relay info to cfdApp 
   ((UI_NavigationTab*)GetParent())->updateParent(0,-1);
}
///////////////////////////////////////////////
//only activate motion when left mouse is    //
//pressed over a specific navigation button  //          
///////////////////////////////////////////////
void UI_NavButton::onMouse(wxMouseEvent& mouse)
{
   int activeId = ((UI_NavigationTab*)GetParent())->getActiveButton();

   //no button pushed yet
   if(activeId == NONE){     
      if(mouse.LeftIsDown()){
         std::cout<<"Mouse pushed on button: "<<GetId()<<std::endl;
        //set the active id to this button
        //if mouse is down
         _buttonPushed = 1;         

         //update the active button
         ((UI_NavigationTab*)GetParent())->setActiveButton(GetId());

         //pass the nav info to cfdApp
         ((UI_NavigationTab*)GetParent())->updateParent(_buttonPushed,GetId());              
      }
   }
}

///////////////////////////////////////////////////////
void UI_NavigationTab::updateParent(int pushed, int id)
{
   //if we released a button tell cfdApp to stop moving
   if(!pushed){
      ((UI_Tabs*)GetParent())->cId = -1;
      ((UI_Tabs*)GetParent())->cIso_value = -1;
      ((UI_Tabs*)GetParent())->sendDataArrayToServer();
   }else{
      //we pushed a button.
      //tell cfdApp to move appropriately
      ((UI_Tabs*)GetParent())->cId = GUI_NAV;
      ((UI_Tabs*)GetParent())->cIso_value = id;
      ((UI_Tabs*)GetParent())->sendDataArrayToServer();
   }
}

void UI_NavigationTab::OnNavigationStepSlider( wxScrollEvent& event)
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_NAVIGATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = navigationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnResetNavPosition( wxCommandEvent& event )
{
   ((UI_Tabs *)_parent)->cId  = RESET_NAVIGATION_POSITION;
   ((UI_Tabs *)_parent)->cIso_value = navigationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
