#include "UI_NavTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"


BEGIN_EVENT_TABLE(UI_NavigationTab, wxPanel)
  EVT_MOUSE_EVENTS(UI_NavigationTab::onMouse)
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
  wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* col1 = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* navCol = new wxBoxSizer(wxVERTICAL);
  wxBoxSizer* col2 = new wxBoxSizer(wxVERTICAL);

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
   wxStaticText* blank8 = new wxStaticText(this, -1, ""); //just a place holder
 
   //first row of the grid
   topSizer->Add(_ccwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_forwardButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_cwButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank1,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_upButton,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //second row of the grid
   topSizer->Add(_leftButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank2,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_rightButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank3,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank4,0,wxALIGN_CENTER_HORIZONTAL);
    //third row
   topSizer->Add(blank5,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_backButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank6,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(blank7,0,wxALIGN_CENTER_HORIZONTAL);
   topSizer->Add(_downButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
 
   navCol->Add(topSizer,1,wxALIGN_CENTER_HORIZONTAL);

   mainSizer->Add(col1,0,wxALIGN_CENTER_HORIZONTAL);
   mainSizer->Add(navCol,1,wxALIGN_CENTER_HORIZONTAL);
   mainSizer->Add(col2,0,wxALIGN_CENTER_HORIZONTAL);
   SetSizer(mainSizer);
}
///////////////////////////////////////////////////
void UI_NavigationTab::onMouse(wxMouseEvent& mouse)
{
   cout<<"Mouse action from tab!!"<<endl;
   //if left button comes up 
   //specific button we need to 
   //tell cfdApp to stop moving 
   if(mouse.LeftUp()){
      cout<<"left is up from tab!"<<endl;
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
:wxButton(parent,id,label)
{
  _buttonPushed = 0; 
}
///////////////////////////////////////////////////
void UI_NavButton::onMouseUp(wxMouseEvent& mouse)
{
   _buttonPushed = 0;
   cout<<"Mouse released from button: "<<GetId()<<endl;
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
        cout<<"Mouse pushed on button: "<<GetId()<<endl;
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
