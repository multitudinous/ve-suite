#include "UI_NavTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>


BEGIN_EVENT_TABLE(UI_NavigationTab, wxPanel)
   EVT_MOUSE_EVENTS(UI_NavigationTab::onMouse)
   EVT_COMMAND_SCROLL( TRANS_STEP_SLIDER, UI_NavigationTab::OnTransStepSlider)
   EVT_COMMAND_SCROLL( ROT_STEP_SLIDER, UI_NavigationTab::OnRotStepSlider)
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

   wxBoxSizer* navCol = new wxBoxSizer(wxVERTICAL);

   wxGridSizer* topSizer = new wxGridSizer(11,4);

   _activeButton = NONE;

////////////////////////////////////////////////////////////////////////////////////////////////////////
   //*******Loading up the bitmaps for navigation buttons
   //*******Relative paths are handled differently in windows, so check the OS and implement accordingly
////////////////////////////////////////////////////////////////////////////////////////////////////////
#ifdef WIN32
   wxBitmap* image1 = new wxBitmap();
   image1->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/x_left.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image2 = new wxBitmap();
   image2->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/x_right.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image3 = new wxBitmap();
   image3->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/z_up.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image4 = new wxBitmap();
   image4->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/z_down.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image5 = new wxBitmap();
   image5->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/y_up.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image6 = new wxBitmap();
   image6->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/y_down.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image7 = new wxBitmap();
   image7->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/pitch_down.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image8 = new wxBitmap();
   image8->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/pitch_up.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image9 = new wxBitmap();
   image9->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/ccw_roll.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image10 = new wxBitmap();
   image10->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/cw_roll.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image11 = new wxBitmap();
   image11->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/yaw_ccw.BMP",wxBITMAP_TYPE_BMP);
   wxBitmap* image12 = new wxBitmap();
   image12->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/yaw_cw.BMP",wxBITMAP_TYPE_BMP);

   wxBitmap* imagecoord = new wxBitmap();
   imagecoord->LoadFile("~/../../VE_Conductor/Framework/Nav_bitmaps/coordinates.bmp",wxBITMAP_TYPE_BMP);
#else
   
   wxImage* image1 = new wxImage();
   wxString temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/x_left.BMP");
   image1->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image2 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/x_right.BMP");
   image2->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image3 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/z_up.BMP");
   image3->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image4 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/z_down.BMP");
   image4->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image5 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/y_up.BMP");
   image5->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image6 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/y_down.BMP");
   image6->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image7 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/pitch_down.BMP");
   image7->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image8 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/pitch_up.bmp");
   image8->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image9 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/ccw_roll.BMP");
   image9->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image10 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/cw_roll.BMP");
   image10->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image11 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/yaw_ccw.bmp");
   image11->LoadFile(temp,wxBITMAP_TYPE_BMP);
   wxImage* image12 = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/yaw_cw.bmp");
   image12->LoadFile(temp,wxBITMAP_TYPE_BMP);

   wxImage* imagecoord = new wxImage();
   temp.clear();
   temp = strcat( getenv("VE_SUITE_HOME"), "/VE_Conductor/Framework/Nav_Bitmaps/coordinates.bmp");
   imagecoord->LoadFile(temp,wxBITMAP_TYPE_BMP);
#endif
//************Done loading up the bitmaps

   //Assign the bitmaps to the respective buttons
   _leftButton = new UI_NavButton(this, NAV_LEFT, wxImage(*image1));
   _rightButton = new UI_NavButton(this, NAV_RIGHT, wxBitmap(*image2));
   _upButton = new UI_NavButton(this, NAV_UP, wxBitmap(*image3));
   _downButton = new UI_NavButton(this, NAV_DOWN, wxBitmap(*image4));
   _forwardButton = new UI_NavButton(this, NAV_FWD, wxBitmap(*image5));
   _backButton = new UI_NavButton(this, NAV_BKWD, wxBitmap(*image6));
   _pitchdownButton = new UI_NavButton(this, PITCH_DOWN, wxBitmap(*image7));
   _pitchupButton = new UI_NavButton(this, PITCH_UP, wxBitmap(*image8));
   _rollccwButton = new UI_NavButton(this, ROLL_CCW, wxBitmap(*image9));
   _rollcwButton = new UI_NavButton(this, ROLL_CW, wxBitmap(*image10));
   _yawccwButton = new UI_NavButton(this, YAW_CCW, wxBitmap(*image11));
   _yawcwButton = new UI_NavButton(this, YAW_CW, wxBitmap(*image12));

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

   buttonStaticBoxSizer->Add(navCol,4,wxALIGN_CENTER_HORIZONTAL);

   wxStaticBitmap* coordpic = new wxStaticBitmap(this, -1,wxBitmap(*imagecoord),wxDefaultPosition,
											wxSize(100,102),wxMINIMIZE_BOX|wxTHICK_FRAME); 
   wxGridSizer* picSizer = new wxGridSizer(1,1);
   picSizer->Add(coordpic,1,wxALIGN_CENTER_HORIZONTAL);
   
   // add step size sliders
   translationStepSize = new wxSlider(this, TRANS_STEP_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
   rotationStepSize = new wxSlider(this, ROT_STEP_SLIDER,50,1,100,
                                wxDefaultPosition, wxDefaultSize,
                                wxSL_HORIZONTAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS|wxSL_RIGHT );
   
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
   miscGroup->Add( headRotationChk,1,wxALL|wxALIGN_LEFT, 5);

   resetNavPosition = new wxButton(this, RESET_NAV_POSITION,
                                    wxT("Reset Nav Position"));
   miscGroup->Add( resetNavPosition,1,wxALL|wxALIGN_LEFT, 5);
   miscGroup->Add( picSizer,1,wxALIGN_RIGHT);

   // Add everything to static box sizer
   buttonStaticBoxSizer->Add( stepSizeGroup,2,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   buttonStaticBoxSizer->Add( miscGroup,2,wxALL|wxALIGN_LEFT, 5);

   mainSizer->Add( buttonStaticBoxSizer,1,wxALL|wxALIGN_LEFT|wxEXPAND, 5);
   SetSizer( mainSizer );

   // Update VE-Xplorer data
   ((UI_Tabs *)_parent)->cId = CHANGE_TRANSLATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = translationStepSize->GetValue();
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

void UI_NavigationTab::OnTransStepSlider( wxScrollEvent& event)
{
   event.GetInt();
   ((UI_Tabs *)_parent)->cId  = CHANGE_TRANSLATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = translationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnRotStepSlider( wxScrollEvent& event)
{
   event.GetInt();
   ((UI_Tabs *)_parent)->cId  = CHANGE_ROTATION_STEP_SIZE;
   ((UI_Tabs *)_parent)->cIso_value = rotationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_NavigationTab::OnResetNavPosition( wxCommandEvent& event )
{
   event.GetInt();
   ((UI_Tabs *)_parent)->cId  = RESET_NAVIGATION_POSITION;
   ((UI_Tabs *)_parent)->cIso_value = translationStepSize->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
