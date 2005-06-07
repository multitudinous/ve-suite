#include "UI_ViewLocTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>

BEGIN_EVENT_TABLE(UI_ViewLocTab, wxPanel)
   EVT_RADIOBOX(VIEWLOC_RBOX,UI_ViewLocTab::_onViewLoc)
   EVT_BUTTON(VIEWLOC_LOAD_BUTTON,UI_ViewLocTab::_onLoad)
   EVT_BUTTON(VIEWLOC_WRITE_BUTTON,UI_ViewLocTab::_onWrite)
   EVT_BUTTON(VIEWLOC_READ_BUTTON,UI_ViewLocTab::_onRead)
   //EVT_BUTTON(VIEWLOC_REMOVE_BUTTON,UI_ViewLocTab::_onRemove)
   EVT_BUTTON(VIEWLOC_MOVE_BUTTON,UI_ViewLocTab::_onMove)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_ViewLocTab::UI_ViewLocTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;
   _locationsRBox = 0;

   _buildPage();
}

UI_ViewLocTab::~UI_ViewLocTab( void )
{
   delete [] _defaultName;
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void UI_ViewLocTab::_buildPage()
{
   //the radio box
   int numStoredLocations = ((UI_Tabs *)_parent)->num_viewlocs;
   
   if(numStoredLocations < 0)
   {
      numStoredLocations = 0;
   }

   std::cout<<"# locs"<<numStoredLocations<<std::endl;
   char viewpt_name[30];
   if(numStoredLocations){
      _defaultName = new wxString[ numStoredLocations ];
   }else{
      _defaultName = new wxString[1];
      _defaultName[0] = wxT("Default");
   }
   for( int i=0; i<numStoredLocations; i++)
   {
      sprintf(viewpt_name,"View Location %i",i);
      _defaultName[i] = viewpt_name;
   }
   
   /*if ( numStoredLocations > 0 )
   {
      defaultName = new wxString[ numStoredLocations ];
      for(CORBA::ULong i = 0; i < (unsigned int)numStoredLocations; i++)
      {
         defaultName[ i ] = ((UI_Tabs*)_parent)->viewlocNameArray[ i ];
         std::cout << "View Point  Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      numStoredLocations = 1;
      defaultName = new wxString[ numStoredLocations ];
      defaultName[ 0 ] = wxT("No Stored View Points");
   }*/
   if(numStoredLocations)
   {
      _locationsRBox = new wxRadioBox(this, VIEWLOC_RBOX, wxT("Stored View Points"),
                                wxDefaultPosition, wxDefaultSize, numStoredLocations,
                                _defaultName,1 , wxRA_SPECIFY_COLS);
   }
   else
   {
      _locationsRBox = new wxRadioBox(this, VIEWLOC_RBOX, wxT("Stored View Points"),
                                wxDefaultPosition, wxDefaultSize, 1,
                                _defaultName,1 , wxRA_SPECIFY_COLS);
   }
   if ( ((UI_Tabs *)_parent)->num_viewlocs == 0 )
   {
      _locationsRBox->Enable( false );
   }

   // Add View Point Button
   _loadButton = new wxButton(this, VIEWLOC_LOAD_BUTTON, wxT("Add View Point"));
   //_viewpointName = new wxTextCtrl(this, -1, wxT("Enter New View Point Name Here"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER);

   // Remove View Point Button
   //_removeButton = new wxButton(this, VIEWLOC_REMOVE_BUTTON, wxT("Remove View Point"));

   _writeButton = new wxButton(this, VIEWLOC_WRITE_BUTTON, wxT("Write View Point to File"));
   _readButton = new wxButton(this, VIEWLOC_READ_BUTTON, wxT("Read View Points From File"));

   // Move To Selected View Point Button
   _moveButton = new wxButton(this, VIEWLOC_MOVE_BUTTON, wxT("Move To Selected View Point"));

   //the radio panel and move button sizer
   wxBoxSizer* listGroup = new wxBoxSizer(wxVERTICAL);
   listGroup->Add(_locationsRBox,5,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   listGroup->Add(_moveButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 

   // the text entrie for location name


   wxGridSizer* buttonSizer = new wxGridSizer(1,4);

   wxStaticText* blank1 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank2 = new wxStaticText(this, -1, ""); //just a place holder
   wxStaticText* blank3 = new wxStaticText(this, -1, ""); //just a place holder

   buttonSizer->Add(blank1,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //buttonSizer->Add(_viewpointName,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   buttonSizer->Add(_loadButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   buttonSizer->Add(blank2,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   buttonSizer->Add(_writeButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   buttonSizer->Add(blank3,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   //buttonSizer->Add(_removeButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);
   buttonSizer->Add(_readButton,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //the action button sizer
   wxBoxSizer* buttonGroup = new wxBoxSizer(wxVERTICAL);
   buttonGroup->Add(buttonSizer,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //the main group
   wxBoxSizer* viewlocPanelGroup = new wxBoxSizer(wxHORIZONTAL);

   //add the rows to the main panel
   viewlocPanelGroup->Add(listGroup,2,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
   viewlocPanelGroup->Add(buttonGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(viewlocPanelGroup);
}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_ViewLocTab::_onViewLoc(wxCommandEvent& WXUNUSED(event))
{
   /*// Are there any stored locations loaded?
   if ( ((UI_Tabs *)_parent)->num_locations > 0 )
   {
      for( int i = 0; i < ((UI_Tabs *)_parent)->num_locations; i++)
      {  
         // _teacherRBox->GetSelection();
         // This code is not correct
         // Need to fix this 
         ((UI_Tabs *)_parent)->cIso_value = i;
      }
      ((UI_Tabs *)_parent)->cId = CURRENT_VIEW_LOC;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
   else
   {
      std::cout << "There are no stored view points loaded to select!" << std::endl;
   }*/
}

void UI_ViewLocTab::_onLoad(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = LOAD_POINT;
   //((UI_Tabs *)_parent)->viewlocNewPointName = _viewpointName->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

/*void UI_ViewLocTab::_onDetach(wxCommandEvent& event)
{
   if ( ((UI_Tabs *)_parent)->num_locations > 0 )
   {
      for( int i = 0; i < ((UI_Tabs *)_parent)->num_locations; i++)
      {  
         // _teacherRBox->GetSelection();
         // This code is not correct
         // Need to fix this 
         ((UI_Tabs *)_parent)->cIso_value = i;
      }
      ((UI_Tabs *)_parent)->cId = REMOVE_VIEW_LOC;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
   else
   {
      std::cout << "There are no stored view points loaded to remove!" << std::endl;
   }
}*/

void UI_ViewLocTab::_onWrite(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = WRITE_POINTS_TO_FILE;
   //((UI_Tabs *)_parent)->viewlocNewPointName = _viewpointName->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_ViewLocTab::_onRead(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId = READ_POINTS_FROM_FILE;
   //((UI_Tabs *)_parent)->viewlocNewPointName = _viewpointName->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

void UI_ViewLocTab::_onMove(wxCommandEvent& WXUNUSED(event))
{
   if ( ((UI_Tabs *)_parent)->num_viewlocs > 0 )
   {
      ((UI_Tabs *)_parent)->cIso_value = _locationsRBox->GetSelection();
      ((UI_Tabs *)_parent)->cId = MOVE_TO_SELECTED_LOCATION;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
   else
   {
      std::cout << "There are no stored view points loaded to move to!" << std::endl;
   }
}

