#include "UI_GeometryTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>
#include <string>
#include <cmath>

BEGIN_EVENT_TABLE(UI_GeometryTab, wxPanel)
   //EVT_RADIOBOX(GEOMETRY_RBOX,UI_GeometryTab::_onGeometry)
   //EVT_CHECKLISTBOX(GEOMETRY_RBOX,UI_GeometryTab::_onGeometry)
   EVT_BUTTON(GEOMETRY_UPDATE_BUTTON,UI_GeometryTab::_onUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_GeometryTab::UI_GeometryTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   
   _geometryRBox = 0;
   _geometryCBox = 0;
   _updateButton = 0;
   _parent = tControl;

   _buildPage();
}
//////////////////////////////
//build the geometry tab    //
//////////////////////////////
void UI_GeometryTab::_buildPage()
{

   //the radio box
   int numGeoms = ((UI_Tabs *)_parent)->num_geo;
   wxString* defaultName;

   if ( numGeoms > 0 )
   {  
      defaultName = new wxString[ numGeoms ];
      for(CORBA::ULong i = 0; i < (unsigned int)numGeoms; i++)
      {  
         defaultName[ i ] = ((UI_Tabs*)_parent)->geoNameArray[ i ];
         std::cout << "Geometry Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      numGeoms = 1;
      defaultName = new wxString[ numGeoms ];
      defaultName[ 0 ] = wxT("No Geometry Files");
   }

   /*_geometryRBox = new wxRadioBox(  this, GEOMETRY_RBOX, wxT("Geometry Files"),
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, defaultName,
                                    1 , wxRA_SPECIFY_COLS);*/

   _geometryCBox = new wxCheckListBox( this, GEOMETRY_CBOX, wxDefaultPosition, 
                                       wxDefaultSize, numGeoms, defaultName, 
                                       0, wxDefaultValidator, 
                                       wxT("Geometry Files") );
   // Used to initialize all the checkboxes on
   
   for(int j = 0; j < numGeoms; j++)
   {
      _geometryCBox->Check( j );
   }

   if ( ((UI_Tabs *)_parent)->num_geo == 0 )
   {
      _geometryCBox->Enable( false );
   }

   //the update button
   _updateButton = new wxButton(this,GEOMETRY_UPDATE_BUTTON,wxT("Update"));

   //the panel sizer
   wxBoxSizer* geometryPanelGroup = new wxBoxSizer(wxVERTICAL);
   //geometryPanelGroup->Add(_geometryRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   geometryPanelGroup->Add(_geometryCBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   geometryPanelGroup->Add(_updateButton,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(geometryPanelGroup);

}
//////////////////
//event handling//
///////////////////

//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_GeometryTab::_onGeometry(wxCommandEvent& event)
{
}
//////////////////////////////////////////////////
void UI_GeometryTab::_onUpdate(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cGeo_state = 0;
   for(int i = 0; i < ((UI_Tabs *)_parent)->num_geo; i++)
   {
      if ( _geometryCBox->IsChecked( i ) )
         ((UI_Tabs *)_parent)->cGeo_state += (int)pow( 2.0f, (float)i );
   }
   cout << ((UI_Tabs *)_parent)->cGeo_state <<endl;
   ((UI_Tabs *)_parent)->cId  = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}



