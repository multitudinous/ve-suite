#include "UI_GeometryTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>
#include <string>
#include <cmath>

BEGIN_EVENT_TABLE(UI_GeometryTab, wxPanel)
   EVT_COMMAND_SCROLL(GEOMETRY_OPACITY_SLIDER, UI_GeometryTab::ChangeOpacity)
   EVT_COMMAND_SCROLL(GEOMETRY_LOD_SLIDER, UI_GeometryTab::_onGeometry)
   EVT_CHECKLISTBOX(GEOMETRY_CBOX,UI_GeometryTab::_onUpdate)
   //EVT_RADIOBOX(GEOMETRY_RBOX,UI_GeometryTab::ChangeOpacity)
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
   geomOpacitySlider = 0;
   geomLODSlider = 0;
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
   wxString* defaultName = 0;
   wxString* opacitytName = 0;

   if ( numGeoms > 0 )
   {  
      defaultName = new wxString[ numGeoms ];
      opacitytName = new wxString[ numGeoms ];
      for(CORBA::ULong i = 0; i < (unsigned int)numGeoms; i++)
      {  
         defaultName[ i ] = ((UI_Tabs*)_parent)->geoNameArray[ i ];
         std::cout << "Geometry Name " << i << " : " << defaultName[ i ] << std::endl;
         opacitytName[ i ] = wxString::Format("%s %i", "File", (int)(i+1) );
      }
   }
   else
   {
      numGeoms = 1;
      defaultName = new wxString[ numGeoms ];
      defaultName[ 0 ] = wxT("No Geometry Files");
      opacitytName = new wxString[ numGeoms ];
      opacitytName[ 0 ] = wxT( "0" );
   }

   _geometryRBox = new wxRadioBox(  this, GEOMETRY_RBOX, wxT("Opacity Control"),
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, opacitytName,
                                    1, wxRA_SPECIFY_COLS);

   wxStaticBox* geomFiles = new wxStaticBox(this,-1, wxT("Geometry Files"));
   wxStaticBoxSizer* geomFilesGroup = new wxStaticBoxSizer(geomFiles,wxVERTICAL);   
   _geometryCBox = new wxCheckListBox( this, GEOMETRY_CBOX,  
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, defaultName );
   geomFilesGroup->Add(_geometryCBox,1,wxALIGN_LEFT|wxEXPAND);
   
   wxBoxSizer* radioAndCheckBoxes = new wxBoxSizer( wxHORIZONTAL );
   radioAndCheckBoxes->Add(geomFilesGroup,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5);
   radioAndCheckBoxes->Add(_geometryRBox,0,wxALIGN_RIGHT|wxEXPAND|wxALL, 5);

   // Used to initialize all the checkboxes on
   
   for(int j = 0; j < numGeoms; j++)
   {
      _geometryCBox->Check( j );
   }

   if ( ((UI_Tabs *)_parent)->num_geo == 0 )
   {
      _geometryCBox->Enable( false );
   }

   // slider info
   //the labels for the sliders
   wxStaticText* opacityLabel = new wxStaticText(this, -1, wxT("Geometry Opacity"));
   wxStaticText* opacityLabelLeft = new wxStaticText(this, -1, wxT("Transparent"));
   wxStaticText* opacityLabelRight = new wxStaticText(this, -1, wxT("Opaque"));
   
   wxStaticText* lodLabel = new wxStaticText(this, -1, wxT("Geometry LOD Control"));
   wxStaticText* lodLabelLeft = new wxStaticText(this, -1, wxT("Lower"));
   wxStaticText* lodLabelRight = new wxStaticText(this, -1, wxT("Higher")); 

   //opacity slider
   geomOpacitySlider = new wxSlider(this, GEOMETRY_OPACITY_SLIDER,100,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );

   //lod slider
   geomLODSlider = new wxSlider(this, GEOMETRY_LOD_SLIDER,1000,0,1000,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_AUTOTICKS|
                                       wxSL_LABELS|wxSL_RIGHT );

   //two sizers to group the sliders and their lables
   wxBoxSizer* opacityGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* opacityLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   wxBoxSizer* lodGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* lodLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   opacityGroup->Add(opacityLabel,0,wxALIGN_LEFT|wxEXPAND);
   opacityGroup->Add(geomOpacitySlider,1,wxALIGN_LEFT|wxEXPAND);
   opacityLabelBottom->Add(opacityLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   opacityLabelBottom->Add(opacityLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   opacityGroup->Add(opacityLabelBottom,0,wxALIGN_LEFT|wxEXPAND);

   lodGroup->Add(lodLabel,0,wxALIGN_LEFT|wxEXPAND);
   lodGroup->Add(geomLODSlider,1,wxALIGN_LEFT|wxEXPAND);
   lodLabelBottom->Add(lodLabelLeft,6,wxALIGN_LEFT|wxEXPAND);
   lodLabelBottom->Add(lodLabelRight,0,wxALIGN_RIGHT|wxEXPAND);
   lodGroup->Add(lodLabelBottom,0,wxALIGN_LEFT|wxEXPAND|wxALL);

   //the update button
   //_updateButton = new wxButton(this,GEOMETRY_UPDATE_BUTTON,wxT("Update"));

   wxStaticBox* geomControls = new wxStaticBox(this,-1, wxT("Geometry Controls"));
   wxStaticBoxSizer* geomControlsGroup = new wxStaticBoxSizer(geomControls,wxVERTICAL);
   //the panel sizer
   //geometryPanelGroup->Add(_geometryRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   geomControlsGroup->Add(radioAndCheckBoxes,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
   geomControlsGroup->Add(opacityGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);
   geomControlsGroup->Add(lodGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);

   wxBoxSizer* geometryPanelGroup = new wxBoxSizer(wxVERTICAL);
   geometryPanelGroup->Add(geomControlsGroup,1,wxEXPAND|wxALL, 5);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(geometryPanelGroup);

   // Send lod info back to ve-xplorer
   ((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
   ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
//////////////////
//event handling//
///////////////////

//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_GeometryTab::_onGeometry( wxScrollEvent& event )
{
   ((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
   ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////
void UI_GeometryTab::ChangeOpacity( wxScrollEvent& event )
{
   ((UI_Tabs *)_parent)->cPre_state = 0;
   ((UI_Tabs *)_parent)->cSc = _geometryRBox->GetSelection();
   ((UI_Tabs *)_parent)->cMin = geomOpacitySlider->GetValue();
   ((UI_Tabs *)_parent)->cId = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////
void UI_GeometryTab::_onUpdate(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cGeo_state = 0;
   ((UI_Tabs *)_parent)->cPre_state = 1;
   for(int i = 0; i < ((UI_Tabs *)_parent)->num_geo; i++)
   {
      if ( _geometryCBox->IsChecked( i ) )
         ((UI_Tabs *)_parent)->cGeo_state += (int)pow( 2.0f, (float)i );
   }
   std::cout << " UI_GeometryTab::_onUpdate : " << 
         ((UI_Tabs *)_parent)->cGeo_state << std::endl;
   ((UI_Tabs *)_parent)->cId  = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}



