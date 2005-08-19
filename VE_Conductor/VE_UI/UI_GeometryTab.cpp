/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_GeometryTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>
#include <string>
#include <cmath>

BEGIN_EVENT_TABLE(UI_GeometryTab, wxPanel)
   EVT_COMMAND_SCROLL(GEOMETRY_OPACITY_SLIDER, UI_GeometryTab::ChangeOpacity)
   //EVT_COMMAND_SCROLL_ENDSCROLL( GEOMETRY_OPACITY_SLIDER, UI_GeometryTab::ChangeOpacity )
   EVT_COMMAND_SCROLL( GEOMETRY_LOD_SLIDER, UI_GeometryTab::_onGeometry )
   //EVT_COMMAND_SCROLL_ENDSCROLL( GEOMETRY_LOD_SLIDER, UI_GeometryTab::_onGeometry )
   EVT_CHECKLISTBOX( GEOMETRY_CBOX,UI_GeometryTab::_onUpdate )
   //EVT_RADIOBOX( GEOMETRY_CONFIG_RBOX, UI_GeometryTab::ChangeOpacity )
   //EVT_COMBOBOX( GEOMETRY_SELECT_COMBO, UI_GeometryTab::OpacityFileSelection )
   EVT_LISTBOX( GEOMETRY_SELECT_COMBO, UI_GeometryTab::OpacityFileSelection )
END_EVENT_TABLE()

////////////////////////////////////////////////////
//Constructor                                     //
////////////////////////////////////////////////////
UI_GeometryTab::UI_GeometryTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _geometryRBox = 0;
   _geometryCBox = 0;
   _updateButton = 0;
   geomOpacitySlider = 0;
   geomLODSlider = 0;
   _parent = tControl;
   geometryCombo = 0;
   opacityMemory.clear();
   _buildPage();
}

////////////////////////////////////////////////////
//Build the Geometry Tab                          //
////////////////////////////////////////////////////
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
         opacityMemory.push_back( 100 );
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

   /*geometryCombo = new wxComboBox(this, GEOMETRY_SELECT_COMBO, wxT("Set Active Geometry File"),
                                    wxDefaultPosition, wxDefaultSize,
                                    numGeoms,defaultName, wxCB_DROPDOWN);*/
   wxStaticBox* geomFiles = new wxStaticBox(this,-1, wxT("Geometry Files"));
   wxStaticBoxSizer* geomFilesGroup = new wxStaticBoxSizer(geomFiles,wxVERTICAL);   
   _geometryCBox = new wxCheckListBox( this, GEOMETRY_CBOX,  
                                    wxDefaultPosition, wxDefaultSize, 
                                    numGeoms, defaultName );
   geomFilesGroup->Add(_geometryCBox,1,wxALIGN_LEFT|wxEXPAND); 
   wxBoxSizer* radioAndCheckBoxes = new wxBoxSizer( wxHORIZONTAL );
   radioAndCheckBoxes->Add(geomFilesGroup,1,wxALIGN_LEFT|wxEXPAND|wxALL, 5);
   //radioAndCheckBoxes->Add(_geometryRBox,0,wxALIGN_RIGHT|wxEXPAND|wxALL, 5);

   // Used to initialize all the checkboxes on
   for(int j = 0; j < numGeoms; j++)
   {
      if ( ((UI_Tabs *)_parent)->num_geo == 0 )
      {
         _geometryCBox->Check( j, true );
      }
      else
         _geometryCBox->Check( j, (bool)(*((UI_Tabs*)_parent)->geomFileSettings.at( j )) );
   }
 
   // slider info
   //the labels for the sliders
   wxStaticText* opacityLabel = new wxStaticText(this, -1, wxT("Geometry Opacity"));
   wxStaticText* opacityLabelLeft = new wxStaticText(this, -1, wxT("Transparent"));
   wxStaticText* opacityLabelRight = new wxStaticText(this, -1, wxT("Opaque"));
   
   wxStaticText* lodLabel = new wxStaticText(this, -1, wxT("Geometry LOD Control"));
   wxStaticText* lodLabelLeft = new wxStaticText(this, -1, wxT("Higher Detail"));
   wxStaticText* lodLabelRight = new wxStaticText(this, -1, wxT("Lower Detail")); 

   //opacity slider
   geomOpacitySlider = new wxSlider(this, GEOMETRY_OPACITY_SLIDER,100,0,100,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_LABELS );

   //lod slider
   geomLODSlider = new wxSlider(this, GEOMETRY_LOD_SLIDER,0,0,1000,
                                       wxDefaultPosition, wxDefaultSize,
                                       wxSL_HORIZONTAL|
                                       wxSL_LABELS );

   //two sizers to group the sliders and their lables
   wxBoxSizer* opacityGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* opacityLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   wxBoxSizer* lodGroup = new wxBoxSizer( wxVERTICAL );
   wxBoxSizer* lodLabelBottom = new wxBoxSizer( wxHORIZONTAL );

   opacityGroup->Add(opacityLabel,0,wxALIGN_LEFT);
   opacityGroup->Add(geomOpacitySlider,1,wxALIGN_LEFT|wxEXPAND);
   opacityLabelBottom->Add(opacityLabelLeft,6,wxALIGN_LEFT);
   opacityLabelBottom->Add(opacityLabelRight,0,wxALIGN_RIGHT);
   opacityGroup->Add(opacityLabelBottom,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);   

   lodGroup->Add(lodLabel,0,wxALIGN_LEFT);
   lodGroup->Add(geomLODSlider,1,wxALIGN_LEFT|wxEXPAND);
   lodLabelBottom->Add(lodLabelLeft,6,wxALIGN_LEFT);
   lodLabelBottom->Add(lodLabelRight,0,wxALIGN_RIGHT);
   lodGroup->Add(lodLabelBottom,0,wxALIGN_LEFT|wxEXPAND|wxALL);

   wxStaticBox* geomControls = new wxStaticBox(this,-1, wxT("Geometry Controls"));
   wxStaticBoxSizer* geomControlsGroup = new wxStaticBoxSizer(geomControls,wxVERTICAL);
   //the panel sizer
   //geometryPanelGroup->Add(_geometryRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   geomControlsGroup->Add(radioAndCheckBoxes,4,wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
   //geomControlsGroup->Add(geometryCombo,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
   geomControlsGroup->Add(opacityGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);
   geomControlsGroup->Add(lodGroup,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND|wxALL, 5);

   wxBoxSizer* geometryPanelGroup = new wxBoxSizer(wxVERTICAL);
   geometryPanelGroup->Add(geomControlsGroup,1,wxEXPAND|wxALL, 5);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(geometryPanelGroup);

   // Disable guis as neccessary
   if ( ((UI_Tabs *)_parent)->num_geo == 0 )
   {
      _geometryCBox->Enable( false );
      geomLODSlider->Enable( false );
      //geometryCombo->Enable( false );
      geomOpacitySlider->Enable( false );
   }

   SetSize( GetSize() );
   // Send lod info back to ve-xplorer
   if ( ((UI_Tabs *)_parent)->num_geo != 0 )
   {
      ((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
      ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
}

////////////////////////////////////////////////////
void UI_GeometryTab::_onGeometry( wxScrollEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cSc = geomLODSlider->GetValue();
   ((UI_Tabs *)_parent)->cId = CHANGE_LOD_SCALE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

////////////////////////////////////////////////////
void UI_GeometryTab::ChangeOpacity( wxScrollEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cPre_state = 0;
   for ( unsigned int i = 0; i < opacityMemory.size(); ++i )
   {
      if ( _geometryCBox->IsSelected( i ) )
      {
         ((UI_Tabs *)_parent)->cSc = i;
         ((UI_Tabs *)_parent)->cMin = geomOpacitySlider->GetValue();
         ((UI_Tabs *)_parent)->cId = UPDATE_GEOMETRY;
         ((UI_Tabs *)_parent)->sendDataArrayToServer();
         // Update Memory
         opacityMemory.at( ((UI_Tabs *)_parent)->cSc ) = geomOpacitySlider->GetValue();
         break;
      }
   }
}

////////////////////////////////////////////////////
void UI_GeometryTab::_onUpdate( wxCommandEvent& WXUNUSED(event) )
{
   ((UI_Tabs *)_parent)->cGeo_state = 0;
   ((UI_Tabs *)_parent)->cPre_state = 1;
   for(int i = 0; i < ((UI_Tabs *)_parent)->num_geo; i++)
   {
      if ( _geometryCBox->IsChecked( i ) )
      {
         ((UI_Tabs *)_parent)->cGeo_state += (int)pow( 2.0f, (float)i );
         (*((UI_Tabs*)_parent)->geomFileSettings.at( i )) = 1;
      }
      else
         (*((UI_Tabs*)_parent)->geomFileSettings.at( i )) = 0;
   }
   ((UI_Tabs *)_parent)->cId  = UPDATE_GEOMETRY;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

////////////////////////////////////////////////////
void UI_GeometryTab::OpacityFileSelection( wxCommandEvent& WXUNUSED(event) )
{
   // Initiallize opacity slider to last value for a particular file
   for ( unsigned int i = 0; i < opacityMemory.size(); ++i )
   {
      if ( _geometryCBox->IsSelected( i ) )
      {
         geomOpacitySlider->SetValue( opacityMemory.at( i ) );
         break;
      }
   }
}

