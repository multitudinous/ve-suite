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
 * File:          $RCSfile: UI_VertTab.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/VE_UI/UI_VertTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"

#include <wx/radiobox.h>
#include <wx/button.h>
#include <wx/slider.h>
#include <wx/notebook.h>
#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>

BEGIN_EVENT_TABLE(UI_VertTab, wxPanel)
   EVT_RADIOBOX      (PARTICLE_OPTIONS_RBOX,    UI_VertTab::_onParticleOption)
   EVT_BUTTON        (DISPLAY_PARTICLE_BUTTON,  UI_VertTab::_onDisplayParticle)
   EVT_COMMAND_SCROLL(SPHERE_POINT_SIZE_SLIDER, UI_VertTab::_onSpherePointSizeSlider)
END_EVENT_TABLE()

////////////////////////////////////////////////////////
//Constructor                                         //
////////////////////////////////////////////////////////
UI_VertTab::UI_VertTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _particleOptionRBox = 0;
   _displayParticlesButton = 0;
   _spherePointSizeSlider = 0;   
   _parent = tControl;
   _buildPage();
}

///////////////////////////////////
//Build the Vertex Data Tab      //
///////////////////////////////////
void UI_VertTab::_buildPage()
{
   //the box for the first group
   wxBoxSizer* sliderGroup = new wxBoxSizer(wxHORIZONTAL);

   //the labels for the Particle Option Slider
   wxStaticText* sliderLabel = new wxStaticText(this,-1,wxT("Sphere/Point Size"));

   //making the slider
   wxSize slidesize(150,300);
   _spherePointSizeSlider = new wxSlider(this, SPHERE_SIZE_SLIDER,50,0,100,
                                wxDefaultPosition, slidesize,
                                wxSL_VERTICAL|
                                wxSL_AUTOTICKS|
                                wxSL_LABELS );

   //sizers for the slider and label
   wxBoxSizer* leftGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* rightGroup = new wxBoxSizer(wxVERTICAL);

   //add the slider and slider label now
   leftGroup->Add(sliderLabel,0,wxALIGN_CENTER_HORIZONTAL);
   leftGroup->Add(_spherePointSizeSlider,1,wxALIGN_CENTER_HORIZONTAL);

   sliderGroup->Add(leftGroup,1,wxALIGN_LEFT|wxEXPAND);
   sliderGroup->Add(rightGroup,1,wxALIGN_RIGHT|wxEXPAND);

   //create the radio box for the Particle Options
   //Particle Option radio box
   wxString particleOption[] = {wxT("View as a point cloud"),
                            wxT("View as variably sized spheres")};

   _particleOptionRBox = new wxRadioBox(this,PARTICLE_OPTION_RBOX,
                                wxT("Cursor Selection"),
                                wxDefaultPosition, wxDefaultSize,
                                2, particleOption, 1,
                                wxRA_SPECIFY_COLS);

   //the buttons and check box for the bottom of the UI
   _displayParticlesButton = new wxButton(this, DISPLAY_PARTICLES_BUTTON,
                                    wxT("Display Particles"));

   //group the sliders and the labels together
   /*wxBoxSizer* propGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* intGroup = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* stepGroup = new wxBoxSizer(wxVERTICAL);*/

   ////////////////////////////////////////////////////////////////////
   //the layout                                                      //
   //heirarchy                                                       //
   //main group                                                      //
   //   row 1                                                        //
   //        2 groups - slider group, radio box group                //
   //   row 2                                                        // 
   //        1 group - 1 button                                      //
   ////////////////////////////////////////////////////////////////////

   //the main group
   wxBoxSizer* streamPanelGroup = new wxBoxSizer(wxVERTICAL);  

   //the two rows
   wxBoxSizer* row1 = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* row2 = new wxBoxSizer(wxHORIZONTAL);

   //we can add the button to the last row now 
   row2->Add(_displayParticlesButton,1,wxALIGN_CENTER_HORIZONTAL);

   //the radio box group
   wxBoxSizer* radioBoxGroup = new wxBoxSizer(wxVERTICAL); 
   radioBoxGroup->Add(_particleOptionRBox,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND);

   //now add the groups to the first row
   row1->Add(sliderGroup,1,wxEXPAND|wxALIGN_LEFT);
   row1->Add(radioBoxGroup,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
  
   //add the rows to the main panel
   streamPanelGroup->Add(row1,1,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 
   streamPanelGroup->Add(row2,0,wxALIGN_CENTER_HORIZONTAL|wxEXPAND); 

   //set this flag and let wx handle alignment
   SetAutoLayout(true);

   //assign the group to the panel
   SetSizer(streamPanelGroup);   
}
//////////////////////
//Event handling    //
//////////////////////

//////////////////////////////////////////////////////////////
void UI_VertTab::_onParticleOption(wxCommandEvent& WXUNUSED(event))
{
}

///////////////////////////////////////////////////////////
void  UI_VertTab::_onDisplayParticle(wxCommandEvent& WXUNUSED(event))
{
   ((UI_Tabs *)_parent)->cId  = CHANGE_PARTICLE_VIEW_OPTION;
   ((UI_Tabs *)_parent)->cGeo_state = _particleOptionRBox->GetSelection();
   ((UI_Tabs *)_parent)->cIso_value = _spherePointSizeSlider->GetValue();
   ((UI_Tabs *)_parent)->sendDataArrayToServer();

}

//////////////////////////////////////////////////////////////
void UI_VertTab::_onSpherePointSizeSlider(wxScrollEvent& WXUNUSED(event))
{
}

