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
#include "VE_Conductor/VE_UI/UI_DesignParTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>
#include <cmath>

#include <wx/textctrl.h>
#include <wx/button.h>
#include <wx/sizer.h>
#include <wx/stattext.h>

BEGIN_EVENT_TABLE(UI_DesignParTab, wxPanel)
   EVT_BUTTON(DESIGNPAR_UPDATE_BUTTON,UI_DesignParTab::_onUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_DesignParTab::UI_DesignParTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;
   _updateButton = 0;

   _buildPage();
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void UI_DesignParTab::_buildPage()
{
   //the input boxes
   _param1 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param2 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param3 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param4 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param5 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param6 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param7 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);
   _param8 = new wxTextCtrl(this, -1, wxT("0"),wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator);

   //build the labels
   wxStaticText* param1Label = new wxStaticText(this, -1, wxT("param1 "));
   wxStaticText* param2Label = new wxStaticText(this, -1, wxT("param2 "));
   wxStaticText* param3Label = new wxStaticText(this, -1, wxT("param3 "));
   wxStaticText* param4Label = new wxStaticText(this, -1, wxT("param4 "));
   wxStaticText* param5Label = new wxStaticText(this, -1, wxT("param5 "));
   wxStaticText* param6Label = new wxStaticText(this, -1, wxT("param6 "));
   wxStaticText* param7Label = new wxStaticText(this, -1, wxT("param7 "));
   wxStaticText* param8Label = new wxStaticText(this, -1, wxT("param8 "));

   //attach the labels to the boxes
   wxBoxSizer* param1Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param2Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param3Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param4Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param5Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param6Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param7Group = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* param8Group = new wxBoxSizer(wxHORIZONTAL);

   param1Group->Add(param1Label,0,wxALIGN_CENTER_HORIZONTAL);
   param1Group->Add(_param1,0,wxALIGN_CENTER_HORIZONTAL);

   param2Group->Add(param2Label,0,wxALIGN_CENTER_HORIZONTAL);
   param2Group->Add(_param2,0,wxALIGN_CENTER_HORIZONTAL);

   param3Group->Add(param3Label,0,wxALIGN_CENTER_HORIZONTAL);
   param3Group->Add(_param3,0,wxALIGN_CENTER_HORIZONTAL);

   param4Group->Add(param4Label,0,wxALIGN_CENTER_HORIZONTAL);
   param4Group->Add(_param4,0,wxALIGN_CENTER_HORIZONTAL);

   param5Group->Add(param5Label,0,wxALIGN_CENTER_HORIZONTAL);
   param5Group->Add(_param5,0,wxALIGN_CENTER_HORIZONTAL);

   param6Group->Add(param6Label,0,wxALIGN_CENTER_HORIZONTAL);
   param6Group->Add(_param6,0,wxALIGN_CENTER_HORIZONTAL);

   param7Group->Add(param7Label,0,wxALIGN_CENTER_HORIZONTAL);
   param7Group->Add(_param7,0,wxALIGN_CENTER_HORIZONTAL);

   param8Group->Add(param8Label,0,wxALIGN_CENTER_HORIZONTAL);
   param8Group->Add(_param8,0,wxALIGN_CENTER_HORIZONTAL);

   //the update button
   _updateButton = new wxButton(this,DESIGNPAR_UPDATE_BUTTON,wxT("Update"));

   //the panel sizer
   wxBoxSizer* designparPanelGroup = new wxBoxSizer(wxVERTICAL);
   designparPanelGroup->Add(param1Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param2Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param3Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param4Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param5Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param6Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param7Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(param8Group,0,wxALIGN_CENTER_HORIZONTAL);
   designparPanelGroup->Add(_updateButton,0,wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(designparPanelGroup);
   
}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_DesignParTab::_onDesignPar(wxCommandEvent& WXUNUSED(event))
{
      //((UI_Tabs *)_parent)->cId = DESIGN_PARMS;
      //((UI_Tabs *)_parent)->sendDataArrayToServer();
}

//////////////////////////////////////////////////
void UI_DesignParTab::_onUpdate(wxCommandEvent& WXUNUSED(event))
{
   /*((UI_Tabs *)_parent)->cIso_value = 0;
   for(int i = 0; i < ((UI_Tabs *)_parent)->numSounds; i++)
   {
      if ( _soundCBox->IsChecked( i ) )
         ((UI_Tabs *)_parent)->cIso_value += (int)pow( 2.0f, (float)i );
   }
   std::cout << ((UI_Tabs *)_parent)->cIso_value <<std::endl;*/
   //((UI_Tabs *)_parent)->cId  = UPDATE_DESIGNPARMS;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();
}
