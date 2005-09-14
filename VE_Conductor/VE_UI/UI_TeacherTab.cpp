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
#include "VE_Conductor/VE_UI/UI_TeacherTab.h"
#include "VE_Conductor/VE_UI/UI_Tabs.h"
#include "VE_Xplorer/cfdEnum.h"
#include <iostream>

#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>

BEGIN_EVENT_TABLE(UI_TeacherTab, wxPanel)
   EVT_RADIOBOX(TEACHER_RBOX,UI_TeacherTab::_onTeacher)
   //EVT_BUTTON(TEACHER_CLEAR_BUTTON,UI_TeacherTab::_onClear)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_TeacherTab::UI_TeacherTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;
   _teacherRBox = 0;

   _buildPage();
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void UI_TeacherTab::_buildPage()
{
   //the radio box
   // add one for defualt button for no pfbs loaded
   int numStoredScenes = ((UI_Tabs *)_parent)->num_teacher + 1;
   wxString* defaultName;

   if ( numStoredScenes > 1 )
   {
      defaultName = new wxString[ numStoredScenes ];
      defaultName[ 0 ] = wxT("No PFB Files Selected");

      for(CORBA::ULong i = 1; i < (unsigned int)numStoredScenes; ++i )
      {
         defaultName[ i ] = ((UI_Tabs*)_parent)->teacher_attrib[ i - 1 ];
         std::cout << "PFB  Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      defaultName = new wxString[ numStoredScenes ];
      defaultName[ 0 ] = wxT("No PFB Files");
   }

   _teacherRBox = new wxRadioBox(this, TEACHER_RBOX, wxT("PFB Files"),
                                wxDefaultPosition, wxDefaultSize, numStoredScenes,
                                defaultName,1 , wxRA_SPECIFY_COLS);

   if ( ((UI_Tabs *)_parent)->num_teacher == 0 )
   {
      _teacherRBox->Enable( false );
   }

   // Clear button
   //_clearButton = new wxButton(this, TEACHER_CLEAR_BUTTON, wxT("Clear PFB Files"));

   //the panel sizer
   wxBoxSizer* teacherPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   teacherPanelGroup->Add(_teacherRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   //teacherPanelGroup->Add(_clearButton,0,wxALIGN_RIGHT);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(teacherPanelGroup);
}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_TeacherTab::_onTeacher(wxCommandEvent& WXUNUSED(event))
{
   // Are there any stored scenes loaded?
   if ( ((UI_Tabs *)_parent)->num_teacher > 0 )
   {
      // We must use this if statement becuase the first value
      // is no selected pfb...see above
      if ( _teacherRBox->GetSelection() == 0 )
      {
         ((UI_Tabs *)_parent)->cId = CLEAR_PFB_FILE;
      }
      else
      {
         ((UI_Tabs *)_parent)->cIso_value = _teacherRBox->GetSelection() - 1;
         ((UI_Tabs *)_parent)->cId = LOAD_PFB_FILE;
      }
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
   else
   {
      std::cout << "There are no stored scenes loaded to send!" << std::endl;
   }
}
/*
void UI_TeacherTab::_onClear(wxCommandEvent& event)
{
   event.GetInt();
   ((UI_Tabs *)_parent)->cId = CLEAR_PFB_FILE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
*/
