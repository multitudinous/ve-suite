/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: UI_TeacherTab.cpp,v $
 * Date modified: $Date: 2006-01-10 11:21:30 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3470 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/UI_TeacherTab.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/CORBAServiceList.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include "VE_Open/skel/VjObsS.h"

#include <iostream>

#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/msgdlg.h>


BEGIN_EVENT_TABLE(UI_TeacherTab, wxDialog )
   EVT_RADIOBOX(TEACHER_RBOX,UI_TeacherTab::_onTeacher)
   EVT_BUTTON( RECORD_SCENE,UI_TeacherTab::_onClear)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_TeacherTab::UI_TeacherTab(wxWindow* tControl)
:wxDialog(tControl, -1, wxString("Stored Scenes"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP) 
{
   _parent = tControl;
   _teacherRBox = 0;

   _buildPage();

   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( displaySize.GetWidth() * 0.667f, 125, displaySize.GetWidth() * 0.333f, 500 );
   this->SetSize( dialogPosition );
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void UI_TeacherTab::_buildPage()
{
   //the radio box
   // add one for defualt button for no pfbs loaded
   int numStoredScenes = 1;
   VjObs::scalar_p_var fileNames;

   if ( dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->IsConnectedToXplorer() )
   {
      fileNames = dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->GetXplorerPointer()->get_teacher_name();
      numStoredScenes = fileNames->length();
   }   
   
   wxString* defaultName;

   if ( numStoredScenes > 1 )
   {
      defaultName = new wxString[ numStoredScenes ];
      defaultName[ 0 ] = wxT("No PFB Files Selected");

      for(CORBA::ULong i = 1; i < (unsigned int)numStoredScenes; ++i )
      {
         defaultName[ i ] = fileNames[ i - 1 ];
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

   if ( numStoredScenes == 1 )
   {
      _teacherRBox->Enable( false );
      //_teacherRBox->Enable( false );
   }

   // Clear button
   //_clearButton = new wxButton(this, TEACHER_CLEAR_BUTTON, wxT("Clear PFB Files"));

   //the panel sizer
   wxBoxSizer* teacherPanelGroup = new wxBoxSizer(wxVERTICAL);
   teacherPanelGroup->Add(_teacherRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   wxBoxSizer* buttonPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   wxButton* _closeButton = new wxButton( this, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   buttonPanelGroup->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   wxButton* recordButton = new wxButton( this, RECORD_SCENE, _T("Record Scene"), wxDefaultPosition, wxDefaultSize, 0 );
   buttonPanelGroup->Add(recordButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

   teacherPanelGroup->Add( buttonPanelGroup,0, wxALIGN_CENTER_VERTICAL|wxALL, 5);

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
   std::string dataValueName;
   if ( _teacherRBox->GetSelection() == 0 )
   {
      dataValueName = "CLEAR_PFB_FILE";
   }
   else
   {
      dataValueName = "LOAD_PFB_FILE";
   }
   //This assumes that the command name was set by the callback
   //as well as the DataValuePairs
   VE_XML::Command* veCommand = new VE_XML::Command();

   std::string _commandName = "Stored Scenes";
   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( std::string("UNSIGNED INT") );
   unsigned int sceneId = _teacherRBox->GetSelection() - 1;
   dataValuePair->SetDataName( dataValueName );
   dataValuePair->SetDataValue( sceneId );

   veCommand->SetCommandName( _commandName );
   veCommand->AddDataValuePair( dataValuePair );

   try
   {
      // CORBA releases the allocated memory so we do not have to
      dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );
   }
   catch ( ... )
   {
      wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                     "Communication Failure", wxOK | wxICON_INFORMATION );
   }

   //Clean up memory
   delete veCommand;
}

void UI_TeacherTab::_onClear(wxCommandEvent& event)
{
   VE_XML::Command* veCommand = new VE_XML::Command();

   std::string _commandName = "Stored Scenes";
   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( std::string("UNSIGNED INT") );
   unsigned int sceneId = _teacherRBox->GetSelection() - 1;
   dataValuePair->SetDataName( "RECORD_SCENE" );
   dataValuePair->SetDataValue( sceneId );

   veCommand->SetCommandName( _commandName );
   veCommand->AddDataValuePair( dataValuePair );

   try
   {
      // CORBA releases the allocated memory so we do not have to
      dynamic_cast< AppFrame* >( wxGetApp().GetTopWindow() )->GetCORBAServiceList()->SendCommandStringToXplorer( veCommand );
   }
   catch ( ... )
   {
      wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                     "Communication Failure", wxOK | wxICON_INFORMATION );
   }
   //Clean up memory
   delete veCommand;
}
