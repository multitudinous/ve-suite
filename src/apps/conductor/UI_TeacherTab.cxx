/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/util/CORBAServiceList.h>
#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/radiobut.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/msgdlg.h>
#include <wx/filename.h>

#include <app/conductor/UI_TeacherTab.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <iostream>

BEGIN_EVENT_TABLE(UI_TeacherTab, wxDialog )
   EVT_RADIOBOX(TEACHER_RBOX,UI_TeacherTab::_onTeacher)
   EVT_BUTTON( RECORD_SCENE,UI_TeacherTab::_onClear)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_TeacherTab::UI_TeacherTab(wxWindow* tControl)
:wxDialog(tControl, -1, _("Stored Scenes"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP) 
{
   _parent = tControl;
   _teacherRBox = 0;

   //the panel sizer
   teacherPanelGroup = new wxBoxSizer(wxVERTICAL);

   _buildPage();

   wxBoxSizer* buttonPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   wxButton* _closeButton = new wxButton( this, wxID_OK, _T("Close"), wxDefaultPosition, wxDefaultSize, 0 );
   buttonPanelGroup->Add(_closeButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   
   wxButton* recordButton = new wxButton( this, RECORD_SCENE, _T("Record Scene"), wxDefaultPosition, wxDefaultSize, 0 );
   buttonPanelGroup->Add(recordButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   
   teacherPanelGroup->Add( buttonPanelGroup,0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   
   wxSize displaySize = ::wxGetDisplaySize();
   wxRect dialogPosition( static_cast< int >( displaySize.GetWidth() * 0.667f ), 
                          125, 
                          static_cast< int >( displaySize.GetWidth() * 0.333f ), 
                          500 
                        );
   this->SetSize( dialogPosition );
   
   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(teacherPanelGroup);   
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

   if ( VE_Conductor::CORBAServiceList::instance()->IsConnectedToXplorer() )
   {
      fileNames = VE_Conductor::CORBAServiceList::instance()->GetXplorerPointer()->get_teacher_name();
      numStoredScenes = fileNames->length() + 1;
   }   
   
    wxString* defaultName;
    defaultName = new wxString[ numStoredScenes ];
    defaultName[ 0 ] = wxT("No Stored Scenes Selected");

    for( CORBA::ULong i = 1; i < (unsigned int)numStoredScenes; ++i )
    {
        defaultName[ i ] = wxString( fileNames[ i - 1 ], wxConvUTF8 );
        //std::cout << "PFB  Name " << i << " : " << defaultName[ i ] << std::endl;
    }

   if( _teacherRBox )
   {
      teacherPanelGroup->Detach( _teacherRBox );
      _teacherRBox->Destroy();
      //delete _teacherRBox;
      _teacherRBox = 0;
   }
   
   _teacherRBox = new wxRadioBox(this, TEACHER_RBOX, wxT( "Stored Scenes" ),
                                wxDefaultPosition, wxDefaultSize, numStoredScenes,
                                defaultName,1 , wxRA_SPECIFY_COLS);
   teacherPanelGroup->Insert(0,_teacherRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight() +1);
   temp.SetWidth( temp.GetWidth()+1 );
   SetSize( temp );      
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
      VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   }
   catch ( ... )
   {
      wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."), 
                     _("Communication Failure"), wxOK | wxICON_INFORMATION );
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
      VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
      _buildPage();
   }
   catch ( ... )
   {
      wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."), 
                     _("Communication Failure"), wxOK | wxICON_INFORMATION );
   }
   //Clean up memory
   delete veCommand;
}
