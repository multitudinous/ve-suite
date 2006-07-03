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
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/SoundsPane.h"
#include "VE_Open/XML/DOMDocumentManager.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"

#include <wx/button.h>
#include <wx/checklst.h>
#include <wx/sizer.h>
#include <wx/msgdlg.h>
#include <iostream>
#include <cmath>

BEGIN_EVENT_TABLE(SoundsPane, wxDialog)
   //EVT_CHECKLISTBOX(SOUND_CBOX,SoundsPane::_onSounds)
   EVT_BUTTON(SOUND_UPDATE_BUTTON,SoundsPane::_onUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
SoundsPane::SoundsPane( VjObs_ptr veEngine, VE_XML::DOMDocumentManager* domManagerIn )
:wxDialog(NULL,-1, wxString("Sounds Pane"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   _soundCBox = 0;
   _updateButton = 0;

   _buildPage();
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void SoundsPane::_buildPage()
{
   //the check box list
   int numSounds = num_sounds;
   int numCheckboxes = numSounds;
   wxString* defaultName;
   
   if ( numSounds > 0 )
   {
      defaultName = new wxString[ numSounds ];
      for(CORBA::ULong i = 0; i < (unsigned int)numSounds; i++)
      {
         defaultName[ i ] = soundNameArray[ i ];
         std::cout << "Sound Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      // create a dummy checkbox when there are no sounds available...
      numCheckboxes = 1;
      defaultName = new wxString[ 1 ];
      defaultName[ 0 ] = wxT("No Sound Files");
   }

   _soundCBox = new wxCheckListBox( this, SOUND_CBOX, wxDefaultPosition, 
                                    wxDefaultSize, numCheckboxes, defaultName, 
                                    0, wxDefaultValidator,
                                    wxT("Sound Files") );
/*
   // Used to initialize all the checkboxes on
   for ( int j = 0; j < numSounds; j++ )
   {
      _soundCBox->Check( j );
   }
*/

   if ( numSounds == 0 )
   {
      _soundCBox->Enable( false );
   }

   //the update button
   _updateButton = new wxButton(this,SOUND_UPDATE_BUTTON,wxT("Update"));

   //the panel sizer
   wxBoxSizer* soundPanelGroup = new wxBoxSizer(wxVERTICAL);
   soundPanelGroup->Add(_soundCBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   soundPanelGroup->Add(_updateButton,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(soundPanelGroup);
}

//////////////////
//event handling//
///////////////////

void SoundsPane::_onSounds(wxCommandEvent& WXUNUSED(event))
{
}

void SoundsPane::_onUpdate(wxCommandEvent& WXUNUSED(event))
{
   cIso_value = 0;
   for ( int i = 0; i < num_sounds; i++ )
   {
      if ( _soundCBox->IsChecked( i ) )
         cIso_value += (int)pow( 2.0f, (float)i );
   }
   std::cout << cIso_value << std::endl;
   dataValueName = "UPDATE_SOUNDS";
   SendCommandsToXplorer();
}

void SoundsPane::SetCommInstance( VjObs_ptr veEngine )
{
   xplorerPtr = VjObs::_duplicate( veEngine );
}
//////////////////////////////////////////////////
void SoundsPane::SendCommandsToXplorer( void )
{
     // Now need to construct domdocument and populate it with the new vecommand
   domManager->CreateCommandDocument("Command");
   doc = domManager->GetCommandDocument();

   // Create the command and data value pairs
   VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair( std::string("FLOAT") );
   dataValuePair->SetOwnerDocument(doc);
   dataValuePair->SetDataName( dataValueName );
   dataValuePair->SetDataValue( static_cast<double>(cIso_value) );
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetOwnerDocument(doc);
   veCommand->SetCommandName( std::string("Sound_Data") );
   veCommand->AddDataValuePair( dataValuePair );
   doc->getDocumentElement()->appendChild( veCommand->GetXMLData( "vecommand" ) );

   // New need to destroy document and send it
   std::string commandData = domManager->WriteAndReleaseCommandDocument();
   char* tempDoc = new char[ commandData.size() + 1 ];
   tempDoc = CORBA::string_dup( commandData.c_str() );

   if ( !CORBA::is_nil( xplorerPtr ) && !commandData.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         xplorerPtr->SetCommandString( tempDoc );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
         delete [] tempDoc;
      }
   }
   else
   {
      delete [] tempDoc;
   }
   //Clean up memory
   delete veCommand;
}
