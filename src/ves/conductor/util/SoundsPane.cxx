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
// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/util/SoundsPane.h>

#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/ParameterBlock.h>

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/button.h>
#include <wx/checklst.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/filename.h>

// -- C/C++ Libraries --- //
#include <iostream>
#include <cmath>

BEGIN_EVENT_TABLE( SoundsPane, wxDialog )
    EVT_CHECKLISTBOX( SOUND_CBOX, SoundsPane::_onSounds )
    EVT_BUTTON( SOUND_LOAD_BUTTON, SoundsPane::_onLoadAndUpdate )
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
SoundsPane::SoundsPane( VE_XML::VE_Model::ModelWeakPtr activeModel )
:
wxDialog(NULL,-1, _("Sounds Pane"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   _soundCBox = 0;
   _loadButton = 0;
   _numSounds = 0;
   m_activeModel = activeModel;

   _buildPage();
}
/////////////////////////////////////////////////
void SoundsPane::_buildPage()
{
   //the check box list
   _updateSoundsInformationFromModel();

   //the update button
   _loadButton = new wxButton(this,SOUND_LOAD_BUTTON,wxT("Load"));

   //the panel sizer
   wxBoxSizer* soundPanelGroup = new wxBoxSizer(wxVERTICAL);
   soundPanelGroup->Add(_soundCBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   wxBoxSizer* buttonRow = new wxBoxSizer(wxHORIZONTAL);
   buttonRow->Add(_loadButton,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   buttonRow->Add(new wxButton(this,wxID_OK,wxT("OK")),1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   soundPanelGroup->Add(buttonRow,0,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(soundPanelGroup);
}
/////////////////////////////////////////////////
void SoundsPane::_onSounds( wxCommandEvent& event )
{
   int checkSound = event.GetSelection();
   unsigned int onOff = (_soundCBox->IsChecked(checkSound))?1:0;
   ///turn on/off sound
   ///send commands to xplorer
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName("Enable/Disable Sound");
   
   VE_XML::DataValuePair* soundName = new VE_XML::DataValuePair();
   soundName->SetData( "Sound Name",ConvertUnicode( _soundCBox->GetString(checkSound)));
   veCommand->AddDataValuePair(soundName);
   
   VE_XML::DataValuePair* status = new VE_XML::DataValuePair();
   status->SetData( "Status",onOff);
   veCommand->AddDataValuePair(status);
   
   VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand; 
}
//////////////////////////////////////////////////////////////////
void SoundsPane::_onLoadAndUpdate(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog(this,
                       _T("Open Sound File"), 
                       _T(""), 
                       _T(""),
                       _T("Wave files (*.wav)|*.wav;|MP3 files (*.mp3)|*.mp3;|All Files (*.*)|*.*"),
                       wxOPEN|wxFILE_MUST_EXIST|wxMULTIPLE,//|wxCHANGE_DIR, 
                       wxDefaultPosition);
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxArrayString fileNamesVector;
      dialog.GetPaths( fileNamesVector );
      
      for ( size_t i = 0; i < fileNamesVector.GetCount(); ++i )
      {
         wxFileName soundFileName( fileNamesVector.Item(i).c_str());
         if(_ensureSounds(soundFileName.GetName()))
         { 
            _soundCBox->Append(soundFileName.GetName());

            VE_XML::ParameterBlock* modelSounds = 0;
            modelSounds = m_activeModel->GetInformationPacket("Model Sounds");
            
            VE_XML::Command* veCommand = new VE_XML::Command();
            veCommand->SetCommandName("Add New Sound");

            //Have to do this so because of our current memory scheme in XML...
            //This will change once we switch to smart pointers
            modelSounds->GetProperty(-1)->SetData( 
                           ConvertUnicode( soundFileName.GetName().c_str() ), 
                           ConvertUnicode( fileNamesVector.Item(i).c_str() ) );

            VE_XML::DataValuePair* soundName = new VE_XML::DataValuePair();
            soundName->SetData("Sound Name", 
                            ConvertUnicode( soundFileName.GetName().c_str() ) );
            veCommand->AddDataValuePair(soundName);

            VE_XML::DataValuePair* soundFilename = new VE_XML::DataValuePair();
            soundFilename->SetData( "Sound Filename",
                            ConvertUnicode( fileNamesVector.Item(i).c_str() ) );
            veCommand->AddDataValuePair(soundFilename);
            
            VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
            delete veCommand; 
         }
      }
   }
}
/////////////////////////////////////////////////
bool SoundsPane::_ensureSounds(wxString filename)
{
   for(unsigned int i = 0; i < _numSounds; ++i)
   {
      if(filename == _loadedSounds[i])
      {
         ///already loaded this file
         wxMessageDialog promptDlg( this, 
                                    _("Sound file already loaded!"), 
                                    _("Sound Load Error"), 
                                    wxICON_ERROR, 
                                    wxDefaultPosition);
         promptDlg.ShowModal();
         return false;
      }
   }
   
   ///add the file to the list
   _loadedSounds.push_back(filename);
   _numSounds = _loadedSounds.size();
   _soundCBox->Enable( true);
   return true;
}
///////////////////////////////////////////////////////////////
void SoundsPane::SetActiveModel(VE_XML::VE_Model::ModelWeakPtr model)
{
    m_activeModel = model;
    _updateSoundsInformationFromModel();
}
////////////////////////////////////////////////////
void SoundsPane::_updateSoundsInformationFromModel()
{
   if(!m_activeModel)
      return;

   _clearLoadedSounds();
   
   int numCheckboxes = 1;
   VE_XML::ParameterBlock* modelSounds = 0;
   modelSounds = m_activeModel->GetInformationPacket("Model Sounds");
   ///create the model sounds
   if(!modelSounds)
   {
      modelSounds = m_activeModel->GetInformationPacket(-1);
      modelSounds->SetName("Model Sounds");
   }
   _numSounds = modelSounds->GetNumberOfProperties();

   if ( _numSounds > 0 )
   {
      //defaultName.push_back new wxString[ _numSounds ];
      for(unsigned int i = 0; i < _numSounds; ++i)
      {
         std::string soundName = modelSounds->GetProperty(i)->GetDataName();
         if(soundName.empty())
         {
            _numSounds = 0;
            ///Some kind of error.
            break;
         }

         VE_XML::Command* veCommand = new VE_XML::Command();
         veCommand->SetCommandName("Add New Sound");
           
         VE_XML::DataValuePair* soundNameDVP = new VE_XML::DataValuePair();
         soundNameDVP->SetData("Sound Name",soundName );
         veCommand->AddDataValuePair(soundNameDVP);

         std::string fileName;
         modelSounds->GetProperty(i)->GetData(fileName);
         VE_XML::DataValuePair* soundFilename = new VE_XML::DataValuePair();
         soundFilename->SetData( "Sound Filename", fileName );
         veCommand->AddDataValuePair(soundFilename);

         _loadedSounds.push_back(wxString( soundName.c_str(), wxConvUTF8));
            
         VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
         delete veCommand; 
      }
   }
   else
   {
      // create a dummy checkbox when there are no sounds available...
      _loadedSounds.push_back(wxT("No Sound Files"));
   }
   if(_soundCBox)
   {
      _soundCBox->Set(_loadedSounds);
      _soundCBox->Enable( true );
   }
   else
   {
      if ( _numSounds == 0 )
      {
         wxString noFiles("No Sound Files", wxConvUTF8);
         _soundCBox = new wxCheckListBox( this, SOUND_CBOX, wxDefaultPosition, 
                                    wxDefaultSize, 1,&noFiles, 
                                    0, wxDefaultValidator,
                                    wxT("Sound Files") );
         _soundCBox->Enable( false );
      }
      else
      {
         _soundCBox = new wxCheckListBox( this, SOUND_CBOX, wxDefaultPosition, 
                                          wxDefaultSize, _loadedSounds, 
                                          0, wxDefaultValidator,
                                          wxT("Sound Files") );
         _soundCBox->Enable( true );
      }
   }
}
///////////////////////////////////////////////////////////////
void SoundsPane::_loadSoundsInXplorer( wxString soundFileName )
{
   //send the load over to xplorer
   ///SendCommandsToXplorer();
}
/////////////////////////////////////
void SoundsPane::_clearLoadedSounds()
{
   _loadedSounds.Clear();
   //send the command to xplorer
}
//////////////////////////////////////////////
void SoundsPane::SendCommandsToXplorer( void )
{

}
