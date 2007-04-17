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
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Framework/SoundsPane.h"
#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/ParameterBlock.h"

#include <wx/button.h>
#include <wx/checklst.h>
#include <wx/sizer.h>
#include <wx/msgdlg.h>
#include <wx/filedlg.h>
#include <wx/filename.h>
#include <iostream>
#include <cmath>

BEGIN_EVENT_TABLE(SoundsPane, wxDialog)
   EVT_CHECKLISTBOX(SOUND_CBOX,SoundsPane::_onSounds)
   EVT_BUTTON(SOUND_LOAD_BUTTON,SoundsPane::_onLoadAndUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
SoundsPane::SoundsPane( VE_XML::VE_Model::Model* activeModel)
:wxDialog(NULL,-1, _("Sounds Pane"), 
		  wxDefaultPosition, wxDefaultSize, 
		  (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX) & ~ wxSTAY_ON_TOP)
{
   _soundCBox = 0;
   _loadButton = 0;
   _numSounds = 0;
   _activeModel = activeModel;

   _buildPage();
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
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
//////////////////
//event handling//
//////////////////
/////////////////////////////////////////////////
void SoundsPane::_onSounds(wxCommandEvent& event)
{
   int checkSound = event.GetSelection();
   unsigned int onOff = (_soundCBox->IsChecked(checkSound))?1:0;
   ///turn on/off sound
   ///send commands to xplorer
   VE_XML::Command* veCommand = new VE_XML::Command();
   veCommand->SetCommandName("SOUNDS_ENABLE");
   
   VE_XML::DataValuePair* soundName = new VE_XML::DataValuePair();
   soundName->SetData( ConvertUnicode( _soundCBox->GetStringSelection()), onOff );
   veCommand->AddDataValuePair(soundName);
   
   VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
   delete veCommand; 
}
//////////////////////////////////////////////////////////////////
void SoundsPane::_onLoadAndUpdate(wxCommandEvent& WXUNUSED(event))
{
   /*
   cIso_value = 0;
   for ( int i = 0; i < num_sounds; i++ )
   {
      if ( _soundCBox->IsChecked( i ) )
         cIso_value += (int)pow( 2.0f, (float)i );
   }
   std::cout << cIso_value << std::endl;
   dataValueName = "UPDATE_SOUNDS";*/
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
         if(_ensureSounds(fileNamesVector.Item(i)))
         { 
            wxFileName soundFileName( fileNamesVector.Item(i).c_str());
            _soundCBox->Append(soundFileName.GetName());

            VE_XML::ParameterBlock* modelSounds = 0;
            modelSounds = _activeModel->GetInformationPacket("Model Sounds");
            
            VE_XML::Command* veCommand = new VE_XML::Command();
            veCommand->SetCommandName("SOUNDS_LOAD_NEW");
           
            VE_XML::DataValuePair* soundProperty = modelSounds->GetProperty(-1);
            soundProperty->SetData( ConvertUnicode( soundFileName.GetName().c_str() ), ConvertUnicode( fileNamesVector.Item(i).c_str() ) );
            veCommand->AddDataValuePair(soundProperty);
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
void SoundsPane::SetActiveModel(VE_XML::VE_Model::Model* model)
{
   _activeModel = model;
   if(_activeModel)
   {
      _updateSoundsInformationFromModel();
   }
}
////////////////////////////////////////////////////
void SoundsPane::_updateSoundsInformationFromModel()
{
   if(!_activeModel)
      return;

   _clearLoadedSounds();
   
   int numCheckboxes = 1;
   VE_XML::ParameterBlock* modelSounds = 0;
   modelSounds = _activeModel->GetInformationPacket("Model Sounds");
   ///create the model sounds
   if(!modelSounds)
   {
      modelSounds = _activeModel->GetInformationPacket(-1);
      modelSounds->SetName("Model Sounds");
   }
   _numSounds = modelSounds->GetNumberOfProperties();

   if ( _numSounds > 0 )
   {
      //defaultName.push_back new wxString[ _numSounds ];
      for(unsigned int i = 0; i < _numSounds; ++i)
      {
         _loadedSounds.push_back(wxString( modelSounds->GetProperty(i)->GetDataName().c_str(), wxConvUTF8));
      }
      numCheckboxes = _numSounds;
   }
   else
   {
      // create a dummy checkbox when there are no sounds available...
      numCheckboxes = 1;
      _loadedSounds.push_back(wxT("No Sound Files"));
   }
   if(_soundCBox)
   {
      _soundCBox->Set(_loadedSounds);
   }
   else
   {
      _soundCBox = new wxCheckListBox( this, SOUND_CBOX, wxDefaultPosition, 
                                    wxDefaultSize, _loadedSounds, 
                                    0, wxDefaultValidator,
                                    wxT("Sound Files") );
      if ( _numSounds == 0 )
      {
         _soundCBox->Enable( false );
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
