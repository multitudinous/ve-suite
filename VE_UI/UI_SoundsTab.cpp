#include "UI_SoundsTab.h"
#include "UI_Tabs.h"
#include <iostream>
#include <cmath>

BEGIN_EVENT_TABLE(UI_SoundTab, wxPanel)
   //EVT_RADIOBOX(SOUND_RBOX,UI_SoundTab::_onSounds)
   EVT_BUTTON(SOUND_UPDATE_BUTTON,UI_SoundTab::_onUpdate)
END_EVENT_TABLE()

///////////////
//Constructor//
///////////////
UI_SoundTab::UI_SoundTab(wxNotebook* tControl)
:wxPanel(tControl)
{
   _parent = tControl;
   _soundRBox = 0;
   _soundCBox = 0;
   _updateButton = 0;

   _buildPage();
}
//////////////////////////////
//build the sound tab       //
//////////////////////////////
void UI_SoundTab::_buildPage()
{
   //the radio box
   //wxString* defaultName;//[] ={wxT("default")};
   /*_soundRBox = new wxRadioBox(this, SOUND_RBOX, wxT("Sound Files"),
                                wxDefaultPosition, wxDefaultSize, 1,
                                defaultName,1 , wxRA_SPECIFY_COLS);*/

   //the radio box
   int numSounds = ((UI_Tabs *)_parent)->numSounds;
   wxString* defaultName;
   
   if ( numSounds > 0 )
   {
      defaultName = new wxString[ numSounds ];
      for(CORBA::ULong i = 0; i < (unsigned int)numSounds; i++)
      {
         defaultName[ i ] = ((UI_Tabs*)_parent)->soundNameArray[ i ];
         std::cout << "Sound Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      numSounds = 1;
      defaultName = new wxString[ numSounds ];
      defaultName[ 0 ] = wxT("No Sound Files");
   }

   _soundCBox = new wxCheckListBox( this, SOUND_CBOX, wxDefaultPosition, 
                                       wxDefaultSize, numSounds, defaultName, 
                                       0, wxDefaultValidator, 
                                       wxT("Sound Files") );
   // Used to initialize all the checkboxes on
   for(int j = 0; j < numSounds; j++)
   {
      _soundCBox->Check( j );
   }

   if ( ((UI_Tabs *)_parent)->numSounds == 0 )
   {
      _soundCBox->Enable( false );
   }

   //the update button
   _updateButton = new wxButton(this,SOUND_UPDATE_BUTTON,wxT("Update"));

   //the panel sizer
   wxBoxSizer* soundPanelGroup = new wxBoxSizer(wxVERTICAL);
   //soundPanelGroup->Add(_soundRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
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

//////////////////////////////////////////////////
void UI_SoundTab::_onSounds(wxCommandEvent& event)
{
}

//////////////////////////////////////////////////
void UI_SoundTab::_onUpdate(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cIso_value = 0;
   for(int i = 0; i < ((UI_Tabs *)_parent)->numSounds; i++)
   {
      if ( _soundCBox->IsChecked( i ) )
         ((UI_Tabs *)_parent)->cIso_value += (int)pow( 2.0f, (float)i );
   }
   cout << ((UI_Tabs *)_parent)->cIso_value <<endl;
   ((UI_Tabs *)_parent)->cId  = UPDATE_SOUNDS;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}
