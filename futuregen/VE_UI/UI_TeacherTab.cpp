#include "UI_TeacherTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>

BEGIN_EVENT_TABLE(UI_TeacherTab, wxPanel)
   EVT_RADIOBOX(TEACHER_RBOX,UI_TeacherTab::_onTeacher)
   EVT_RADIOBOX(TEACHER_CLEAR_BUTTON,UI_TeacherTab::_onClear)
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
   int numStoredScenes = ((UI_Tabs *)_parent)->num_teacher;
   wxString* defaultName;
   
   if ( numStoredScenes > 0 )
   {
      defaultName = new wxString[ numStoredScenes ];
      for(CORBA::ULong i = 0; i < (unsigned int)numStoredScenes; i++)
      {
         defaultName[ i ] = ((UI_Tabs*)_parent)->teacher_attrib[ i ];
         std::cout << "PFB  Name " << i << " : " << defaultName[ i ] << std::endl;
      }
   }
   else
   {
      numStoredScenes = 1;
      defaultName = new wxString[ numStoredScenes ];
      defaultName[ 0 ] = wxT("No PFB Files");
   }

   _teacherRBox = new wxRadioBox(this, TEACHER_RBOX, wxT("PFB Files"),
                                wxDefaultPosition, wxDefaultSize, 1,
                                defaultName,1 , wxRA_SPECIFY_COLS);

   if ( ((UI_Tabs *)_parent)->num_teacher == 0 )
   {
      _teacherRBox->Enable( false );
   }

   // Clear button
   _clearButton = new wxButton(this, TEACHER_CLEAR_BUTTON, wxT("Clear PFB Files"));

   //the panel sizer
   wxBoxSizer* teacherPanelGroup = new wxBoxSizer(wxHORIZONTAL);
   teacherPanelGroup->Add(_teacherRBox,6,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   teacherPanelGroup->Add(_clearButton,0,wxALIGN_RIGHT);

   //set this flag and let wx handle alignment
   SetAutoLayout(true);
   //assign the group to the panel
   SetSizer(teacherPanelGroup);
}
//////////////////
//event handling//
///////////////////

//////////////////////////////////////////////////
void UI_TeacherTab::_onTeacher(wxCommandEvent& event)
{
   // Are there any stored scenes loaded?
   if ( ((UI_Tabs *)_parent)->num_teacher > 0 )
   {
      for( int i = 0; i < ((UI_Tabs *)_parent)->num_teacher; i++)
      {  
         // _teacherRBox->GetSelection();
         // This code is not correct
         // Need to fix this 
         ((UI_Tabs *)_parent)->cIso_value = i;
      }
      ((UI_Tabs *)_parent)->cId = LOAD_PFB_FILE;
      ((UI_Tabs *)_parent)->sendDataArrayToServer();
   }
   else
   {
      std::cout << "There are no stored scenes loaded to send!" << std::endl;
   }
}

void UI_TeacherTab::_onClear(wxCommandEvent& event)
{
   ((UI_Tabs *)_parent)->cId = CLEAR_PFB_FILE;
   ((UI_Tabs *)_parent)->sendDataArrayToServer();
}

