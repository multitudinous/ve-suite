#include "UI_TeacherTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>

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
void UI_TeacherTab::_onTeacher(wxCommandEvent& event)
{
   event.GetInt();
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
