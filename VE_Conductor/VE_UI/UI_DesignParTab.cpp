#include "UI_DesignParTab.h"
#include "UI_Tabs.h"
#include "cfdEnum.h"
#include <iostream>
#include <cmath>

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
   cout << ((UI_Tabs *)_parent)->cIso_value <<endl;*/
   //((UI_Tabs *)_parent)->cId  = UPDATE_DESIGNPARMS;
   //((UI_Tabs *)_parent)->sendDataArrayToServer();
}
