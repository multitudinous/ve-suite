#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Conductor/Framework/DeviceProperties.h"
#include "VE_Conductor/Framework/App.h"
#include "VE_Conductor/Framework/Frame.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/XMLReaderWriter.h"
#include "VE_Open/XML/Command.h"

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/scrolwin.h>
#include <wx/listbox.h>
#include <wx/splitter.h>
#include <wx/button.h>
#include <wx/filename.h>

BEGIN_EVENT_TABLE(DeviceProperties,wxDialog)
   EVT_CHECKBOX(ANIMATE_CHECKBOX,DeviceProperties::OnAnimate)
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
DeviceProperties::DeviceProperties()
:wxDialog(NULL,-1,_("Device Interface"),wxDefaultPosition,wxDefaultSize, 
		   (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX)&~wxSTAY_ON_TOP)
{
   device_splitter=NULL;
   animate_check_box=NULL;

   animate=false;

   BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
DeviceProperties::~DeviceProperties()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::BuildGUI()
{
   wxBoxSizer* box_sizer_1=new wxBoxSizer(wxVERTICAL);
   this->SetSizer(box_sizer_1);

   wxBoxSizer* box_sizer_2=new wxBoxSizer(wxHORIZONTAL);
   box_sizer_1->Add(box_sizer_2,0,wxALIGN_LEFT|wxLEFT|wxTOP,5);

   wxStaticText* static_text_1=new wxStaticText(this,wxID_STATIC,_("Devices"),wxDefaultPosition,wxDefaultSize,0);
   static_text_1->SetFont(wxFont(9,wxDEFAULT,wxNORMAL,wxBOLD,false));
   box_sizer_2->Add(static_text_1,0,wxALIGN_CENTER_VERTICAL|wxALL|wxADJUST_MINSIZE,5);

   box_sizer_2->Add(75,5,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

   wxStaticText* static_text_2=new wxStaticText(this,wxID_STATIC, _("Properties"),wxDefaultPosition,wxDefaultSize,0);
   static_text_2->SetFont(wxFont(9,wxDEFAULT,wxNORMAL,wxBOLD,false));
   box_sizer_2->Add(static_text_2,0,wxALIGN_CENTER_VERTICAL|wxALL|wxADJUST_MINSIZE,5);

   device_splitter=new wxSplitterWindow(this,DEVICE_SPLITTERWINDOW,wxDefaultPosition,wxSize(100,100),wxNO_BORDER);
   device_splitter->SetMinimumPaneSize(0);

   wxString list_box_strings[]={_("KeyboardMouse"),_("Wand")};
   wxListBox* list_box_1=new wxListBox(device_splitter,DEVICE_LISTBOX,wxDefaultPosition,wxDefaultSize,2,list_box_strings,wxLB_SINGLE);
   list_box_1->SetStringSelection(_("KeyboardMouse"));

   wxPanel* panel_trackball=new wxPanel(device_splitter,DEVICE_TRACKBALL_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
   wxBoxSizer* box_sizer_3=new wxBoxSizer(wxVERTICAL);
   panel_trackball->SetSizer(box_sizer_3);

   wxBoxSizer* box_sizer_4=new wxBoxSizer(wxHORIZONTAL);
   box_sizer_3->Add(box_sizer_4,0,wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
   animate_check_box=new wxCheckBox(panel_trackball,ANIMATE_CHECKBOX,_("Animate"),wxDefaultPosition,wxDefaultSize,0);
   animate_check_box->SetValue(false);
   box_sizer_4->Add(animate_check_box,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

   device_splitter->SplitVertically(list_box_1,panel_trackball,150);
   box_sizer_1->Add(device_splitter,1,wxGROW|wxLEFT|wxRIGHT,5);

   wxBoxSizer* box_sizer_5=new wxBoxSizer(wxHORIZONTAL);
   box_sizer_1->Add(box_sizer_5,0,wxALIGN_RIGHT,5);

   wxButton* button_ok=new wxButton(this,wxID_OK,_("&OK"),wxDefaultPosition,wxDefaultSize,0);
   box_sizer_5->Add(button_ok,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

   wxButton* button_cancel=new wxButton(this,wxID_CANCEL, _("&Cancel"),wxDefaultPosition,wxDefaultSize,0);
   box_sizer_5->Add(button_cancel,0,wxALIGN_CENTER_VERTICAL|wxALL,5);
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::OnAnimate(wxCommandEvent &event)
{
   animate=animate_check_box->GetValue();

   VE_XML::DataValuePair* animateDVP=new VE_XML::DataValuePair();
   animateDVP->SetData(std::string("AnimateID"),(unsigned int)(animate));
   instructions.push_back(animateDVP);

   SendCommandsToXplorer();
   ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::SendCommandsToXplorer(){
   //Build the command
   VE_XML::Command* command=new VE_XML::Command();
   command->SetCommandName("TRACKBALL_PROPERTIES");

   for(size_t i=0;i<instructions.size();i++){
      command->AddDataValuePair(instructions.at(i));
   }

   VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer(command);

   //Clean up memory
   delete command;
}
////////////////////////////////////////////////////////////////////////////////
void DeviceProperties::ClearInstructions()
{
   instructions.clear();
}
////////////////////////////////////////////////////////////////////////////////
