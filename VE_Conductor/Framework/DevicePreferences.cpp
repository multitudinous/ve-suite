#include "VE_Conductor/Framework/DevicePreferences.h"

#include <wx/sizer.h>
#include <wx/checkbox.h>
#include <wx/stattext.h>
#include <wx/scrolwin.h>
#include <wx/listbox.h>
#include <wx/splitter.h>

BEGIN_EVENT_TABLE(DevicePreferences,wxDialog)

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
DevicePreferences::DevicePreferences()
:wxDialog(NULL,-1,wxString("Device Interface Preferences"),wxDefaultPosition,wxDefaultSize, 
		   (wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX)&~wxSTAY_ON_TOP)
{
   device_splitter=NULL;

   BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
DevicePreferences::~DevicePreferences()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void DevicePreferences::BuildGUI()
{
    wxBoxSizer* box_sizer_1=new wxBoxSizer(wxVERTICAL);
    this->SetSizer(box_sizer_1);

    wxBoxSizer* box_sizer_2=new wxBoxSizer(wxHORIZONTAL);
    box_sizer_1->Add(box_sizer_2,0,wxALIGN_LEFT|wxLEFT|wxTOP,5);

    wxStaticText* static_text_1=new wxStaticText(this,wxID_STATIC,"Devices",wxDefaultPosition,wxDefaultSize,0);
    static_text_1->SetFont(wxFont(9,wxDEFAULT,wxNORMAL,wxBOLD,false));
    box_sizer_2->Add(static_text_1,0,wxALIGN_CENTER_VERTICAL|wxALL|wxADJUST_MINSIZE,5);

    box_sizer_2->Add(75,5,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

    wxStaticText* static_text_2=new wxStaticText(this,wxID_STATIC,"Device Interface Preferences",wxDefaultPosition,wxDefaultSize,0);
    static_text_2->SetFont(wxFont(9,wxDEFAULT,wxNORMAL,wxBOLD,false));
    box_sizer_2->Add(static_text_2,0,wxALIGN_CENTER_VERTICAL|wxALL|wxADJUST_MINSIZE,5);

    device_splitter=new wxSplitterWindow(this,DEVICE_SPLITTERWINDOW,wxDefaultPosition,wxSize(100,100),wxNO_BORDER);
    device_splitter->SetMinimumPaneSize(0);

    wxString list_box_strings[]={"Trackball","Wand"};
    wxListBox* list_box_1=new wxListBox(device_splitter,DEVICE_LISTBOX,wxDefaultPosition,wxDefaultSize,2,list_box_strings,wxLB_SINGLE);
    list_box_1->SetStringSelection("Two");

    wxPanel* panel_1=new wxPanel(device_splitter,DEVICE_PANEL,wxDefaultPosition,wxDefaultSize,wxSUNKEN_BORDER|wxTAB_TRAVERSAL);
    wxBoxSizer* box_sizer_3=new wxBoxSizer(wxVERTICAL);
    panel_1->SetSizer(box_sizer_3);

    wxBoxSizer* box_sizer_4=new wxBoxSizer(wxHORIZONTAL);
    box_sizer_3->Add(box_sizer_4,0,wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    wxCheckBox* check_box_animate=new wxCheckBox(panel_1,ANIMATE_CHECKBOX,"Animate",wxDefaultPosition,wxDefaultSize,0);
    check_box_animate->SetValue(false);
    box_sizer_4->Add(check_box_animate,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

    device_splitter->SplitVertically(list_box_1,panel_1,150);
    box_sizer_1->Add(device_splitter,1,wxGROW|wxLEFT|wxRIGHT,5);

    wxBoxSizer* box_sizer_5=new wxBoxSizer(wxHORIZONTAL);
    box_sizer_1->Add(box_sizer_5,0,wxALIGN_RIGHT,5);

    wxButton* button_ok=new wxButton(this,wxID_OK,"&OK",wxDefaultPosition,wxDefaultSize,0);
    box_sizer_5->Add(button_ok,0,wxALIGN_CENTER_VERTICAL|wxALL,5);

    wxButton* button_cancel=new wxButton(this,wxID_CANCEL,"&Cancel",wxDefaultPosition,wxDefaultSize,0);
    box_sizer_5->Add(button_cancel,0,wxALIGN_CENTER_VERTICAL|wxALL,5);
}
////////////////////////////////////////////////////////////////////////////////