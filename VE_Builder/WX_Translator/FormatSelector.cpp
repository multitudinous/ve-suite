// File: FormatSelector.cpp
// Author: Jeremy Jarrell
//		   jarrell@csee.wvu.edu
//         West Virginia Virtual Environments Lab
// Date: Spring 2004
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.
//

#include "FormatSelector.h"

FormatSelector::FormatSelector(wxWindow* parent, int id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
    // FormatSelector::FormatSelector
    label_1 = new wxStaticText(this, -1, wxT("  Please select the format \nof the file you are importing:"), wxDefaultPosition, wxDefaultSize, wxALIGN_CENTRE);
    const wxString radio_box_1_choices[] = {
        wxT("mFix"),
        wxT("Fluent")
    };
    radio_box_1 = new wxRadioBox(this, -1, wxT("File Format"), wxDefaultPosition, wxDefaultSize, 2, radio_box_1_choices, 0, wxRA_SPECIFY_ROWS);
    button_1 = new wxButton(this, 0, wxT("OK"));

    set_properties();
    do_layout();
}

void FormatSelector::set_properties()
{
    // FormatSelector::set_properties
    SetTitle(wxT("CFD Translator"));
    SetSize(wxSize(300, 250));
    radio_box_1->SetSelection(0);
	radio_box_1->SetToolTip(wxT("Available formats"));
	button_1->SetToolTip(wxT("Proceed"));

	// Set icon
	wxIcon icon;
    icon.CopyFromBitmap(wxBitmap(wxT("res/vel.ico"), wxBITMAP_TYPE_ANY));
    SetIcon(icon);
}


void FormatSelector::do_layout()
{
    // FormatSelector::do_layout
    wxBoxSizer* sizer_1 = new wxBoxSizer(wxVERTICAL);
    sizer_1->Add(20, 20, 0, 0, 0);
    sizer_1->Add(0, 0, 0, 0, 0);
    sizer_1->Add(label_1, 0, wxLEFT|wxRIGHT|wxALIGN_CENTER_HORIZONTAL, 16);
    sizer_1->Add(20, 20, 0, 0, 0);
    sizer_1->Add(radio_box_1, 0, wxALIGN_CENTER_HORIZONTAL, 0);
    sizer_1->Add(20, 20, 0, 0, 0);
    sizer_1->Add(button_1, 0, wxALIGN_CENTER_HORIZONTAL, 0);
    sizer_1->Add(20, 20, 0, 0, 0);
    SetAutoLayout(true);
    SetSizer(sizer_1);
    Layout();
}

// Event Table (FormatSelector)
BEGIN_EVENT_TABLE(FormatSelector, wxDialog)
	EVT_BUTTON	(BUTTON_OK, FormatSelector::OnOKButton)
	EVT_BUTTON	(wxID_CANCEL, FormatSelector::OnCancelButton)
END_EVENT_TABLE()

// Added Event handler functions
void FormatSelector::OnOKButton(wxCommandEvent& event)
{
	if(radio_box_1->GetSelection() == 0)
	{		
		OnOK(event);
		mFixTranslator* myMFixTranslator = new mFixTranslator(0, -1, wxT(""));
		myMFixTranslator->Centre(wxBOTH);
		myMFixTranslator->SetToolTip(wxT("mFix File Translator"));
		myMFixTranslator->Show(true);
		Destroy();
	} else {
		OnOK(event);
		FluentTranslator* myFluentTranslator = new FluentTranslator(0, -1, wxT(""));
		myFluentTranslator->Centre(wxBOTH);
		myFluentTranslator->SetToolTip(wxT("Fluent File Translator"));
		myFluentTranslator->Show(true);	
		Destroy(); 
	}
}

void FormatSelector::OnCancelButton(wxCommandEvent& event)
{
	OnCancel(event);
	Destroy();
}

class MyApp: public wxApp {
public:
    bool OnInit();
};

IMPLEMENT_APP(MyApp)

bool MyApp::OnInit()
{
	wxInitAllImageHandlers();
    FormatSelector* myFormatSelector = new FormatSelector(0, -1, wxT(""));

	// Splash screen code
	wxBitmap bitmap;
	if (bitmap.LoadFile(wxT("res/splash.bmp"), wxBITMAP_TYPE_BMP))
	{
		wxSplashScreen* splash = new wxSplashScreen(bitmap, wxSPLASH_CENTRE_ON_PARENT|wxSPLASH_TIMEOUT,
													3000, NULL, -1, wxDefaultPosition, wxDefaultSize, 
													wxSIMPLE_BORDER|wxSTAY_ON_TOP);
		splash->SetTitle(wxT("CFD Translator"));
	}
	wxYield(); 

    SetTopWindow(myFormatSelector);
	myFormatSelector->Centre(wxBOTH);
	myFormatSelector->Show();
	
    return true;
}

