// File: FormatSelector.h
// Author: Jeremy Jarrell
//		   jarrell@csee.wvu.edu
//         West Virginia Virtual Environments Lab
// Date: Spring 2004
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.
//

#ifndef FORMATSELECTOR_H
#define FORMATSELECTOR_H

#include <wx/wx.h>
#include <wx/image.h>
#include <wx/string.h>

#include <wx/splash.h>

// Other dialogs
#include "mFixTranslator.h"
#include "FluentTranslator.h"


class FormatSelector: public wxDialog {
public:
    // FormatSelector::ids

	FormatSelector* myFormatSelector;
	FormatSelector(wxWindow* parent, int id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);

private:
    // FormatSelector::methods
    void set_properties();
    void do_layout();

protected:
    // FormatSelector::attributes
    wxStaticText* label_1;
    wxRadioBox* radio_box_1;
    wxButton* button_1;

	// Added EVENT_TABLE
	DECLARE_EVENT_TABLE()

	enum EVENTS
	{
		BUTTON_OK			
	};	

	// Added Event handler functions
	void OnOKButton(wxCommandEvent& event);
	void OnCancelButton(wxCommandEvent& event);

};


#endif // FORMATSELECTOR_H
