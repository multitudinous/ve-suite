// File: FluentTranslator.h
// Author: Jeremy Jarrell
//		   jarrell@csee.wvu.edu
//         West Virginia Virtual Environments Lab
// Date: Spring 2004
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.
//

#include <wx/wx.h>
#include <wx/image.h>

#include <wx/progdlg.h>
#ifndef FLUENTTRANSLATOR_H
#define FLUENTTRANSLATOR_H


class FluentTranslator: public wxDialog {
public:
    // FluentTranslator::ids

    FluentTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos=wxDefaultPosition, const wxSize& size=wxDefaultSize, long style=wxDEFAULT_DIALOG_STYLE);
	
private:
    // FluentTranslator::methods
    void set_properties();
    void do_layout();

protected:
    // FluentTranslator::attributes
    wxStaticText* label_2;

	wxButton* OKButton;


	DECLARE_EVENT_TABLE()

	enum EVENTS
	{
		BUTTON_OK
	};

	
	void OnOKButton(wxCommandEvent& event);
	void OnCancelButton(wxCommandEvent& event);
	void OnCloseWindow(wxCommandEvent& event);
};


#endif // FLUENTTRANSLATOR_H
