/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
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
 * File:          $RCSfile: FluentTranslator.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// Author: Jeremy Jarrell jarrell@csee.wvu.edu
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.

#ifndef FLUENTTRANSLATOR_H
#define FLUENTTRANSLATOR_H

class wxButton;
class wxCheckBox;
class wxCommandEvent;
class wxFileDialog;
class wxListBox;
class wxPanel;
class wxStaticLine;
class wxStaticText;
class wxString;
class wxTextCtrl;

#include <wx/dialog.h>

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
    wxButton* CaseButton;
    wxButton* DataButton;
    wxButton* LoadCaseAndData;
    wxPanel* panel_1;
    wxButton* AboutButton;
    wxStaticLine* static_line_1;
    wxCheckBox* ASCIICheckbox;
    wxCheckBox* BinaryCheckbox;
    wxCheckBox* SerialXMLCheckbox;
    wxCheckBox* ParallelXMLCheckbox;
    wxCheckBox* CGNSCheckbox;
    wxStaticLine* static_line_2;
    wxButton* SelectAllButton;
    wxButton* UnSelectAllButton;
    wxStaticLine* static_line_3;
    wxTextCtrl* StatusTextCntrl;
    wxStaticText* label_1;
    wxPanel* panel_4;
    wxStaticText* label_2;
    wxListBox* PossibleVariableListBox;
    wxStaticLine* static_line_4;
    wxListBox* SelectedVariableListBox;
    wxButton* AddButton;
    wxButton* AddAllButton;
    wxButton* DeleteAllButton;
    wxButton* GoButton;
    wxButton* CloseButton;

    wxFileDialog* OpenFileDialog;

    int CaseFileSelected_Flag;
    int DataFileSelected_Flag;
    wxString  CaseFilename;
    wxString  DataFilename;

	DECLARE_EVENT_TABLE()

	enum EVENTS
	{
		BUTTON_CASE,
		BUTTON_DATA,
		BUTTON_LOAD_CASE_AND_DATA,
		BUTTON_ABOUT,
		CHECKBOX_ASCII,
		CHECKBOX_BINARY,
		CHECKBOX_SERIAL_XML,
		CHECKBOX_PARALLEL_XML,
		CHECKBOX_CGNS,
		BUTTON_SELECT_ALL,
		BUTTON_UNSELECT_ALL,
		BUTTON_ADD,
		BUTTON_ADD_ALL,
		BUTTON_DELETE_ALL,
		BUTTON_GO,
		BUTTON_CLOSE,
		//Added by Alberto Jove 09/29/2004
		ID_LISTBOX=1000
	};

	void OnCaseButton(wxCommandEvent& event);
	void OnDataButton(wxCommandEvent& event);
	void OnLoadCaseAndDataButton(wxCommandEvent& event);
	void OnAboutButton(wxCommandEvent& event);
	void OnASCIICheckBox(wxCommandEvent& event);
	void OnBinaryCheckBox(wxCommandEvent& event);
	void OnSerialXMLCheckBox(wxCommandEvent& event);
	void OnParallelXMLCheckBox(wxCommandEvent& event);
	void OnCGNSCheckBox(wxCommandEvent& event);
	void OnSelectAllButton(wxCommandEvent& event);
	void OnUnSelectAllButton(wxCommandEvent& event);
	void OnAddButton(wxCommandEvent& event);
	void OnAddAllButton(wxCommandEvent& event);
	void OnDeleteAllButton(wxCommandEvent& event);
	void OnGoButton(wxCommandEvent& event);
	void OnCloseButton(wxCommandEvent& event);
	void OnCancelButton(wxCommandEvent& event);
	//Added by Alberto Jove 11/19/2004
	wxString getFilename(wxString name); //helps to verify the root of the filenames
	//
};

#endif // FLUENTTRANSLATOR_H
