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
 * File:          $RCSfile: FluentTranslator.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// Author: Jeremy Jarrell jarrell@csee.wvu.edu
//
// This is a portion of the GUI written for the CFD Translator developed on contract from DOE-NETL.

#include "FluentTranslator.h"

#include <iostream> 

#include <wx/statline.h>
#include <wx/wx.h>
#include <wx/image.h>
#include <wx/progdlg.h>

void parseSet( std::string casefile, std::string datafile, bool isBinary, bool isGzip, int var_id );


FluentTranslator::FluentTranslator(wxWindow* parent, int id, const wxString& title, const wxPoint& pos, const wxSize& size, long style):
    wxDialog(parent, id, title, pos, size, wxDEFAULT_DIALOG_STYLE)
{
    // begin wxGlade: FluentTranslator::FluentTranslator
    CaseButton = new wxButton(this, BUTTON_CASE, wxT("Case File"));
    DataButton = new wxButton(this, BUTTON_DATA, wxT("Data File"));
    LoadCaseAndData = new wxButton(this, BUTTON_LOAD_CASE_AND_DATA, wxT("Load Case and Data"));
    panel_1 = new wxPanel(this, -1);
    AboutButton = new wxButton(this, BUTTON_ABOUT, wxT("About"));
    static_line_1 = new wxStaticLine(this, -1);
    ASCIICheckbox = new wxCheckBox(this, CHECKBOX_ASCII, wxT("VTK ASCII"));
    BinaryCheckbox = new wxCheckBox(this, CHECKBOX_BINARY, wxT("VTK Binary"));
    SerialXMLCheckbox = new wxCheckBox(this, CHECKBOX_SERIAL_XML, wxT("VTK Serial XML"));
    ParallelXMLCheckbox = new wxCheckBox(this, CHECKBOX_PARALLEL_XML, wxT("VTK Parallel XML"));
    CGNSCheckbox = new wxCheckBox(this, CHECKBOX_CGNS, wxT("CGNS"));
    static_line_2 = new wxStaticLine(this, -1);
    SelectAllButton = new wxButton(this, BUTTON_SELECT_ALL, wxT("Select All"));
    UnSelectAllButton = new wxButton(this, BUTTON_UNSELECT_ALL, wxT("Unselect All"));
    static_line_3 = new wxStaticLine(this, -1);
    StatusTextCntrl = new wxTextCtrl(this, -1, wxT(""), wxDefaultPosition, wxDefaultSize, wxTE_MULTILINE|wxTE_READONLY|wxTE_RICH|wxTE_DONTWRAP|wxHSCROLL);
    label_1 = new wxStaticText(this, -1, wxT("Possible Variables"));
    panel_4 = new wxPanel(this, -1);
    label_2 = new wxStaticText(this, -1, wxT("Selected Variables"));
    const wxString PossibleVariableListBox_choices[] = {

    };
    PossibleVariableListBox = new wxListBox(this, -1, wxDefaultPosition, wxDefaultSize, 0, PossibleVariableListBox_choices, 0);
    static_line_4 = new wxStaticLine(this, -1, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL);
    const wxString SelectedVariableListBox_choices[] = {

    };
    SelectedVariableListBox = new wxListBox(this, -1, wxDefaultPosition, wxDefaultSize, 0, SelectedVariableListBox_choices, 0);
    AddButton = new wxButton(this, BUTTON_ADD, wxT("Add"));
    AddAllButton = new wxButton(this, BUTTON_ADD_ALL, wxT("Add All"));
    DeleteAllButton = new wxButton(this, BUTTON_DELETE_ALL, wxT("Delete All"));
    GoButton = new wxButton(this, BUTTON_GO, wxT("Go"));
    CloseButton = new wxButton(this, BUTTON_CLOSE, wxT("Close"));

    CaseFileSelected_Flag = 0;
    DataFileSelected_Flag = 0;

    set_properties();
    do_layout();
}

void FluentTranslator::set_properties()
{
    // begin wxGlade: FluentTranslator::set_properties
    SetTitle(wxT("Fluent Translator"));
    SetSize(wxSize(800, 620));
    ASCIICheckbox->SetValue(1);
    StatusTextCntrl->SetSize(wxSize(300, 300));
    panel_4->SetSize(wxSize(150, 26));
    PossibleVariableListBox->SetSize(wxSize(200, 425));
    PossibleVariableListBox->SetSelection(0);
    SelectedVariableListBox->SetSize(wxSize(200, 425));
    SelectedVariableListBox->SetSelection(0);
    // end wxGlade
}

void FluentTranslator::do_layout()
{
    // begin wxGlade: FluentTranslator::do_layout
    wxBoxSizer* OverallSizer = new wxBoxSizer(wxVERTICAL);
    wxBoxSizer* BottomButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* sizer_3 = new wxBoxSizer(wxHORIZONTAL);
    wxStaticBoxSizer* RightSideSizer = new wxStaticBoxSizer(new wxStaticBox(this, -1, wxT("Output Variable Selection")), wxVERTICAL);
    wxBoxSizer* OutputVariableButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* VariableSelectionSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* LabelSizer = new wxBoxSizer(wxHORIZONTAL);
    wxBoxSizer* LeftSideSizer = new wxBoxSizer(wxVERTICAL);
    wxStaticBoxSizer* StatusInfoSizer = new wxStaticBoxSizer(new wxStaticBox(this, -1, wxT("Status Information")), wxHORIZONTAL);
    wxBoxSizer* SelectUnSelectSizer = new wxBoxSizer(wxHORIZONTAL);
    wxStaticBoxSizer* OutputFormatSizer = new wxStaticBoxSizer(new wxStaticBox(this, -1, wxT("Output File Formats")), wxVERTICAL);
    wxBoxSizer* TopButtonsSizer = new wxBoxSizer(wxHORIZONTAL);
    TopButtonsSizer->Add(CaseButton, 0, wxALL, 10);
    TopButtonsSizer->Add(DataButton, 0, wxALL, 10);
    TopButtonsSizer->Add(LoadCaseAndData, 0, wxALL, 10);
    TopButtonsSizer->Add(panel_1, 1, wxEXPAND, 0);
    TopButtonsSizer->Add(AboutButton, 0, wxALL, 10);
    OverallSizer->Add(TopButtonsSizer, 0, wxEXPAND, 0);
    OverallSizer->Add(static_line_1, 0, wxEXPAND, 0);
    OutputFormatSizer->Add(ASCIICheckbox, 0, 0, 0);
    OutputFormatSizer->Add(BinaryCheckbox, 0, 0, 0);
    OutputFormatSizer->Add(SerialXMLCheckbox, 0, 0, 0);
    OutputFormatSizer->Add(ParallelXMLCheckbox, 0, 0, 0);
    OutputFormatSizer->Add(CGNSCheckbox, 0, 0, 0);
    LeftSideSizer->Add(OutputFormatSizer, 0, wxEXPAND, 0);
    LeftSideSizer->Add(static_line_2, 0, wxEXPAND, 0);
    SelectUnSelectSizer->Add(SelectAllButton, 0, wxALL, 10);
    SelectUnSelectSizer->Add(UnSelectAllButton, 0, wxALL|wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 10);
    LeftSideSizer->Add(SelectUnSelectSizer, 0, 0, 0);
    LeftSideSizer->Add(static_line_3, 0, wxEXPAND, 0);
    StatusInfoSizer->Add(StatusTextCntrl, 0, 0, 0);
    LeftSideSizer->Add(StatusInfoSizer, 1, wxEXPAND, 0);
    sizer_3->Add(LeftSideSizer, 0, wxEXPAND, 0);
    LabelSizer->Add(label_1, 0, wxALL, 5);
    LabelSizer->Add(panel_4, 1, wxEXPAND, 0);
    LabelSizer->Add(label_2, 0, wxALL, 5);
    RightSideSizer->Add(LabelSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    VariableSelectionSizer->Add(PossibleVariableListBox, 0, 0, 0);
    VariableSelectionSizer->Add(static_line_4, 0, wxEXPAND, 0);
    VariableSelectionSizer->Add(SelectedVariableListBox, 0, 0, 0);
    RightSideSizer->Add(VariableSelectionSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    OutputVariableButtonsSizer->Add(AddButton, 0, wxALL, 10);
    OutputVariableButtonsSizer->Add(AddAllButton, 0, wxALL, 10);
    OutputVariableButtonsSizer->Add(DeleteAllButton, 0, wxALL, 10);
    RightSideSizer->Add(OutputVariableButtonsSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    sizer_3->Add(RightSideSizer, 1, wxEXPAND, 0);
    OverallSizer->Add(sizer_3, 0, wxEXPAND, 0);
    BottomButtonsSizer->Add(GoButton, 0, wxALL, 10);
    BottomButtonsSizer->Add(CloseButton, 0, wxALL, 10);
    OverallSizer->Add(BottomButtonsSizer, 0, wxALIGN_CENTER_HORIZONTAL|wxALIGN_CENTER_VERTICAL, 0);
    SetAutoLayout(true);
    SetSizer(OverallSizer);
    Layout();
    // end wxGlade
}


BEGIN_EVENT_TABLE(FluentTranslator, wxDialog)
	EVT_BUTTON	(BUTTON_CASE, FluentTranslator::OnCaseButton)
	EVT_BUTTON	(BUTTON_DATA, FluentTranslator::OnDataButton)
	EVT_BUTTON	(BUTTON_LOAD_CASE_AND_DATA, FluentTranslator::OnLoadCaseAndDataButton)
	EVT_BUTTON	(BUTTON_ABOUT, FluentTranslator::OnAboutButton)
	EVT_CHECKBOX	(CHECKBOX_ASCII, FluentTranslator::OnASCIICheckBox)
	EVT_CHECKBOX	(CHECKBOX_BINARY, FluentTranslator::OnBinaryCheckBox)
	EVT_CHECKBOX	(CHECKBOX_SERIAL_XML, FluentTranslator::OnSerialXMLCheckBox)
	EVT_CHECKBOX	(CHECKBOX_PARALLEL_XML, FluentTranslator::OnParallelXMLCheckBox)
	EVT_CHECKBOX	(CHECKBOX_CGNS, FluentTranslator::OnCGNSCheckBox)
	EVT_BUTTON	(BUTTON_SELECT_ALL, FluentTranslator::OnSelectAllButton)
	EVT_BUTTON	(BUTTON_UNSELECT_ALL, FluentTranslator::OnUnSelectAllButton)
	EVT_BUTTON	(BUTTON_ADD, FluentTranslator::OnAddButton)
	EVT_BUTTON	(BUTTON_ADD_ALL, FluentTranslator::OnAddAllButton)
	EVT_BUTTON	(BUTTON_DELETE_ALL, FluentTranslator::OnDeleteAllButton)
	EVT_BUTTON	(BUTTON_GO, FluentTranslator::OnGoButton)
	EVT_BUTTON	(BUTTON_CLOSE, FluentTranslator::OnCloseButton)
	EVT_BUTTON	(wxID_CANCEL, FluentTranslator::OnCancelButton)

END_EVENT_TABLE()


void FluentTranslator::OnCaseButton(wxCommandEvent& event)
{
	std::cout << "Case Function" << std::endl;

	OpenFileDialog = new wxFileDialog(this, wxT("Choose a Fluent Case File..."), wxT(""), wxT(""), wxT("*.cas"), wxOPEN|wxHIDE_READONLY, wxDefaultPosition);
	if(OpenFileDialog->ShowModal() == wxID_OK)
	{

		CaseFilename = OpenFileDialog->GetPath();

		std::cout << CaseFilename << std::endl;
   		wxString status_text;
    		status_text.Printf(wxT("Case File = "));
    		StatusTextCntrl->AppendText(status_text);
    		StatusTextCntrl->AppendText(CaseFilename);
    		status_text.Printf(wxT("\n"));
    		StatusTextCntrl->AppendText(status_text);

		CaseFileSelected_Flag = 1;
	}

	OpenFileDialog->Destroy();
}

void FluentTranslator::OnDataButton(wxCommandEvent& event)
{
	std::cout << "Data Function" << std::endl;

	OpenFileDialog = new wxFileDialog(this, wxT("Choose a Fluent Case File..."), wxT(""), wxT(""), wxT("*.dat"), wxOPEN|wxHIDE_READONLY, wxDefaultPosition);
	if(OpenFileDialog->ShowModal() == wxID_OK)
	{

		DataFilename = OpenFileDialog->GetPath();

		std::cout << DataFilename << std::endl;
   		wxString status_text;
    		status_text.Printf(wxT("Data File = "));
    		StatusTextCntrl->AppendText(status_text);
    		StatusTextCntrl->AppendText(DataFilename);
    		status_text.Printf(wxT("\n"));
    		StatusTextCntrl->AppendText(status_text);

		DataFileSelected_Flag = 1;
	}

	OpenFileDialog->Destroy();
}

void FluentTranslator::OnLoadCaseAndDataButton(wxCommandEvent& event)
{
	//std::cout << "Load Case and Data Function" << std::endl;

	if(CaseFileSelected_Flag && DataFileSelected_Flag){
    		wxString status_text;
    		status_text.Printf(wxT("Both Files Selected.\nStart Reading Files.\n"));
    		StatusTextCntrl->AppendText(status_text);

		parseSet( (std::string)CaseFilename, (std::string)DataFilename, 1, 0, 200 );

	}
	else if(CaseFileSelected_Flag && !DataFileSelected_Flag){
		wxMessageBox(wxT("You forgot to select the data file."), wxT("Problem!"), wxOK);
	}
	else if(!CaseFileSelected_Flag && DataFileSelected_Flag){
		wxMessageBox(wxT("You forgot to select the case file."), wxT("Problem!"), wxOK);
	}
	else {
		wxMessageBox(wxT("You forgot to select the case and data files."), wxT("Problem!"), wxOK);
	}
}

void FluentTranslator::OnAboutButton(wxCommandEvent& event)
{
	wxMessageBox(wxT("Fluent to VTK Data Translator\nNETL/DOE - 2004, v1.0.5\n------------------------------------------\nDavid E. Huckaby <david.huckaby@netl.doe.gov>\nBrian Dotson <brian.dotson@netl.doe.gov>"), wxT("About"), wxOK);
}

void FluentTranslator::OnASCIICheckBox(wxCommandEvent& event)
{

}

void FluentTranslator::OnBinaryCheckBox(wxCommandEvent& event)
{
}

void FluentTranslator::OnSerialXMLCheckBox(wxCommandEvent& event)
{
}

void FluentTranslator::OnParallelXMLCheckBox(wxCommandEvent& event)
{
}

void FluentTranslator::OnCGNSCheckBox(wxCommandEvent& event)
{
}

void FluentTranslator::OnSelectAllButton(wxCommandEvent& event)
{
    ASCIICheckbox->SetValue(TRUE);
    BinaryCheckbox->SetValue(TRUE);
    SerialXMLCheckbox->SetValue(TRUE);
    ParallelXMLCheckbox->SetValue(TRUE);
    CGNSCheckbox->SetValue(TRUE);
    wxString status_text;
    status_text.Printf(wxT("Selected All Formats.\n"));
    StatusTextCntrl->AppendText(status_text);
}

void FluentTranslator::OnUnSelectAllButton(wxCommandEvent& event)
{
    ASCIICheckbox->SetValue(FALSE);
    BinaryCheckbox->SetValue(FALSE);
    SerialXMLCheckbox->SetValue(FALSE);
    ParallelXMLCheckbox->SetValue(FALSE);
    CGNSCheckbox->SetValue(FALSE);
    wxString status_text;
    status_text.Printf(wxT("Unselected All Formats.\n"));
    StatusTextCntrl->AppendText(status_text);

}

void FluentTranslator::OnAddButton(wxCommandEvent& event)
{
}

void FluentTranslator::OnAddAllButton(wxCommandEvent& event)
{
}

void FluentTranslator::OnDeleteAllButton(wxCommandEvent& event)
{
}

void FluentTranslator::OnGoButton(wxCommandEvent& event)
{
}

void FluentTranslator::OnCloseButton(wxCommandEvent& event)
{
	Destroy();
}

void FluentTranslator::OnCancelButton(wxCommandEvent& event)
{
	OnCancel(event);
	Destroy();
}
