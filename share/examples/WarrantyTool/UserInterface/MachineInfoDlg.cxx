///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#include "MachineInfoDlg.h"

///////////////////////////////////////////////////////////////////////////

MachineInfoDlg::MachineInfoDlg( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style ) 
    : 
    UIDialog( parent, id, wxT("WarrantyTool_Dialog" ) )
{
	this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxBoxSizer* bSizer1;
	bSizer1 = new wxBoxSizer( wxVERTICAL );
	
	wxStaticBoxSizer* sbSizer1;
	sbSizer1 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Data Load") ), wxVERTICAL );
	
	wxBoxSizer* bSizer3;
	bSizer3 = new wxBoxSizer( wxHORIZONTAL );
	
	m_dataLoadButton = new wxButton( this, wxID_ANY, wxT("Load"), wxDefaultPosition, wxDefaultSize, 0 );
	m_dataLoadButton->Enable( false );
	
	bSizer3->Add( m_dataLoadButton, 0, wxALIGN_CENTER_VERTICAL|wxALIGN_LEFT|wxALL, 5 );
	
	m_productDataLoader = new wxFilePickerCtrl( this, wxID_ANY, wxEmptyString, wxT("Select a file"), wxT("Warranty data file (*.csv;*.tsv;*.db)|*.csv;*.tsv;*.db"), wxDefaultPosition, wxSize( 300,-1 ), wxFLP_DEFAULT_STYLE|wxFLP_FILE_MUST_EXIST );
	m_productDataLoader->SetFont( wxFont( wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );
	m_productDataLoader->SetMinSize( wxSize( 300,-1 ) );
	
	bSizer3->Add( m_productDataLoader, 0, wxALIGN_CENTER_VERTICAL|wxALIGN_LEFT|wxALL, 5 );
	
	sbSizer1->Add( bSizer3, 0, 0, 5 );
	
	bSizer1->Add( sbSizer1, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5 );
	
	wxStaticBoxSizer* sbSizer2;
	sbSizer2 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Query Composition") ), wxVERTICAL );
	
	wxBoxSizer* bSizer5;
	bSizer5 = new wxBoxSizer( wxVERTICAL );
	
	wxBoxSizer* bSizer2;
	bSizer2 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_variableChoice00Choices;
	m_variableChoice00 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableChoice00Choices, 0 );
	m_variableChoice00->SetSelection( 0 );
	bSizer2->Add( m_variableChoice00, 0, wxALL, 5 );
	
	wxString m_variableLogicOperator00Choices[] = { wxT("Less Than"), wxT("Greater Than"), wxT("Equal"), wxT("Not Equal"), wxT("Like") };
	int m_variableLogicOperator00NChoices = sizeof( m_variableLogicOperator00Choices ) / sizeof( wxString );
	m_variableLogicOperator00 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableLogicOperator00NChoices, m_variableLogicOperator00Choices, 0 );
	m_variableLogicOperator00->SetSelection( 0 );
	bSizer2->Add( m_variableLogicOperator00, 0, wxALL, 5 );
	
	m_textInput00 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	bSizer2->Add( m_textInput00, 0, wxALL, 5 );
	
	bSizer5->Add( bSizer2, 1, wxEXPAND, 5 );
	
	wxString m_logicOperator00Choices[] = { wxT("None"), wxT("And"), wxT("Or") };
	int m_logicOperator00NChoices = sizeof( m_logicOperator00Choices ) / sizeof( wxString );
	m_logicOperator00 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_logicOperator00NChoices, m_logicOperator00Choices, 0 );
	m_logicOperator00->SetSelection( 0 );
	bSizer5->Add( m_logicOperator00, 0, wxALL, 5 );
	
	wxBoxSizer* bSizer21;
	bSizer21 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_variableChoice01Choices;
	m_variableChoice01 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableChoice01Choices, 0 );
	m_variableChoice01->SetSelection( 0 );
	bSizer21->Add( m_variableChoice01, 0, wxALL, 5 );
	
	wxString m_variableLogicOperator01Choices[] = { wxT("Less Than"), wxT("Greater Than"), wxT("Equal"), wxT("Not Equal"), wxT("Like") };
	int m_variableLogicOperator01NChoices = sizeof( m_variableLogicOperator01Choices ) / sizeof( wxString );
	m_variableLogicOperator01 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableLogicOperator01NChoices, m_variableLogicOperator01Choices, 0 );
	m_variableLogicOperator01->SetSelection( 0 );
	bSizer21->Add( m_variableLogicOperator01, 0, wxALL, 5 );
	
	m_textInput01 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	bSizer21->Add( m_textInput01, 0, wxALL, 5 );
	
	bSizer5->Add( bSizer21, 1, wxEXPAND, 5 );
	
	wxString m_logicOperator01Choices[] = { wxT("None"), wxT("And"), wxT("Or") };
	int m_logicOperator01NChoices = sizeof( m_logicOperator01Choices ) / sizeof( wxString );
	m_logicOperator01 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_logicOperator01NChoices, m_logicOperator01Choices, 0 );
	m_logicOperator01->SetSelection( 0 );
	bSizer5->Add( m_logicOperator01, 0, wxALL, 5 );
	
	wxBoxSizer* bSizer22;
	bSizer22 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_variableChoice02Choices;
	m_variableChoice02 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableChoice02Choices, 0 );
	m_variableChoice02->SetSelection( 0 );
	bSizer22->Add( m_variableChoice02, 0, wxALL, 5 );
	
	wxString m_variableLogicOperator02Choices[] = { wxT("Less Than"), wxT("Greater Than"), wxT("Equal"), wxT("Not Equal"), wxT("Like") };
	int m_variableLogicOperator02NChoices = sizeof( m_variableLogicOperator02Choices ) / sizeof( wxString );
	m_variableLogicOperator02 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableLogicOperator02NChoices, m_variableLogicOperator02Choices, 0 );
	m_variableLogicOperator02->SetSelection( 0 );
	bSizer22->Add( m_variableLogicOperator02, 0, wxALL, 5 );
	
	m_textInput02 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	bSizer22->Add( m_textInput02, 0, wxALL, 5 );
	
	bSizer5->Add( bSizer22, 1, wxEXPAND, 5 );
	
	wxString m_logicOperator02Choices[] = { wxT("None"), wxT("And"), wxT("Or") };
	int m_logicOperator02NChoices = sizeof( m_logicOperator02Choices ) / sizeof( wxString );
	m_logicOperator02 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_logicOperator02NChoices, m_logicOperator02Choices, 0 );
	m_logicOperator02->SetSelection( 0 );
	bSizer5->Add( m_logicOperator02, 0, wxALL, 5 );
	
	wxBoxSizer* bSizer221;
	bSizer221 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_variableChoice03Choices;
	m_variableChoice03 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableChoice03Choices, 0 );
	m_variableChoice03->SetSelection( 0 );
	bSizer221->Add( m_variableChoice03, 0, wxALL, 5 );
	
	wxString m_variableLogicOperator03Choices[] = { wxT("Less Than"), wxT("Greater Than"), wxT("Equal"), wxT("Not Equal"), wxT("Like") };
	int m_variableLogicOperator03NChoices = sizeof( m_variableLogicOperator03Choices ) / sizeof( wxString );
	m_variableLogicOperator03 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_variableLogicOperator03NChoices, m_variableLogicOperator03Choices, 0 );
	m_variableLogicOperator03->SetSelection( 0 );
	bSizer221->Add( m_variableLogicOperator03, 0, wxALL, 5 );
	
	m_textInput03 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	bSizer221->Add( m_textInput03, 0, wxALL, 5 );
	
	bSizer5->Add( bSizer221, 1, wxEXPAND, 5 );
	
	wxStaticBoxSizer* queryTextCommandSizer;
	queryTextCommandSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Query Command") ), wxVERTICAL );
	
	m_queryTextCommandCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER );
	queryTextCommandSizer->Add( m_queryTextCommandCtrl, 0, wxEXPAND, 5 );
	
	bSizer5->Add( queryTextCommandSizer, 0, wxEXPAND|wxTOP, 5 );
	
	sbSizer2->Add( bSizer5, 1, wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer2, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5 );
	
	/*wxStaticBoxSizer* sbSizer5;
	sbSizer5 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Color Selection") ), wxVERTICAL );
	
	wxBoxSizer* bSizer9;
	bSizer9 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_choice13Choices;
	m_choice13 = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice13Choices, 0 );
	m_choice13->SetSelection( 0 );
	bSizer9->Add( m_choice13, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );
	
	wxStaticBoxSizer* sbSizer6;
	sbSizer6 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Min") ), wxVERTICAL );
	
	m_textCtrl18 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer6->Add( m_textCtrl18, 0, 0, 5 );
	
	bSizer9->Add( sbSizer6, 1, wxLEFT|wxRIGHT, 5 );
	
	wxStaticBoxSizer* sbSizer7;
	sbSizer7 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Max") ), wxVERTICAL );
	
	m_textCtrl19 = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	sbSizer7->Add( m_textCtrl19, 0, 0, 5 );
	
	bSizer9->Add( sbSizer7, 1, wxLEFT|wxRIGHT, 5 );
	
	sbSizer5->Add( bSizer9, 1, wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer5, 0, wxALL|wxEXPAND, 5 );*/
	
	wxStaticBoxSizer* sbSizer3;
	sbSizer3 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Part Selction") ), wxVERTICAL );
	
	wxBoxSizer* bSizer4;
	bSizer4 = new wxBoxSizer( wxHORIZONTAL );
	
	wxArrayString m_manualPartSelectionChoiceChoices;
	m_manualPartSelectionChoice = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_manualPartSelectionChoiceChoices, 0 );
	m_manualPartSelectionChoice->SetSelection( 0 );
	bSizer4->Add( m_manualPartSelectionChoice, 0, wxRIGHT, 5 );
	
	m_partTextEntry = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxSize( 200,-1 ), wxTE_PROCESS_ENTER );
	bSizer4->Add( m_partTextEntry, 0, wxEXPAND|wxLEFT, 5 );
	
	sbSizer3->Add( bSizer4, 0, wxALL|wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer3, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5 );
	
	wxStaticBoxSizer* sbSizer8;
	sbSizer8 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Text Display Selection") ), wxVERTICAL );
	
	wxArrayString m_displayTextChkListChoices;
	m_displayTextChkList = new wxCheckListBox( this, wxID_ANY, wxDefaultPosition, wxSize( -1,75 ), m_displayTextChkListChoices, wxLB_ALWAYS_SB|wxLB_EXTENDED );
	sbSizer8->Add( m_displayTextChkList, 0, wxALIGN_CENTER|wxEXPAND, 5 );
	
	bSizer1->Add( sbSizer8, 0, wxALL|wxEXPAND, 5 );
	
	wxStaticBoxSizer* sbSizer10;
	sbSizer10 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Controls") ), wxHORIZONTAL );
	
	m_toggleUnselected = new wxCheckBox( this, wxID_ANY, wxT("Toggle Unselected"), wxDefaultPosition, wxSize( -1,20 ), wxCHK_2STATE );
	
	sbSizer10->Add( m_toggleUnselected, 0, wxALL, 5 );
	
	m_button2 = new wxButton( this, wxID_ANY, wxT("Clear"), wxDefaultPosition, wxSize( -1,20 ), 0 );
	m_button2->SetMaxSize( wxSize( -1,20 ) );
	
	sbSizer10->Add( m_button2, 0, wxALL, 5 );
	
	bSizer1->Add( sbSizer10, 0, wxALL|wxEXPAND, 5 );
	
	m_dialogButtons = new wxStdDialogButtonSizer();
	m_dialogButtonsOK = new wxButton( this, wxID_OK );
	m_dialogButtons->AddButton( m_dialogButtonsOK );
	m_dialogButtonsApply = new wxButton( this, wxID_APPLY );
	m_dialogButtons->AddButton( m_dialogButtonsApply );
	m_dialogButtonsCancel = new wxButton( this, wxID_CANCEL );
	m_dialogButtons->AddButton( m_dialogButtonsCancel );
	m_dialogButtons->Realize();
	bSizer1->Add( m_dialogButtons, 0, wxALIGN_CENTER|wxALL, 5 );
	
	this->SetSizer( bSizer1 );
	this->Layout();
	bSizer1->Fit( this );
	
	// Connect Events
	m_productDataLoader->Connect( wxEVT_COMMAND_FILEPICKER_CHANGED, wxFileDirPickerEventHandler( MachineInfoDlg::OnDataLoad ), NULL, this );
	m_variableChoice00->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator00->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput00->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput00->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator00->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice01->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator01->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput01->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput01->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator01->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice02->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator02->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput02->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput02->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator02->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice03->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator03->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput03->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput03->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_queryTextCommandCtrl->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnTextQueryEnter ), NULL, this );
	m_manualPartSelectionChoice->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnPartSelection ), NULL, this );
	m_partTextEntry->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnPartNumberEntry ), NULL, this );
	m_displayTextChkList->Connect( wxEVT_COMMAND_CHECKLISTBOX_TOGGLED, wxCommandEventHandler( MachineInfoDlg::OnTextChkListToggle ), NULL, this );
	m_toggleUnselected->Connect( wxEVT_COMMAND_CHECKBOX_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnToggleUnselected ), NULL, this );
	m_button2->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnClearData ), NULL, this );
	m_dialogButtonsApply->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnQueryApply ), NULL, this );
	m_dialogButtonsCancel->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnDialogCancel ), NULL, this );
	m_dialogButtonsOK->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnQueryOK ), NULL, this );
}

MachineInfoDlg::~MachineInfoDlg()
{
	// Disconnect Events
	m_productDataLoader->Disconnect( wxEVT_COMMAND_FILEPICKER_CHANGED, wxFileDirPickerEventHandler( MachineInfoDlg::OnDataLoad ), NULL, this );
	m_variableChoice00->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator00->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput00->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput00->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator00->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice01->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator01->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput01->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput01->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator01->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice02->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator02->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput02->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput02->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_logicOperator02->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableChoice03->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_variableLogicOperator03->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnVariableAndLogicalChoice ), NULL, this );
	m_textInput03->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_textInput03->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnCreateInputText ), NULL, this );
	m_queryTextCommandCtrl->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnTextQueryEnter ), NULL, this );
	m_manualPartSelectionChoice->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( MachineInfoDlg::OnPartSelection ), NULL, this );
	m_partTextEntry->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( MachineInfoDlg::OnPartNumberEntry ), NULL, this );
	m_displayTextChkList->Disconnect( wxEVT_COMMAND_CHECKLISTBOX_TOGGLED, wxCommandEventHandler( MachineInfoDlg::OnTextChkListToggle ), NULL, this );
	m_toggleUnselected->Disconnect( wxEVT_COMMAND_CHECKBOX_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnToggleUnselected ), NULL, this );
	m_button2->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnClearData ), NULL, this );
	m_dialogButtonsApply->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnQueryApply ), NULL, this );
	m_dialogButtonsCancel->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnDialogCancel ), NULL, this );
	m_dialogButtonsOK->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( MachineInfoDlg::OnQueryOK ), NULL, this );
}
