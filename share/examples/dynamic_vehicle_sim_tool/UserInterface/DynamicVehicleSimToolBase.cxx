///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Sep 12 2010)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#include "DynamicVehicleSimToolBase.h"

///////////////////////////////////////////////////////////////////////////
using namespace dvst;

DynamicVehicleSimToolBase::DynamicVehicleSimToolBase( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style ) 
    : 
    UIDialog( parent, id, title )
{
    this->SetSizeHints( wxDefaultSize, wxDefaultSize );
    
    wxBoxSizer* bSizer1;
    bSizer1 = new wxBoxSizer( wxVERTICAL );
    
    wxStaticBoxSizer* sbSizer1;
    sbSizer1 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Info") ), wxVERTICAL );
    
    wxBoxSizer* bSizer8;
    bSizer8 = new wxBoxSizer( wxHORIZONTAL );
    
    wxStaticBoxSizer* sbSizer4;
    sbSizer4 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Computer Name") ), wxVERTICAL );
    
    m_computerTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT("225.0.0.37"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer4->Add( m_computerTextCtrl, 0, wxEXPAND, 5 );
    
    bSizer8->Add( sbSizer4, 1, wxRIGHT, 5 );
    
    wxStaticBoxSizer* sbSizer5;
    sbSizer5 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Port Number") ), wxVERTICAL );
    
    m_portTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT("12345"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer5->Add( m_portTextCtrl, 0, wxEXPAND, 5 );
    
    bSizer8->Add( sbSizer5, 1, wxLEFT, 5 );
    
    sbSizer1->Add( bSizer8, 0, wxEXPAND, 5 );
    
    wxStaticBoxSizer* sbSizer3;
    sbSizer3 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Controls") ), wxHORIZONTAL );
    
    m_toggleBtn1 = new wxToggleButton( this, wxID_ANY, wxT("Start/Stop"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer3->Add( m_toggleBtn1, 0, wxRIGHT, 5 );
    
    m_resetButton = new wxButton( this, wxID_ANY, wxT("Reset"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer3->Add( m_resetButton, 0, wxALIGN_CENTER_HORIZONTAL|wxLEFT|wxRIGHT, 5 );
    
    wxString m_simScaleChoices[] = { wxT("m -> ft"), wxT("cm -> ft"), wxT("mm -> ft"), wxT("in -> ft") };
    int m_simScaleNChoices = sizeof( m_simScaleChoices ) / sizeof( wxString );
    m_simScale = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_simScaleNChoices, m_simScaleChoices, 0 );
    m_simScale->SetSelection( 1 );
    sbSizer3->Add( m_simScale, 0, wxALIGN_CENTER_HORIZONTAL|wxLEFT, 5 );
    
    sbSizer1->Add( sbSizer3, 0, wxALIGN_CENTER_HORIZONTAL|wxBOTTOM|wxEXPAND|wxTOP, 5 );
    
    bSizer1->Add( sbSizer1, 0, wxALL|wxEXPAND, 5 );
    
    wxStaticBoxSizer* sbSizer2;
    sbSizer2 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Geometry Data Mapping") ), wxVERTICAL );
    
    wxBoxSizer* bSizer2;
    bSizer2 = new wxBoxSizer( wxVERTICAL );
    
    m_scrolledWindow1 = new wxScrolledWindow( this, wxID_ANY, wxDefaultPosition, wxSize( -1,150 ), wxFULL_REPAINT_ON_RESIZE|wxHSCROLL|wxSUNKEN_BORDER|wxVSCROLL );
    m_scrolledWindow1->SetScrollRate( 5, 5 );
    m_scrolledWindow1->SetMinSize( wxSize( -1,150 ) );
    
    m_scrolledWindowSizer = new wxBoxSizer( wxVERTICAL );
    
    m_scrolledWindowSizer->SetMinSize( wxSize( -1,150 ) ); 
    /*wxBoxSizer* bSizer4;
    bSizer4 = new wxBoxSizer( wxHORIZONTAL );
    
    m_staticText1 = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxT("1"), wxDefaultPosition, wxDefaultSize, 0 );
    m_staticText1->Wrap( -1 );
    bSizer4->Add( m_staticText1, 0, wxALIGN_CENTER, 5 );
    
    wxArrayString m_choice1Choices;
    m_choice1 = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice1Choices, 0 );
    m_choice1->SetSelection( 0 );
    bSizer4->Add( m_choice1, 0, wxALIGN_CENTER, 5 );
    
    m_checkBox1 = new wxCheckBox( m_scrolledWindow1, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
    bSizer4->Add( m_checkBox1, 0, wxALL, 5 );
    
    m_scrolledWindowSizer->Add( bSizer4, 0, 0, 5 );
    
    wxBoxSizer* bSizer41;
    bSizer41 = new wxBoxSizer( wxHORIZONTAL );
    
    m_staticText11 = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxT("2"), wxDefaultPosition, wxDefaultSize, 0 );
    m_staticText11->Wrap( -1 );
    bSizer41->Add( m_staticText11, 0, wxALIGN_CENTER, 5 );
    
    wxArrayString m_choice11Choices;
    m_choice11 = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice11Choices, 0 );
    m_choice11->SetSelection( 0 );
    bSizer41->Add( m_choice11, 0, wxALIGN_CENTER, 5 );
    
    m_checkBox2 = new wxCheckBox( m_scrolledWindow1, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
    bSizer41->Add( m_checkBox2, 0, wxALL, 5 );
    
    m_scrolledWindowSizer->Add( bSizer41, 0, 0, 5 );*/
    
    m_scrolledWindow1->SetSizer( m_scrolledWindowSizer );
    m_scrolledWindow1->Layout();
    bSizer2->Add( m_scrolledWindow1, 1, wxBOTTOM|wxEXPAND|wxTOP, 5 );
    
    wxBoxSizer* bSizer3;
    bSizer3 = new wxBoxSizer( wxHORIZONTAL );
    
    m_addButton = new wxButton( this, wxID_ANY, wxT("Add"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add( m_addButton, 0, wxALL, 5 );
    
    m_removeButton = new wxButton( this, wxID_ANY, wxT("Remove"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add( m_removeButton, 0, wxALL, 5 );
    
    m_applyButton = new wxButton( this, wxID_ANY, wxT("Apply"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer3->Add( m_applyButton, 0, wxALL, 5 );
    
    bSizer2->Add( bSizer3, 0, wxALIGN_CENTER_HORIZONTAL, 5 );
    
    wxStaticBoxSizer* sbSizer6;
    sbSizer6 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("User Constrained Geometry") ), wxVERTICAL );
    
    wxString m_constrainedGeomChoiceChoices[] = { wxT("None") };
    int m_constrainedGeomChoiceNChoices = sizeof( m_constrainedGeomChoiceChoices ) / sizeof( wxString );
    m_constrainedGeomChoice = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_constrainedGeomChoiceNChoices, m_constrainedGeomChoiceChoices, 0 );
    m_constrainedGeomChoice->SetSelection( 0 );
    sbSizer6->Add( m_constrainedGeomChoice, 0, 0, 5 );
    
    bSizer2->Add( sbSizer6, 0, wxBOTTOM|wxEXPAND|wxTOP, 5 );
    
    sbSizer2->Add( bSizer2, 0, wxEXPAND, 5 );
    
    bSizer1->Add( sbSizer2, 0, wxALL|wxEXPAND, 5 );
    
    wxStaticBoxSizer* registrationSizer;
    registrationSizer = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Registration Tools") ), wxVERTICAL );
    
    wxStaticBoxSizer* sbSizer8;
    sbSizer8 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("SIP Location X, Y, Z (ft)") ), wxHORIZONTAL );
    
    m_sipLocX = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer8->Add( m_sipLocX, 0, wxALL, 5 );
    
    m_sipLocY = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer8->Add( m_sipLocY, 0, wxALL, 5 );
    
    m_sipLocZ = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer8->Add( m_sipLocZ, 0, wxALL, 5 );
    
    registrationSizer->Add( sbSizer8, 0, wxBOTTOM|wxEXPAND|wxTOP, 5 );
    
    wxBoxSizer* bSizer81;
    bSizer81 = new wxBoxSizer( wxHORIZONTAL );
    
    wxString m_registrationChoiceChoices[] = { wxT("Manual"), wxT("Choose file...") };
    int m_registrationChoiceNChoices = sizeof( m_registrationChoiceChoices ) / sizeof( wxString );
    m_registrationChoice = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_registrationChoiceNChoices, m_registrationChoiceChoices, 0 );
    m_registrationChoice->SetSelection( 0 );
    bSizer81->Add( m_registrationChoice, 0, wxALIGN_CENTER_HORIZONTAL, 5 );
    
    m_registrationButton = new wxButton( this, wxID_ANY, wxT("Register"), wxDefaultPosition, wxDefaultSize, 0 );
    bSizer81->Add( m_registrationButton, 0, wxALIGN_CENTER_HORIZONTAL|wxLEFT, 5 );
    
    registrationSizer->Add( bSizer81, 1, wxEXPAND, 5 );
    
    bSizer1->Add( registrationSizer, 0, wxALL|wxEXPAND, 5 );
    
    m_sdbSizer1 = new wxStdDialogButtonSizer();
    m_sdbSizer1OK = new wxButton( this, wxID_OK );
    m_sdbSizer1->AddButton( m_sdbSizer1OK );
    m_sdbSizer1->Realize();
    bSizer1->Add( m_sdbSizer1, 0, wxALIGN_CENTER|wxALL, 5 );
    
    this->SetSizer( bSizer1 );
    this->Layout();
    
    bSizer1->Fit( this );
    
    // Connect Events
    m_computerTextCtrl->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnComputerNameEnter ), NULL, this );
    m_portTextCtrl->Connect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnPortNumberEnter ), NULL, this );
    m_toggleBtn1->Connect( wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnStartStopButton ), NULL, this );
    m_resetButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnResetSimulation ), NULL, this );
    //m_choice1->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
    //m_choice11->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
    m_addButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnAddGeometryGroupButton ), NULL, this );
    m_removeButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRemoveGeometryGroupButton ), NULL, this );
    m_applyButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
    m_constrainedGeomChoice->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
    m_registrationChoice->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegistrationFileChoice ), NULL, this );
    m_registrationButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegisterButton ), NULL, this );
    //m_sdbSizer1Apply->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
    m_sdbSizer1OK->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnOKButton ), NULL, this );
}

DynamicVehicleSimToolBase::~DynamicVehicleSimToolBase()
{
    // Disconnect Events
    m_computerTextCtrl->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnComputerNameEnter ), NULL, this );
    m_portTextCtrl->Disconnect( wxEVT_COMMAND_TEXT_ENTER, wxCommandEventHandler( DynamicVehicleSimToolBase::OnPortNumberEnter ), NULL, this );
    m_toggleBtn1->Disconnect( wxEVT_COMMAND_TOGGLEBUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnStartStopButton ), NULL, this );
    m_resetButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnResetSimulation ), NULL, this );
    //m_choice1->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
    //m_choice11->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnGeometryDataMapping ), NULL, this );
    m_addButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnAddGeometryGroupButton ), NULL, this );
    m_removeButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRemoveGeometryGroupButton ), NULL, this );
    m_applyButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
    m_constrainedGeomChoice->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
    m_registrationChoice->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegistrationFileChoice ), NULL, this );
    m_registrationButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegisterButton ), NULL, this );
    //m_sdbSizer1Apply->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
    m_sdbSizer1OK->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnOKButton ), NULL, this );
    
}
