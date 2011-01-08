///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Sep 12 2010)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#include "DynamicVehicleSimToolBase.h"

///////////////////////////////////////////////////////////////////////////
using namespace dvst;
using namespace ves::conductor;

DynamicVehicleSimToolBase::DynamicVehicleSimToolBase( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style )
    :
    UIDialog( parent, id, title )
{
    this->SetSizeHints( wxDefaultSize, wxDefaultSize );
    
    wxBoxSizer* bSizer1;
    bSizer1 = new wxBoxSizer( wxVERTICAL );
    bSizer1->SetMinSize( wxSize( 150,-1 ) );

    wxStaticBoxSizer* sbSizer1;
    sbSizer1 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Info") ), wxVERTICAL );
    
    wxBoxSizer* bSizer8;
    bSizer8 = new wxBoxSizer( wxHORIZONTAL );
    
    wxStaticBoxSizer* sbSizer4;
    sbSizer4 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Computer Name") ), wxVERTICAL );
    
    m_computerTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT("225.0.0.37"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer4->Add( m_computerTextCtrl, 0, wxEXPAND, 5 );
    
    bSizer8->Add( sbSizer4, 1, wxLEFT|wxRIGHT, 5 );
    
    wxStaticBoxSizer* sbSizer5;
    sbSizer5 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Port Number") ), wxVERTICAL );
    
    m_portTextCtrl = new wxTextCtrl( this, wxID_ANY, wxT("12345"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer5->Add( m_portTextCtrl, 0, wxEXPAND, 5 );
    
    bSizer8->Add( sbSizer5, 1, wxLEFT|wxRIGHT, 5 );
    
    sbSizer1->Add( bSizer8, 0, wxEXPAND, 5 );
    
    wxStaticBoxSizer* sbSizer3;
    sbSizer3 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Simulator Controls") ), wxHORIZONTAL );
    
    m_toggleBtn1 = new wxToggleButton( this, wxID_ANY, wxT("Start/Stop"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer3->Add( m_toggleBtn1, 0, wxRIGHT, 5 );
    
    m_resetButton = new wxButton( this, wxID_ANY, wxT("Reset"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer3->Add( m_resetButton, 0, wxALIGN_CENTER_HORIZONTAL|wxLEFT, 5 );
    
    wxString m_simScaleChoices[] = { wxT("m -> ft"), wxT("cm -> ft"), wxT("mm -> ft"), wxT("in -> ft") };
    int m_simScaleNChoices = sizeof( m_simScaleChoices ) / sizeof( wxString );
    m_simScale = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_simScaleNChoices, m_simScaleChoices, 0 );
    m_simScale->SetSelection( 1 );
    sbSizer3->Add( m_simScale, 0, wxLEFT, 5 );
    
    sbSizer1->Add( sbSizer3, 0, wxALIGN_CENTER_HORIZONTAL|wxALL|wxEXPAND, 5 );
    
    bSizer1->Add( sbSizer1, 0, wxALL|wxEXPAND, 5 );
    
    wxStaticBoxSizer* sbSizer2;
    sbSizer2 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Geometry Data Mapping") ), wxVERTICAL );
    
    wxBoxSizer* bSizer2;
    bSizer2 = new wxBoxSizer( wxVERTICAL );
    
    m_scrolledWindow1 = new wxScrolledWindow( this, wxID_ANY, wxDefaultPosition, wxSize( -1,100 ), wxHSCROLL|wxSUNKEN_BORDER|wxVSCROLL );
    m_scrolledWindow1->SetScrollRate( 5, 5 );
    m_scrolledWindow1->SetMinSize( wxSize( -1,100 ) );
    
    m_scrolledWindowSizer = new wxBoxSizer( wxVERTICAL );
    
    m_scrolledWindowSizer->SetMinSize( wxSize( -1,100 ) ); 
    /*wxBoxSizer* bSizer4;
    bSizer4 = new wxBoxSizer( wxHORIZONTAL );
    
    m_staticText1 = new wxStaticText( m_scrolledWindow1, wxID_ANY, wxT("1"), wxDefaultPosition, wxDefaultSize, 0 );
    m_staticText1->Wrap( -1 );
    bSizer4->Add( m_staticText1, 0, wxALIGN_CENTER, 5 );
    
    wxArrayString m_choice1Choices;
    m_choice1 = new wxChoice( m_scrolledWindow1, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_choice1Choices, 0 );
    m_choice1->SetSelection( 0 );
    bSizer4->Add( m_choice1, 0, wxALIGN_CENTER, 5 );
    
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
    
    m_scrolledWindowSizer->Add( bSizer41, 0, 0, 5 );*/
    
    m_scrolledWindow1->SetSizer( m_scrolledWindowSizer );
    m_scrolledWindow1->Layout();
    bSizer2->Add( m_scrolledWindow1, 1, wxEXPAND | wxALL, 5 );
    
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
    
    wxBoxSizer* bSizer81;
    bSizer81 = new wxBoxSizer( wxHORIZONTAL );
    
    wxBoxSizer* bSizer9;
    bSizer9 = new wxBoxSizer( wxVERTICAL );
    
    wxStaticBoxSizer* sbSizer8;
    sbSizer8 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Front Bird") ), wxVERTICAL );
    
    wxString m_frontBirdChoices[] = { wxT("None"), wxT("Bird1"), wxT("Bird2"), wxT("Bird3") };
    int m_frontBirdNChoices = sizeof( m_frontBirdChoices ) / sizeof( wxString );
    m_frontBird = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_frontBirdNChoices, m_frontBirdChoices, 0 );
    m_frontBird->SetSelection( 0 );
    sbSizer8->Add( m_frontBird, 0, 0, 5 );
    
    bSizer9->Add( sbSizer8, 0, wxEXPAND|wxRIGHT, 5 );
    
    wxStaticBoxSizer* sbSizer9;
    sbSizer9 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Left Rear Bird") ), wxVERTICAL );
    
    wxString m_leftRearBirdChoices[] = { wxT("None") };
    int m_leftRearBirdNChoices = sizeof( m_leftRearBirdChoices ) / sizeof( wxString );
    m_leftRearBird = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_leftRearBirdNChoices, m_leftRearBirdChoices, 0 );
    m_leftRearBird->SetSelection( 0 );
    sbSizer9->Add( m_leftRearBird, 0, 0, 5 );
    
    bSizer9->Add( sbSizer9, 0, wxEXPAND|wxRIGHT, 5 );
    
    wxStaticBoxSizer* sbSizer10;
    sbSizer10 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Right Rear Bird") ), wxVERTICAL );
    
    wxString m_rightRearBirdChoices[] = { wxT("None") };
    int m_rightRearBirdNChoices = sizeof( m_rightRearBirdChoices ) / sizeof( wxString );
    m_rightRearBird = new wxChoice( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_rightRearBirdNChoices, m_rightRearBirdChoices, 0 );
    m_rightRearBird->SetSelection( 0 );
    sbSizer10->Add( m_rightRearBird, 0, 0, 5 );
    
    bSizer9->Add( sbSizer10, 0, wxEXPAND|wxRIGHT, 5 );
    
    bSizer81->Add( bSizer9, 1, wxEXPAND, 5 );
    
    wxBoxSizer* bSizer10;
    bSizer10 = new wxBoxSizer( wxVERTICAL );
    
    wxStaticBoxSizer* sbSizer11;
    sbSizer11 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("X SIP Offset (ft)") ), wxVERTICAL );
    
    m_xSIPOffset = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer11->Add( m_xSIPOffset, 0, 0, 5 );
    
    bSizer10->Add( sbSizer11, 0, wxEXPAND|wxLEFT, 5 );
    
    wxStaticBoxSizer* sbSizer12;
    sbSizer12 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Y SIP Offset (ft)") ), wxVERTICAL );
    
    m_ySIPOffset = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer12->Add( m_ySIPOffset, 0, 0, 5 );
    
    bSizer10->Add( sbSizer12, 0, wxEXPAND|wxLEFT, 5 );
    
    wxStaticBoxSizer* sbSizer13;
    sbSizer13 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Z SIP Offset (ft)") ), wxVERTICAL );
    
    m_zSIPOffset = new wxTextCtrl( this, wxID_ANY, wxT("0.0"), wxDefaultPosition, wxDefaultSize, 0 );
    sbSizer13->Add( m_zSIPOffset, 0, 0, 5 );
    
    bSizer10->Add( sbSizer13, 0, wxEXPAND|wxLEFT, 5 );
    
    bSizer81->Add( bSizer10, 1, wxEXPAND, 5 );
    
    registrationSizer->Add( bSizer81, 1, wxEXPAND, 5 );
    
    m_registrationButton = new wxButton( this, wxID_ANY, wxT("Register"), wxDefaultPosition, wxDefaultSize, 0 );
    registrationSizer->Add( m_registrationButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5 );
    
    bSizer1->Add( registrationSizer, 0, wxALL|wxEXPAND, 5 );
    
    m_sdbSizer1 = new wxStdDialogButtonSizer();
    m_sdbSizer1OK = new wxButton( this, wxID_OK );
    m_sdbSizer1->AddButton( m_sdbSizer1OK );
    //m_sdbSizer1Apply = new wxButton( this, wxID_APPLY );
    //m_sdbSizer1->AddButton( m_sdbSizer1Apply );
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
    m_constrainedGeomChoice->Connect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
    m_registrationButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegisterButton ), NULL, this );
    m_applyButton->Connect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
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
    m_constrainedGeomChoice->Disconnect( wxEVT_COMMAND_CHOICE_SELECTED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnConstrainedGeometrySelection ), NULL, this );
    m_registrationButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnRegisterButton ), NULL, this );
    m_applyButton->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnApplyButton ), NULL, this );
    m_sdbSizer1OK->Disconnect( wxEVT_COMMAND_BUTTON_CLICKED, wxCommandEventHandler( DynamicVehicleSimToolBase::OnOKButton ), NULL, this );
    
}
