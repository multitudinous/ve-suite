///////////////////////////////////////////////////////////////////////////
// C++ code generated with wxFormBuilder (version Apr 16 2008)
// http://www.wxformbuilder.org/
//
// PLEASE DO "NOT" EDIT THIS FILE!
///////////////////////////////////////////////////////////////////////////

#include <ves/conductor/ScalarControlDialog.h>

///////////////////////////////////////////////////////////////////////////
using namespace ves::conductor;

ScalarControlDialog::ScalarControlDialog( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style ) : wxDialog( parent, id, title, pos, size, style )
{
	this->SetSizeHints( wxDefaultSize, wxDefaultSize );
	
	wxStaticBoxSizer* sbSizer1;
	sbSizer1 = new wxStaticBoxSizer( new wxStaticBox( this, wxID_ANY, wxT("Scalar Control") ), wxVERTICAL );
	
	wxBoxSizer* bSizer1;
	bSizer1 = new wxBoxSizer( wxHORIZONTAL );
	
	m_minTextCtrl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	m_minTextCtrl->SetToolTip( wxT("Min Scalar Value") );
	
	bSizer1->Add( m_minTextCtrl, 0, wxALL, 5 );
	
	m_minSlider = new wxSlider( this, wxID_ANY, 0, 0, 100, wxDefaultPosition, wxSize( 300,-1 ), wxSL_HORIZONTAL );
	m_minSlider->SetToolTip( wxT("Min Scalar Value") );
	
	bSizer1->Add( m_minSlider, 0, wxALL, 5 );
	
	sbSizer1->Add( bSizer1, 1, wxEXPAND, 5 );
	
	m_staticline1 = new wxStaticLine( this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
	sbSizer1->Add( m_staticline1, 0, wxEXPAND | wxALL, 5 );
	
	wxBoxSizer* bSizer2;
	bSizer2 = new wxBoxSizer( wxHORIZONTAL );
	
	m_maxTextControl = new wxTextCtrl( this, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, 0 );
	m_maxTextControl->SetToolTip( wxT("Max Scalar Value") );
	
	bSizer2->Add( m_maxTextControl, 0, wxALL, 5 );
	
	m_maxSlider = new wxSlider( this, wxID_ANY, 100, 0, 100, wxDefaultPosition, wxSize( 300,-1 ), wxSL_HORIZONTAL );
	m_maxSlider->SetToolTip( wxT("Max Scalar Value") );
	
	bSizer2->Add( m_maxSlider, 0, wxALL, 5 );
	
	sbSizer1->Add( bSizer2, 1, wxEXPAND, 5 );
	
	this->SetSizer( sbSizer1 );
	this->Layout();
	sbSizer1->Fit( this );
	
	// Connect Events
	m_minTextCtrl->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( ScalarControlDialog::OnMinTextInput ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_TOP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_BOTTOM, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_LINEUP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_LINEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_PAGEUP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_PAGEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_THUMBTRACK, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_THUMBRELEASE, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Connect( wxEVT_SCROLL_CHANGED, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_maxTextControl->Connect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( ScalarControlDialog::OnMaxTextInput ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_TOP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_BOTTOM, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_LINEUP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_LINEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_PAGEUP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_PAGEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_THUMBTRACK, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_THUMBRELEASE, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Connect( wxEVT_SCROLL_CHANGED, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
}

ScalarControlDialog::~ScalarControlDialog()
{
	// Disconnect Events
	m_minTextCtrl->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( ScalarControlDialog::OnMinTextInput ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_TOP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_BOTTOM, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_LINEUP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_LINEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_PAGEUP, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_PAGEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_THUMBTRACK, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_THUMBRELEASE, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_minSlider->Disconnect( wxEVT_SCROLL_CHANGED, wxScrollEventHandler( ScalarControlDialog::OnMinSlider ), NULL, this );
	m_maxTextControl->Disconnect( wxEVT_COMMAND_TEXT_UPDATED, wxCommandEventHandler( ScalarControlDialog::OnMaxTextInput ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_TOP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_BOTTOM, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_LINEUP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_LINEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_PAGEUP, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_PAGEDOWN, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_THUMBTRACK, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_THUMBRELEASE, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
	m_maxSlider->Disconnect( wxEVT_SCROLL_CHANGED, wxScrollEventHandler( ScalarControlDialog::OnMaxSlider ), NULL, this );
}
