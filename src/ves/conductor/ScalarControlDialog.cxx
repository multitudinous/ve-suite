/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
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
