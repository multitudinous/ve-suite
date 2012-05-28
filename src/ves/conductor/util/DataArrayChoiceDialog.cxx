/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
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

#include <ves/conductor/util/DataArrayChoiceDialog.h>

///////////////////////////////////////////////////////////////////////////

DataArrayChoiceDialog::DataArrayChoiceDialog( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& pos, const wxSize& size, long style ) : wxDialog( parent, id, title, pos, size, style )
{
    this->SetSizeHints( wxDefaultSize, wxDefaultSize );

    wxBoxSizer* bSizer1;
    bSizer1 = new wxBoxSizer( wxVERTICAL );

    wxArrayString m_checkList1Choices;
    m_checkList1 = new wxCheckListBox( this, wxID_ANY, wxDefaultPosition, wxSize( 200, -1 ), m_checkList1Choices, wxLB_ALWAYS_SB | wxLB_MULTIPLE | wxLB_NEEDED_SB );
    m_checkList1->SetMinSize( wxSize( 200, 200 ) );

    bSizer1->Add( m_checkList1, 0, wxALIGN_CENTER_HORIZONTAL | wxALL, 5 );

    m_sdbSizer1 = new wxStdDialogButtonSizer();
    m_sdbSizer1Apply = new wxButton( this, wxID_OK );
    m_sdbSizer1->AddButton( m_sdbSizer1Apply );
    m_sdbSizer1Cancel = new wxButton( this, wxID_CANCEL );
    m_sdbSizer1->AddButton( m_sdbSizer1Cancel );
    m_sdbSizer1->Realize();
    bSizer1->Add( m_sdbSizer1, 1, wxALL | wxEXPAND, 10 );

    this->SetSizer( bSizer1 );
    this->Layout();
    bSizer1->Fit( this );
}

DataArrayChoiceDialog::~DataArrayChoiceDialog()
{
}
