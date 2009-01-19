/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/conductor/PortDialog.h>
#include <iostream>

#include <wx/button.h>
#include <wx/textctrl.h>
#include <wx/sizer.h>
#include <wx/stattext.h>
using namespace ves::conductor;

//BEGIN_EVENT_TABLE(PortDialog, wxDialog)
//EVT_BUTTON(wxID_OK, PortDialog::OnOK)
//END_EVENT_TABLE()

PortDialog::PortDialog( const wxString& title )
        : wxDialog(( wxWindow * ) NULL, -1, title, wxDefaultPosition, wxDefaultSize )
{
    wxSize sz1, sz2, syn;

    sz1.Set( 120, 17 );
    sz2.Set( 250, 300 );
    wxBoxSizer *top_sizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer *first_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer *second_row = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer *third_row = new wxBoxSizer( wxHORIZONTAL );

    syngas = new ListTable( this, -1, wxDefaultPosition,
                            sz2 );

    //PortDialog::SetAffirmativeId(wxID_OK);
    ok = new wxButton( this, wxID_OK, _( "OK" ) );

    top_sizer->Add( first_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( second_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( third_row, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( syngas, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( ok, 0, wxALIGN_CENTER_HORIZONTAL );

    first_row->Add( new wxStaticText( this, -1, _( "Temperature (F): " ), wxDefaultPosition, sz1 ), 1, wxALIGN_LEFT );
    temp = new wxTextCtrl( this, -1, _( "" ), wxDefaultPosition, sz1, wxTE_READONLY );
    first_row->Add( temp, 0, wxALIGN_CENTER_HORIZONTAL );

    second_row->Add( new wxStaticText( this, -1, _( "Pressure (psi): " ), wxDefaultPosition, sz1 ), 1, wxALIGN_LEFT );
    pres = new wxTextCtrl( this, -1, _( "" ), wxDefaultPosition, sz1, wxTE_READONLY );
    second_row->Add( pres, 0, wxALIGN_CENTER_HORIZONTAL );

    third_row->Add( new wxStaticText( this, -1, _( "Flow rate (ton/hr): " ), wxDefaultPosition, sz1 ), 1, wxALIGN_LEFT );
    flrt = new wxTextCtrl( this, -1, _( "" ), wxDefaultPosition, sz1, wxTE_READONLY );
    third_row->Add( flrt, 0, wxALIGN_CENTER_HORIZONTAL );

    SetSizer( top_sizer );
    //SetAutoLayout(TRUE);
    top_sizer->Fit( this );

}


void PortDialog::Set3Cols( const std::vector<wxString>& col1, const std::vector<wxString>& col2, const std::vector<wxString>& col3 )
{
    int len = col1.size();
    int i;

    std::vector<wxString> row;

    row.resize( 3 );
    syngas->ClearAll();
    for( i = 0; i < len; i++ )
    {
        row[0] = col1[i];
        row[1] = col2[i];
        row[2] = col3[i];
        syngas->AddRow( row );
    }
}

PortDialog::~PortDialog()
{}

void PortDialog::SetVal( const wxString &var, const wxString &val )
{
    if( var == _( "TEMP" ) )
        temp->SetValue( val );
    else if( var == _( "PRES" ) )
        pres->SetValue( val );
    else if( var == _( "FLRT" ) )
        flrt->SetValue( val );
}
