/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/TextResultDialog.h>
#include <ves/conductor/TexTable.h>

#include <iostream>

#include <wx/stattext.h>
#include <wx/statbox.h>
#include <wx/sizer.h>
#include <wx/button.h>
using namespace ves::conductor;


//BEGIN_EVENT_TABLE(TextResultDialog, wxDialog)
//EVT_BUTTON(wxID_OK, TextResultDialog::SetAffirmativeId(wxID_OK))
//END_EVENT_TABLE()

TextResultDialog::TextResultDialog( wxWindow * parent, const wxString& title, wxSize tabsize )
        : UIDialog(( wxWindow * )parent, -1, title )
{
    wxSize syn;
    wxBoxSizer* toptop = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* left_margin = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* top_sizer = new wxBoxSizer( wxVERTICAL );
    wxBoxSizer* right_margin = new wxBoxSizer( wxHORIZONTAL );


    //sz2.Set(250, 300);

    left_margin->Add( 10, 10 );
    right_margin->Add( 10, 10 );
    toptop->Add( left_margin, 0, wxALIGN_LEFT );
    toptop->Add( top_sizer, 1,  wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    toptop->Add( right_margin, 0, wxALIGN_RIGHT );

    syngas = new TexTable( this, -1, wxDefaultPosition,
                           tabsize );

    //TextResultDialog::SetAffirmativeId(wxID_OK);
    ok = new wxButton( this, wxID_OK, _( "OK" ) );

    top_sizer->Add( 10, 10, 0 );
    top_sizer->Add(
        new wxStaticText( this, -1, wxString(), wxDefaultPosition, wxDefaultSize ),
        0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( syngas, 0, wxALIGN_CENTER_HORIZONTAL | wxEXPAND );
    top_sizer->Add( 10, 5, 0 );
    top_sizer->Add( ok, 0, wxALIGN_CENTER_HORIZONTAL );
    top_sizer->Add( 10, 10, 0 ); //the bottom margin

    SetSizer( toptop );
    SetAutoLayout( TRUE );
    toptop->Fit( this );
}


void TextResultDialog::Set2Cols( const std::vector<wxString>& col1, const std::vector<wxString>& col2 )
{
    int len = col1.size();
    int i;

    std::vector<wxString> row;

    row.resize( 2 );
    //  syngas->Clear();
    for( i = 0; i < len; i++ )
    {
        row[0] = col1[i];
        row[1] = col2[i];

        syngas->AddRow( row );
    }
}

TextResultDialog::~TextResultDialog()
{}

