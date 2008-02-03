/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
#include <ves/conductor/QueryInputsDlg.h>
using namespace ves::conductor;

BEGIN_EVENT_TABLE( QueryInputsDlg, wxDialog )
    //EVT_CLOSE( QueryInputsDlg::OnClose )
    EVT_BUTTON( ID_WXBUTTON2, QueryInputsDlg::WxButton2Click )
    EVT_BUTTON( ID_WXBUTTON1, QueryInputsDlg::WxButton1Click )
    EVT_BUTTON( ID_WXBUTTON3, QueryInputsDlg::WxButton3Click )
    EVT_BUTTON( ID_WXBUTTON4, QueryInputsDlg::WxButton4Click )
END_EVENT_TABLE()

QueryInputsDlg::QueryInputsDlg( wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style )
        : wxDialog( parent, id, title, position, size, style )
{
    CreateGUIControls();
}

QueryInputsDlg::~QueryInputsDlg()
{}

void QueryInputsDlg::CreateGUIControls()
{
    SetTitle( wxT( "Parameter Selection" ) );
    SetIcon( wxNullIcon );
    SetSize( 8, 8, 457, 522 );
    Center();

    submit = false;

    WxStaticText2 = new wxStaticText( this, ID_WXSTATICTEXT2, wxT( "Selected Parameters" ), wxPoint( 311, 5 ), wxDefaultSize, 0, wxT( "WxStaticText2" ) );
    WxStaticText1 = new wxStaticText( this, ID_WXSTATICTEXT1, wxT( "All Parameters" ), wxPoint( 55, 6 ), wxDefaultSize, 0, wxT( "WxStaticText1" ) );
    WxButton4 = new wxButton( this, ID_WXBUTTON4, wxT( "Cancel" ), wxPoint( 187, 439 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "WxButton4" ) );
    WxButton3 = new wxButton( this, ID_WXBUTTON3, wxT( "Submit" ), wxPoint( 187, 411 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "WxButton3" ) );
    WxButton2 = new wxButton( this, ID_WXBUTTON2, wxT( "<<" ), wxPoint( 198, 69 ), wxSize( 50, 25 ), 0, wxDefaultValidator, wxT( "WxButton2" ) );
    WxButton1 = new wxButton( this, ID_WXBUTTON1, wxT( ">>" ), wxPoint( 198, 35 ), wxSize( 50, 25 ), 0, wxDefaultValidator, wxT( "WxButton1" ) );

    wxArrayString arrayStringFor_WxListBox2;
    WxListBox2 = new wxListBox( this, ID_WXLISTBOX2, wxPoint( 264, 24 ), wxSize( 178, 450 ), arrayStringFor_WxListBox2, wxLB_SINGLE );

    wxArrayString arrayStringFor_WxListBox1;
    WxListBox1 = new wxListBox( this, ID_WXLISTBOX1, wxPoint( 8, 23 ), wxSize( 178, 450 ), arrayStringFor_WxListBox1, wxLB_SINGLE );
}

/*void QueryInputsDlg::OnClose( wxCloseEvent& event )
{
    submit = false;
    Destroy();
}*/

void QueryInputsDlg::WxButton1Click( wxCommandEvent& event )
{
    if( WxListBox2->FindString( WxListBox1->GetStringSelection() ) == -1 )
        WxListBox2->Append( WxListBox1->GetStringSelection() );
}

void QueryInputsDlg::WxButton2Click( wxCommandEvent& event )
{
    if( WxListBox2->GetSelection() != wxNOT_FOUND )
        WxListBox2->Delete( WxListBox2->GetSelection() );
}

void QueryInputsDlg::WxButton3Click( wxCommandEvent& event )
{
    submit = true;
    Destroy();
}

void QueryInputsDlg::WxButton4Click( wxCommandEvent& event )
{
    Destroy();
}

void QueryInputsDlg::AppendList( const char * input )
{
    WxListBox1->Append( wxString( input, wxConvUTF8 ) );
}

bool QueryInputsDlg::IsSubmit()
{
    return submit;
}

wxString QueryInputsDlg::GetDataString( int i )
{
    return WxListBox2->GetString( i );
}

int QueryInputsDlg::GetDataSize()
{
    return WxListBox2->GetStrings().GetCount();
}