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
#include "ADOpenDialog.h"
#include <plugins/ConductorPluginEnums.h>
#include <vector>

BEGIN_EVENT_TABLE( ADOpenDialog, wxDialog )
    EVT_CLOSE( ADOpenDialog::OnClose )
    EVT_BUTTON( ADOPENDIALOG_CANCELBUTTON, ADOpenDialog::CancelButtonClick )
    EVT_BUTTON( ADOPENDIALOG_OKBUTTON, ADOpenDialog::OKButtonClick )
END_EVENT_TABLE()

ADOpenDialog::ADOpenDialog( wxWindow* parent, wxWindowID id, const wxString& title, const wxPoint& position, const wxSize& size, long style )
    : wxDialog( parent, id, title, position, size, style )
{
    CreateGUIControls();
}

ADOpenDialog::~ADOpenDialog()
{
}

void ADOpenDialog::CreateGUIControls()
{
    SetTitle( wxT( "DYNF File" ) );
    SetIcon( wxNullIcon );
    SetSize( 8, 8, 370, 138 );
    Center();

    Label = new wxStaticText( this, -1, wxT( "Aspen Dyn Project" ), wxPoint( 8, 16 ), wxDefaultSize, 0, wxT( "Label" ) );
    Label->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );

    CancelButton = new wxButton( this, ADOPENDIALOG_CANCELBUTTON, wxT( "Cancel" ), wxPoint( 275, 55 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "CancelButton" ) );
    CancelButton->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );

    OKButton = new wxButton( this, ADOPENDIALOG_OKBUTTON, wxT( "Ok" ), wxPoint( 195, 55 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "OKButton" ) );
    OKButton->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );
}

void ADOpenDialog::OnClose( wxCloseEvent& )
{
    Destroy();
}

void ADOpenDialog::OKButtonClick( wxCommandEvent& event )
{
    if( ComboBox->GetCurrentSelection() != wxNOT_FOUND )
    {
        EndModal( wxID_OK );
    }
}

void ADOpenDialog::CancelButtonClick( wxCommandEvent& event )
{
    EndModal( wxID_CANCEL );
}

void ADOpenDialog::SetPopulateFilenames( )
{
    wxDir pluginsDir( ::wxGetCwd() );
    wxString filename;

    //apw
    wxString ext = wxString( "*.dynf", wxConvUTF8 );
    std::vector< wxString > apwList;
    bool cont = pluginsDir.GetFirst( &filename, ext, wxDIR_FILES );
    while( cont )
    {
        filename.Truncate( filename.Len() - 5 );
        arrayStringFor_ComboBox.Add( filename );
        cont = pluginsDir.GetNext( &filename );
    }

    // construct combo box
    ComboBox = new wxComboBox( this, -1, wxT( "" ), wxPoint( 149, 13 ), wxSize( 208, 27 ), arrayStringFor_ComboBox, wxCB_READONLY, wxDefaultValidator, wxT( "ComboBox" ) );
    ComboBox->SetFont( wxFont( 12, wxSWISS, wxNORMAL, wxNORMAL, false, wxT( "Tahoma" ) ) );
}

wxString ADOpenDialog::GetFilename( )
{
    return arrayStringFor_ComboBox[ComboBox->GetCurrentSelection()];
}