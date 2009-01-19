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
#include <ves/conductor/FindDialog.h>
#include <ves/conductor/ConductorLibEnums.h>

#include <wx/stattext.h>
#include <wx/choice.h>
#include <wx/button.h>
#include <wx/dialog.h>
using namespace ves::conductor;

BEGIN_EVENT_TABLE( FindDialog, wxDialog )
    EVT_BUTTON( FINDDIALOG_FINDBUTTON, FindDialog::FindButtonClick )
    EVT_CHOICE( FINDDIALOG_WXCHOICE1, FindDialog::GetChoice )
    EVT_CHOICE( FINDDIALOG_WXCHOICE2, FindDialog::GetChoice )
END_EVENT_TABLE()

FindDialog::FindDialog( wxWindow *parent, wxWindowID id, const wxString &title, const wxPoint &position, const wxSize& size, long style )
        : wxDialog( parent, id, title, position, size, style )
{
    CreateGUIControls();
}

FindDialog::~FindDialog()
{}

void FindDialog::CreateGUIControls()
{
    SetTitle( wxT( "Find" ) );
    SetIcon( wxNullIcon );
    SetSize( 8, 8, 261, 144 );
    Center();

    UnitLabel = new wxStaticText( this, FINDDIALOG_UNITLABEL, wxT( "Unit Operations" ), wxPoint( 4, 22 ), wxDefaultSize, 0, wxT( "UnitLabel" ) );
    UnitLabel->SetFont( wxFont( 10, wxSWISS, wxNORMAL, wxBOLD, FALSE, wxT( "Times New Roman" ) ) );

    StreamLabel = new wxStaticText( this, FINDDIALOG_STREAMLABEL, wxT( "Streams" ), wxPoint( 4, 45 ), wxDefaultSize, 0, wxT( "StreamLabel" ) );
    StreamLabel->SetFont( wxFont( 10, wxSWISS, wxNORMAL, wxBOLD, FALSE, wxT( "Times New Roman" ) ) );

    wxArrayString arrayStringFor_WxChoice1;
    WxChoice1 = new wxChoice( this, FINDDIALOG_WXCHOICE1, wxPoint( 96, 19 ), wxSize( 145, 21 ), arrayStringFor_WxChoice1, 0, wxDefaultValidator, wxT( "WxChoice1" ) );
    WxChoice1->SetSelection( -1 );

    wxArrayString arrayStringFor_WxChoice2;
    WxChoice2 = new wxChoice( this, FINDDIALOG_WXCHOICE2, wxPoint( 96, 42 ), wxSize( 145, 21 ), arrayStringFor_WxChoice2, 0, wxDefaultValidator, wxT( "WxChoice2" ) );
    WxChoice2->SetSelection( -1 );

    CancelButton = new wxButton( this, wxID_CANCEL, wxT( "Cancel" ), wxPoint( 165, 67 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "CancelButton" ) );

    FindButton = new wxButton( this, FINDDIALOG_FINDBUTTON, wxT( "Find" ), wxPoint( 86, 67 ), wxSize( 75, 25 ), 0, wxDefaultValidator, wxT( "FindButton" ) );
    
    //initialize member variables
    selectedModulePos = wxNOT_FOUND;
    type = wxNOT_FOUND;
    mLastChoice = wxNOT_FOUND;
}


void FindDialog::FindButtonClick( wxCommandEvent& event )
{
    if ( mLastChoice == FINDDIALOG_WXCHOICE1 )
    {
        selectedModule = WxChoice1->GetString( WxChoice1->GetSelection() );
        selectedModulePos = WxChoice1->GetSelection();
        type = 0;
    }
    else if( mLastChoice == FINDDIALOG_WXCHOICE2 )
    {
        selectedModule = WxChoice2->GetString( WxChoice2->GetSelection() );
        selectedModulePos = WxChoice2->GetSelection();
        type = 1;
    }
    this->Close();
}

void FindDialog::SetModuleList( std::vector< std::string > modules )
{
    for( int i = 0; i < ( int )modules.size(); i++ )
        WxChoice1->Insert( wxString( modules[i].c_str(), wxConvUTF8 ), i );
}

void FindDialog::SetStreamList( std::vector< std::string > modules )
{
    for( int i = 0; i < ( int )modules.size(); i++ )
        WxChoice2->Insert( wxString( modules[i].c_str(), wxConvUTF8 ), i );
}

const char * FindDialog::GetSelectedModule()
{
    return selectedModule.mb_str();
}

std::pair< int, int > FindDialog::GetSelectedModulePos()
{
    std::pair< int, int > returnValue;
    returnValue.first = type;
    returnValue.second = selectedModulePos;
    return returnValue;
}
void FindDialog::GetChoice(wxCommandEvent &event)
{
    mLastChoice = event.GetId();
}
