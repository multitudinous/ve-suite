/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2006-10-25 17:27:44 -0500 (Wed, 25 Oct 2006) $
 * Version:       $Rev: 5850 $
 * Author:        $Author: mccdo $
 * Id:            $Id: UserPreferences.cxx 5850 2006-10-25 22:27:44Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <wx/sizer.h>
#include <wx/icon.h>
#include <wx/button.h>
#include <wx/msgdlg.h>
#include <wx/textdlg.h>
#include <wx/checklst.h>

#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Conductor/Framework/UserPreferences.h"

BEGIN_EVENT_TABLE( UserPreferences, wxDialog )
   EVT_CHECKLISTBOX( ID_PREFERENCE_CHKBX, UserPreferences::OnPreferenceCheck )
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
UserPreferences::UserPreferences( )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
UserPreferences::UserPreferences( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
   Create(parent, id, caption, pos, size, style );
}
////////////////////////////////////////////////////////////////////////////////
bool UserPreferences::Create( wxWindow* parent, wxWindowID id, const wxString& caption, const wxPoint& pos, const wxSize& size, long style )
{
   prefChkBx = NULL;
   interactiveState = false;
   //SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxDialog::Create( parent, id, caption, pos, size, style );

   CreateControls();
   
   GetSizer()->Fit(this);
   GetSizer()->SetSizeHints(this);
   Centre();
   SetAutoLayout( true );
   Refresh();
   wxSize temp = GetSize();
   temp.SetHeight( temp.GetHeight() +1);
   temp.SetWidth( temp.GetWidth()+1 );
   SetSize( temp );
   this->SetIcon( ve_icon32x32_xpm );
////@end UserPreferences creation
   return true;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::CreateControls()
{    
    UserPreferences* userPrefDialog = this;

    wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
    userPrefDialog->SetSizer(itemBoxSizer2);
    wxString choices[1];
    choices[ 0 ] = wxString( "Interactive Mode" );
    
    prefChkBx = new wxCheckListBox( userPrefDialog, ID_PREFERENCE_CHKBX, wxDefaultPosition, wxDefaultSize, 1, choices, 0, wxDefaultValidator, "listBox");
    itemBoxSizer2->Add( prefChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   ///////////////////////////////////////////////////////
    wxStdDialogButtonSizer* okCancelButton = new wxStdDialogButtonSizer();

    itemBoxSizer2->Add(okCancelButton, 0, wxALIGN_CENTER_HORIZONTAL|wxALL, 5);
    wxButton* itemButton26 = new wxButton( userPrefDialog, wxID_OK, _("Ok"), wxDefaultPosition, wxDefaultSize, 0 );
    //itemButton26->SetDefault();
    okCancelButton->AddButton(itemButton26);

    wxButton* itemButton27 = new wxButton( userPrefDialog, wxID_CANCEL, _("&Cancel"), wxDefaultPosition, wxDefaultSize, 0 );
    okCancelButton->AddButton(itemButton27);

    okCancelButton->Realize();
}
////////////////////////////////////////////////////////////////////////////////
/*
wxBitmap UserPreferences::GetBitmapResource( const wxString& name )
{
    // Bitmap retrieval
////@begin UserPreferences bitmap retrieval
    wxUnusedVar(name);
    return wxNullBitmap;
////@end UserPreferences bitmap retrieval
}
*/
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::OnPreferenceCheck( wxCommandEvent& WXUNUSED(event) )
{
   interactiveState = prefChkBx->IsChecked( 0 );
}
////////////////////////////////////////////////////////////////////////////////
bool UserPreferences::GetInteractiveMode( void )
{
   return interactiveState;
}
