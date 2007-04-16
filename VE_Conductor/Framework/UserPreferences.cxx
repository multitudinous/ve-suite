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
#include <wx/generic/propdlg.h>
#include <wx/bookctrl.h>
#include <wx/panel.h>
#include <wx/config.h>

//#include <iostream>

#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Conductor/Framework/UserPreferences.h"

BEGIN_EVENT_TABLE( UserPreferences, wxDialog )
   EVT_CHECKLISTBOX( ID_PREFERENCE_CHKBX, UserPreferences::OnPreferenceCheck )
   EVT_CHECKLISTBOX( ID_XPLORER_CHKBX, UserPreferences::OnXplorerCheck )
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
UserPreferences::UserPreferences( )
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
UserPreferences::~UserPreferences()
{
   WriteConfiguration();
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
   //SetExtraStyle(GetExtraStyle()|wxWS_EX_BLOCK_EVENTS);
   wxPropertySheetDialog::Create( parent, id, caption, pos, size, style );
   ///Set the map
   preferenceMap[ "Interactive_State" ] = false;
   preferenceMap[ "Auto Launch Nav Pane" ] = false;
   ///Read from wxConfig
   ReadConfiguration();
   ///Read from ves file
   
   ///Update the preferences pane
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

   return true;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::CreateControls()
{    
   UserPreferences* userPrefDialog = this;

   CreateButtons(wxOK|wxCANCEL|wxHELP);

   // Add page
   wxPanel* panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("General"));
   wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
   panel->SetSizer(itemBoxSizer2);
   wxString choices[1];
   choices[ 0 ] = wxString( "Interactive Mode", wxConvUTF8 );
   prefChkBx = new wxCheckListBox( panel, ID_PREFERENCE_CHKBX, wxDefaultPosition, wxDefaultSize, 1, choices, 0, wxDefaultValidator, _("listBox") );
   itemBoxSizer2->Add( prefChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("Xplorer Settings"));
   wxBoxSizer* itemBoxSizer3 = new wxBoxSizer(wxVERTICAL);
   panel->SetSizer(itemBoxSizer3);
   xplorerChoices[ 0 ] = wxString( "Auto Launch Nav Pane", wxConvUTF8 );
   xplorerPrefChkBx = new wxCheckListBox( panel, ID_XPLORER_CHKBX, wxDefaultPosition, wxDefaultSize, 1, xplorerChoices, 0, wxDefaultValidator, _("listBox") );
   xplorerPrefChkBx->Check( 0, preferenceMap[ "Auto Launch Nav Pane" ] );
   itemBoxSizer3->Add( xplorerPrefChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("User Mode"));
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("Defaults"));
   ///////////////////////////////////////
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
   preferenceMap[ "Interactive_State" ] = prefChkBx->IsChecked( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::OnXplorerCheck( wxCommandEvent& event )
{
   wxString mode = xplorerChoices[ event.GetSelection() ];
   //std::cout << ConvertUnicode( mode.c_str() ) << " " 
   //            << xplorerPrefChkBx->IsChecked( event.GetSelection() ) << std::endl;
   preferenceMap[ ConvertUnicode( mode.c_str() ) ] = 
               xplorerPrefChkBx->IsChecked( event.GetSelection() );
}
////////////////////////////////////////////////////////////////////////////////
bool UserPreferences::GetMode( std::string mode )
{
   std::map< std::string, bool >::iterator iter;
   iter = preferenceMap.find( mode );
   if ( iter != preferenceMap.end() )
   {
      return iter->second;
   }
   return false;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::ReadConfiguration( void )
{
   wxConfig* cfg = new wxConfig( _("VE-Conductor") );
   
   wxString key = _T("UserPreferences");
   if ( !cfg->Exists( key ) ) 
   {
      return;
   }
   
   std::map< std::string, bool >::iterator iter;
   for ( iter = preferenceMap.begin(); iter != preferenceMap.end(); ++iter )
   {
      cfg->Read( key + 
                 _T("/") + 
                 wxString( iter->first.c_str(), wxConvUTF8 ), 
                 &iter->second, false);
      //std::cout << iter->second << " " << iter->first << std::endl;
   }
   
   delete cfg;
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::WriteConfiguration( void )
{
   wxConfig* cfg = new wxConfig( _("VE-Conductor") );
   
   wxString key = _T("UserPreferences");
   std::map< std::string, bool >::iterator iter;
   for ( iter = preferenceMap.begin(); iter != preferenceMap.end(); ++iter )
   {
      cfg->Write( key + 
                  _T("/") + 
                  wxString( iter->first.c_str(), wxConvUTF8 ), 
                  iter->second );
   }
   
   delete cfg;
}
