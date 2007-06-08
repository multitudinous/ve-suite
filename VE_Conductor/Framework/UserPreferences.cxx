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
#include <wx/button.h>
#include <wx/checkbox.h>
#include <wx/colordlg.h>


//#include <iostream>
#include "VE_Conductor/Utilities/CORBAServiceList.h"

#include "VE_Installer/installer/installerImages/ve_icon32x32.xpm"
#include "VE_Conductor/Framework/UserPreferences.h"
#include "VE_Conductor/GUIPlugin/UserPreferencesDataBuffer.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_Conductor;

BEGIN_EVENT_TABLE( UserPreferences, wxDialog )
   EVT_CHECKLISTBOX( ID_PREFERENCE_CHKBX, UserPreferences::OnPreferenceCheck )
   EVT_CHECKBOX( ID_NAVIGATION_CHKBX, UserPreferences::OnNavigationCheck )
   EVT_CHECKBOX( ID_BACKGROUND_COLOR_CHKBX, UserPreferences::OnBackgroundColorCheck )
   EVT_BUTTON( ID_BACKGROUND_COLOR_BUTTON, UserPreferences::OnSetBackgroundColor )
END_EVENT_TABLE()
////////////////////////////////////////////////////////////////////////////////
UserPreferences::UserPreferences( )
{
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 0.0f );
   xplorerColor.push_back( 1.0f );
   xplorerWxColor = new wxColourData();
   xplorerWxColor->SetChooseFull(true);
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
   preferenceMap[ "Use Preferred Background Color" ] = false;

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
   wxBoxSizer* colorSizer = new wxBoxSizer(wxHORIZONTAL);
   backgroundColorChkBx = new wxCheckBox(panel, ID_BACKGROUND_COLOR_CHKBX, wxT("Use Preferred Background Color"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
   backgroundColorButton = new wxButton( panel, ID_BACKGROUND_COLOR_BUTTON, _T("Background Color"), wxDefaultPosition, wxDefaultSize, 0 );
   colorSizer->Add(backgroundColorChkBx, 1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   colorSizer->Add(backgroundColorButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   navigationChkBx = new wxCheckBox(panel, ID_NAVIGATION_CHKBX, wxT("Auto Launch Nav Pane"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );

   xplorerChoices[ 0 ] = wxString( "Use Preferred Background Color", wxConvUTF8 );
   xplorerChoices[ 1 ] = wxString( "Auto Launch Nav Pane", wxConvUTF8 );
   backgroundColorChkBx->SetValue( preferenceMap[ "Use Preferred Background Color" ] );
   backgroundColorChkBx->IsChecked();
   navigationChkBx->SetValue( preferenceMap[ "Auto Launch Nav Pane" ] );
   navigationChkBx->IsChecked();

   itemBoxSizer3->Add( colorSizer, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   itemBoxSizer3->Add( navigationChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("User Mode"));
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("Defaults"));
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
void UserPreferences::OnNavigationCheck( wxCommandEvent& event )
{
   wxString mode = xplorerChoices[ event.GetSelection() ];

   preferenceMap[ "Auto Launch Nav Pane" ] = navigationChkBx->IsChecked();
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::OnBackgroundColorCheck( wxCommandEvent& event )
{
   wxString mode = xplorerChoices[ event.GetSelection() ];

   preferenceMap[ "Use Preferred Background Color" ] = backgroundColorChkBx->IsChecked();
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::OnSetBackgroundColor( wxCommandEvent& event )
{
   serviceList = VE_Conductor::CORBAServiceList::instance();

   VE_XML::Command bkColor( UserPreferencesDataBuffer::instance()->GetCommand( "CHANGE_BACKGROUND_COLOR" ) );
   if ( bkColor.GetCommandName() != "NULL" )
   {
      bkColor.GetDataValuePair( "Background Color" )->GetData( xplorerColor );
   }

   wxColourDialog colorDlg(this,NULL);

   colorDlg.SetTitle(wxString("Xplorer Background Color", wxConvUTF8));

   if (colorDlg.ShowModal() == wxID_OK)
   {	   
      wxColourData colorData = colorDlg.GetColourData();  
      wxColour col = colorData.GetColour();

      xplorerColor.clear();
      xplorerColor.push_back(static_cast<double>(col.Red())/255.0);
      xplorerColor.push_back(static_cast<double>(col.Green())/255.0);
      xplorerColor.push_back(static_cast<double>(col.Blue())/255.0);
      xplorerColor.push_back(1.0);

      backgroundColor[ "Red" ] = xplorerColor.at(0);
      backgroundColor[ "Green" ] = xplorerColor.at(1);
      backgroundColor[ "Blue" ] = xplorerColor.at(2);
      backgroundColor[ "Alpha" ] = xplorerColor.at(3);

      // Create the command and data value pairs
      VE_XML::DataValuePair* dataValuePair = new VE_XML::DataValuePair();
      dataValuePair->SetData(std::string("Background Color"),xplorerColor);
      VE_XML::Command* veCommand = new VE_XML::Command();
      veCommand->SetCommandName(std::string("CHANGE_BACKGROUND_COLOR"));
      veCommand->AddDataValuePair(dataValuePair);

      serviceList->SendCommandStringToXplorer( veCommand );
         
      UserPreferencesDataBuffer::instance()->SetCommand( "CHANGE_BACKGROUND_COLOR", *veCommand );
      delete veCommand;
   }
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
    wxConfig* cfg = dynamic_cast<wxConfig*>(wxConfig::Get());//new wxConfig( _("VE-Conductor") );
   
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

        std::map< std::string, double >::iterator colorIter;
        if( iter->first == "Use Preferred Background Color" )
        {
            xplorerColor.clear();
            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Red" ), 
                       &backgroundColor[ "Red" ] );
            xplorerColor.push_back( backgroundColor[ "Red" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Green" ), 
                       &backgroundColor[ "Green" ] ); 
            xplorerColor.push_back( backgroundColor[ "Green" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Blue" ), 
                       &backgroundColor[ "Blue" ] );  
            xplorerColor.push_back( backgroundColor[ "Blue" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Alpha" ), 
                       &backgroundColor[ "Alpha" ] );
            xplorerColor.push_back( backgroundColor[ "Alpha" ] );
        }
        
    }
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::WriteConfiguration( void )
{
    wxConfig* cfg = dynamic_cast<wxConfig*>(wxConfig::Get());
    wxString key = _T("UserPreferences");
    std::map< std::string, bool >::iterator iter;
    for ( iter = preferenceMap.begin(); iter != preferenceMap.end(); ++iter )
    {
        cfg->Write( key + 
                    _T("/") + 
                    wxString( iter->first.c_str(), wxConvUTF8 ), 
                    iter->second );

        std::map< std::string, double >::iterator colorIter;
        if( iter->first == "Use Preferred Background Color" )
        {
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Red" ), 
                        backgroundColor[ "Red" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Green" ), 
                        backgroundColor[ "Green" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Blue" ), 
                        backgroundColor[ "Blue" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Alpha" ), 
                        backgroundColor[ "Alpha" ] );  
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< double > UserPreferences::GetBackgroundColor()
{
    return xplorerColor;
}