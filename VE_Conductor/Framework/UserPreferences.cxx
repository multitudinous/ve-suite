/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
    EVT_CHECKBOX( ID_NAVIGATION_CHKBX, UserPreferences::OnNavigationCheck )
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
UserPreferences::UserPreferences( wxWindow* parent, 
                                            wxWindowID id, 
                                            const wxString& caption, 
                                            const wxPoint& pos, 
                                            const wxSize& size, 
                                            long style )
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
   preferenceMap[ "Interactive State" ] = false;
   preferenceMap[ "Auto Launch Nav Pane" ] = false;
   preferenceMap[ "Use Preferred Background Color" ] = false;
   preferenceMap[ "Shut Down Xplorer Option" ] = false;

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

    wxCheckBox* backgroundColorChkBx = 0;
    wxCheckBox* navigationChkBx = 0;
    wxCheckBox* shutdownModeChkBx = 0;
                   
   // Add page
   wxPanel* panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("General"));
   wxBoxSizer* itemBoxSizer2 = new wxBoxSizer(wxVERTICAL);
   panel->SetSizer(itemBoxSizer2);
   wxString choices[1];
   choices[ 0 ] = wxString( "Interactive Mode", wxConvUTF8 );
   prefChkBx = new wxCheckListBox( panel, ID_NAVIGATION_CHKBX, wxDefaultPosition, wxDefaultSize, 1, choices, 0, wxDefaultValidator, _("listBox") );
   itemBoxSizer2->Add( prefChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("Xplorer Settings"));
   wxBoxSizer* itemBoxSizer3 = new wxBoxSizer(wxVERTICAL);
   panel->SetSizer(itemBoxSizer3);
   wxBoxSizer* colorSizer = new wxBoxSizer(wxHORIZONTAL);
   backgroundColorChkBx = new wxCheckBox(panel, ID_NAVIGATION_CHKBX, wxT("Use Preferred Background Color"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
   backgroundColorButton = new wxButton( panel, ID_BACKGROUND_COLOR_BUTTON, _T("Background Color"), wxDefaultPosition, wxDefaultSize, 0 );
   colorSizer->Add(backgroundColorChkBx, 1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   colorSizer->Add(backgroundColorButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5);
   navigationChkBx = new wxCheckBox(panel, ID_NAVIGATION_CHKBX, wxT("Auto Launch Nav Pane"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
   shutdownModeChkBx = new wxCheckBox(panel, ID_NAVIGATION_CHKBX, wxT("Shut Down Xplorer Option"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );

   xplorerChoices[ 0 ] = wxString( "Use Preferred Background Color", wxConvUTF8 );
   xplorerChoices[ 1 ] = wxString( "Auto Launch Nav Pane", wxConvUTF8 );
   xplorerChoices[ 2 ] = wxString( "Shut Down Xplorer Option", wxConvUTF8 );

   backgroundColorChkBx->SetValue( preferenceMap[ "Use Preferred Background Color" ] );
   backgroundColorChkBx->IsChecked();
   navigationChkBx->SetValue( preferenceMap[ "Auto Launch Nav Pane" ] );
   navigationChkBx->IsChecked();
   shutdownModeChkBx->SetValue( preferenceMap[ "Shut Down Xplorer Option" ] );
   shutdownModeChkBx->IsChecked();

   itemBoxSizer3->Add( colorSizer, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   itemBoxSizer3->Add( navigationChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);
   itemBoxSizer3->Add( shutdownModeChkBx, 0, wxALIGN_LEFT|wxALL|wxEXPAND, 5);

   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("User Mode"));
   ///////////////////////////////////////
   panel = new wxPanel( GetBookCtrl(), -1, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL);
   GetBookCtrl()->AddPage(panel, _("Defaults"));
}
////////////////////////////////////////////////////////////////////////////////
void UserPreferences::OnNavigationCheck( wxCommandEvent& event )
{
   wxString mode = dynamic_cast< wxControl* >( event.GetEventObject() )->GetLabelText();

   preferenceMap[ ConvertUnicode( mode.c_str() ) ] = event.IsChecked();
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
    wxConfig* cfg = dynamic_cast<wxConfig*>(wxConfig::Get());
   
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

        if( iter->first == "Use Preferred Background Color" )
        {
            xplorerColor.clear();
            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Red", wxConvUTF8 ), 
                       &backgroundColor[ "Red" ] );
            xplorerColor.push_back( backgroundColor[ "Red" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Green", wxConvUTF8 ), 
                       &backgroundColor[ "Green" ] ); 
            xplorerColor.push_back( backgroundColor[ "Green" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Blue", wxConvUTF8 ), 
                       &backgroundColor[ "Blue" ] );  
            xplorerColor.push_back( backgroundColor[ "Blue" ] );

            cfg->Read( key + 
                       _T("/") + 
                       _T("BackgroundColor") +
                       _T("/") +
                       wxString( "Alpha", wxConvUTF8 ), 
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

        if( iter->first == "Use Preferred Background Color" )
        {
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Red", wxConvUTF8 ), 
                        backgroundColor[ "Red" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Green", wxConvUTF8 ), 
                        backgroundColor[ "Green" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Blue", wxConvUTF8 ), 
                        backgroundColor[ "Blue" ] );  
            cfg->Write( key + 
                        _T("/") + 
                        _T("BackgroundColor") +
                        _T("/") +
                        wxString( "Alpha", wxConvUTF8 ), 
                        backgroundColor[ "Alpha" ] );  
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
std::vector< double > UserPreferences::GetBackgroundColor()
{
    return xplorerColor;
}
