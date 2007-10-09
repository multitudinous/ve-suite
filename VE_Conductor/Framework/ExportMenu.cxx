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
 * Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
//Don't move Frame.h below ExportMenu.h
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Framework/ExportMenu.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/CommandPtr.h"
#include "VE_Open/XML/DataValuePairPtr.h"
#include "VE_Open/XML/DataValuePair.h"

#include <wx/filedlg.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>

/*BEGIN_EVENT_TABLE( ExportMenu, wxMenu )
    EVT_MENU( ExportMenu::EXPORT_SCREEN_SHOT, ExportMenu::OnScreenShot )
    EVT_MENU( ExportMenu::EXPORT_DOT_FILE, ExportMenu::OnDOTFile )
END_EVENT_TABLE()*/

////////////////////////////////////////////////////////////////////////////////
ExportMenu::ExportMenu() : wxMenu()
{
    CreateExportMenu();
}
////////////////////////////////////////////////////////////////////////////////
ExportMenu::~ExportMenu()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::CreateExportMenu()
{
    Append( ExportMenu::EXPORT_SCREEN_SHOT, _( "Screen Shot" ) );
    Append( ExportMenu::EXPORT_DOT_FILE, _( "OSG Graph File" ) );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnScreenShot( wxCommandEvent& event )
{
    wxFileDialog dialog( NULL, _T("Save Screen Shot..."),
        ::wxGetCwd(),
        _T("xploreScreenCap.jpg"),
        _T("JPG Image (*.jpg)|*.jpg"),
        wxFD_SAVE|wxFD_OVERWRITE_PROMPT
        );
    
    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }
    
    wxFileName vesFileName( dialog.GetPath() );
    vesFileName.ClearExt();
    vesFileName.SetExt( _("jpg") );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
    if( !success )
    {
        wxMessageBox( _("Can't save the screen capture on another drive."), 
                      _("JPG Write Error"), wxOK | wxICON_INFORMATION );
        return;
    }
    
    VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
    VE_XML::CommandPtr command = new VE_XML::Command();
    std::string mode = ConvertUnicode( vesFileName.GetFullPath().c_str() );
    dvp->SetData( std::string( "Filename" ), mode );
    command->SetCommandName( std::string( "SCREEN_SHOT" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->
        SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnDOTFile( wxCommandEvent& event )
{
    wxFileDialog dialog( NULL, _T("Save OSG Graph File..."),
        ::wxGetCwd(),
        _T("osgGraph.dot"),
        _T("DOT Graph (*.dot)|*.dot"),
        wxFD_SAVE|wxFD_OVERWRITE_PROMPT
        );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }
    
    wxFileName vesFileName( dialog.GetPath() );
    vesFileName.ClearExt();
    vesFileName.SetExt( _("dot") );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );   
    if( !success )
    {
        wxMessageBox( _("Can't save the graph file on another drive."), 
                      _("DOT Write Error"), wxOK | wxICON_INFORMATION );
        return;
    }
    
    VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
    VE_XML::CommandPtr command = new VE_XML::Command();
    std::string mode = ConvertUnicode( vesFileName.GetFullPath().c_str() );
    dvp->SetData( std::string( "Filename" ), mode );
    command->SetCommandName( std::string( "DOT_FILE" ) );
    command->AddDataValuePair( dvp );
    
    VE_Conductor::CORBAServiceList::instance()->
        SendCommandStringToXplorer( command );
}
