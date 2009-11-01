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
// --- VE-Suite Includes --- //
//Don't move Frame.h below ExportMenu.h
#include <ves/conductor/util/CORBAServiceList.h>
#include "ExportMenu.h"
#include "ConductorAppEnums.h"

#include <ves/open/xml/Command.h>
#include <ves/open/xml/CommandPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/DataValuePair.h>

#include <wx/filedlg.h>
#include <wx/filename.h>
#include <wx/msgdlg.h>

using namespace ves::open::xml;
using namespace ves::conductor::util;

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
    Append( EXPORTMENU_SCREEN_SHOT, _( "Screen Shot" ) );
    Append( EXPORTMENU_MOVIE_CAPTURE, _( "Movie Capture On" ) );
    Append( EXPORTMENU_MOVIE_CAPTURE_OFF, _( "Movie Capture Off" ) );
    Append( EXPORTMENU_DOT_FILE, _( "OSG Graph File" ) );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnScreenShot( wxCommandEvent& event )
{
    wxFileName vesFileName;
    if( event.GetId() == EXPORTMENU_SCREEN_SHOT )
    {
        wxFileDialog dialog( NULL, _T( "Save Screen Shot..." ),
                            ::wxGetCwd(),
                            _T( "xploreScreenCap" ),
                            _T( "JPG Image (*.jpg;*.png)|*.jpg;*.png" ),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT
                            );
        
        if( dialog.ShowModal() != wxID_OK )
        {
            return;
        }
        
        vesFileName = dialog.GetPath();
    }
    else if( event.GetId() == EXPORTMENU_MOVIE_CAPTURE )
    {
        wxFileDialog dialog( NULL, _T( "Save Movie Capture..." ),
                            ::wxGetCwd(),
                            _T( "xplorer_movie_capture" ),
                            _T( "JPG Image (*.jpg;*.png)|*.jpg;*.png" ),
                            wxFD_SAVE | wxFD_OVERWRITE_PROMPT
                            );
        
        if( dialog.ShowModal() != wxID_OK )
        {
            return;
        }
        
        vesFileName = dialog.GetPath();
    }

    if( event.GetId() != EXPORTMENU_MOVIE_CAPTURE_OFF )
    {
        vesFileName.ClearExt();
        //vesFileName.SetExt( _( "jpg" ) );
        bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );
        if( !success )
        {
            wxMessageBox( _( "Can't save the screen capture on another drive." ),
                         _( "JPG Write Error" ), wxOK | wxICON_INFORMATION );
            return;
        }
    }

    DataValuePairPtr dvp( new DataValuePair() );
    CommandPtr command( new Command() );
    std::string mode = ConvertUnicode( vesFileName.GetFullPath().c_str() );
    dvp->SetData( std::string( "Filename" ), mode );
    if( event.GetId() == EXPORTMENU_SCREEN_SHOT )
    {
        command->SetCommandName( std::string( "SCREEN_SHOT" ) );
    }
    else if( event.GetId() == EXPORTMENU_MOVIE_CAPTURE )
    {
        command->SetCommandName( std::string( "MOVIE_CAPTURE" ) );
    }
    else if( event.GetId() == EXPORTMENU_MOVIE_CAPTURE_OFF )
    {
        command->SetCommandName( std::string( "MOVIE_CAPTURE_OFF" ) );
    }
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->
    SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnDOTFile( wxCommandEvent& event )
{
    wxFileDialog dialog( NULL, _T( "Save OSG Graph File..." ),
                         ::wxGetCwd(),
                         _T( "osgGraph.dot" ),
                         _T( "DOT Graph (*.dot)|*.dot" ),
                         wxFD_SAVE | wxFD_OVERWRITE_PROMPT
                       );

    if( dialog.ShowModal() != wxID_OK )
    {
        return;
    }

    wxFileName vesFileName( dialog.GetPath() );
    vesFileName.ClearExt();
    vesFileName.SetExt( _( "dot" ) );
    bool success = vesFileName.MakeRelativeTo( ::wxGetCwd() );
    if( !success )
    {
        wxMessageBox( _( "Can't save the graph file on another drive." ),
                      _( "DOT Write Error" ), wxOK | wxICON_INFORMATION );
        return;
    }

    DataValuePairPtr dvp( new DataValuePair() );
    CommandPtr command( new Command() );
    std::string mode = ConvertUnicode( vesFileName.GetFullPath().c_str() );
    dvp->SetData( std::string( "Filename" ), mode );
    command->SetCommandName( std::string( "DOT_FILE" ) );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->
    SendCommandStringToXplorer( command );
}
