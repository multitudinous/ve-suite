/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#include "AppToolBar.h"
#include "AppFrame.h"
#include "DBConnectionDialog.h"
#include "VESConnectionDialog.h"
#include "DBAppEnums.h"

#include "xpm/ToolBar/DBConnection.xpm"
#include "xpm/ToolBar/VESConnection.xpm"
#include "xpm/ToolBar/VESConnectionDisabled.xpm"

BEGIN_EVENT_TABLE( AppToolBar, wxToolBar )
EVT_MENU( SHOW_DB_CONNECTION_DIALOG_ATB, AppToolBar::ShowDBConnectionDialog )
EVT_MENU( SHOW_VES_CONNECTION_DIALOG_ATB, AppToolBar::ShowVESConnectionDialog )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppToolBar::AppToolBar( wxWindow* parent )
    :
    wxToolBar(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxTB_FLAT | wxTB_NODIVIDER | wxTB_VERTICAL | wxNO_BORDER,
        wxT( "ToolBar" ) ),
    m_appFrame( static_cast< AppFrame* >( parent ) )
{
    LoadBitmaps();

    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppToolBar::~AppToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::LoadBitmaps()
{
    mToolbarBitmaps[ "dbConnectionBitmap" ] =
        wxBitmap( DBConnection_xpm );

    mToolbarBitmaps[ "vesConnectionBitmap" ] =
        wxBitmap( VESConnection_xpm );

    mToolbarBitmaps[ "vesConnectionDisabledBitmap" ] =
        wxBitmap( VESConnectionDisabled_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool(
        SHOW_DB_CONNECTION_DIALOG_ATB, wxT( "" ),
        mToolbarBitmaps[ "dbConnectionBitmap" ],
        wxT( "DB Connection" ), wxITEM_NORMAL );

#ifdef WIN32
    AddTool(
        SHOW_VES_CONNECTION_DIALOG_ATB, wxT( "" ),
        mToolbarBitmaps[ "vesConnectionBitmap" ],
        mToolbarBitmaps[ "vesConnectionDisabledBitmap" ], wxITEM_NORMAL,
        wxT( "VES Connection" ) );
#else
    AddTool(
        SHOW_VES_CONNECTION_DIALOG_ATB, wxT( "" ),
        mToolbarBitmaps[ "vesConnectionBitmap" ],
        wxT( "VES Connection" ), wxITEM_NORMAL );
#endif

	Realize();

    EnableTool( SHOW_DB_CONNECTION_DIALOG_ATB, true );
    EnableTool( SHOW_VES_CONNECTION_DIALOG_ATB, true );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::DisableVESConnectionDialog()
{
    /*
#ifndef WIN32
    SetToolNormalBitmap(
        SHOW_VES_CONNECTION_DIALOG_ATB,
        mToolbarBitmaps[ "vesConnectionDisabledBitmap" ] );
#endif
    */

    EnableTool( SHOW_VES_CONNECTION_DIALOG_ATB, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::ShowDBConnectionDialog( wxCommandEvent& WXUNUSED( event ) )
{
    m_appFrame->GetDBConnectionDialog()->Show();
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::ShowVESConnectionDialog( wxCommandEvent& WXUNUSED( event ) )
{
    m_appFrame->GetVESConnectionDialog()->Show();
}
////////////////////////////////////////////////////////////////////////////////
