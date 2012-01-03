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
#include "AppFrame.h"
#include "AppMenuBar.h"
#include "AppToolBar.h"
#include "AppTreeCtrl.h"
#include "AppNotebook.h"
#include "DBConnectionDialog.h"
#include "VESConnectionDialog.h"
#include "CorbaUnitManager.h"
#include "DBAppEnums.h"

#include <ves/util/icons/ve_icon16x16.xpm>

// --- wxWidgets Includes --- //
#include <wx/icon.h>
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/scrolwin.h>
#include <wx/statline.h>

BEGIN_EVENT_TABLE( AppFrame, wxFrame )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppFrame::AppFrame( wxWindow* parent, wxWindowID id )
    :
    wxFrame(
        parent,
        id,
        wxT( "VE-DB" ),
        wxDefaultPosition,
        wxSize( 800, 600 ),
        wxDEFAULT_FRAME_STYLE ),
    m_appMenuBar( NULL ),
    m_appToolBar( NULL ),
    m_appTreeCtrl( NULL ),
    m_appNotebook( NULL ),
    m_dbConnectionDialog( new DBConnectionDialog( this ) ),
    m_vesConnectionDialog( new VESConnectionDialog( this ) ),
    m_corbaUnitManager( new CorbaUnitManager() )
{
    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppFrame::~AppFrame()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppFrame::CreateGUI()
{
    SetSizeHints( wxDefaultSize, wxDefaultSize );
	SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetIcon( ve_icon16x16_xpm );

    //Create the menu bar
    m_appMenuBar = new AppMenuBar( this );
    SetMenuBar( m_appMenuBar );

    //Create the tool bar
    m_appToolBar = new AppToolBar( this );
    SetToolBar( m_appToolBar );

    //Create the main sizer
    wxBoxSizer* mainSizer = new wxBoxSizer( wxHORIZONTAL );

    //Create a static line
    wxStaticLine* staticLine =
        new wxStaticLine(
            this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_VERTICAL );
	mainSizer->Add( staticLine, 0, wxLEFT | wxEXPAND, 5 );
	
    //Create the tree control
	m_appTreeCtrl = new AppTreeCtrl( this );
	mainSizer->Add( m_appTreeCtrl, 1, wxTOP | wxEXPAND, 5 );
	
    //Create the notebook
	m_appNotebook = new AppNotebook( this );
	mainSizer->Add( m_appNotebook, 2, wxEXPAND, 5 );
	
	SetSizer( mainSizer );
	Layout();
}
////////////////////////////////////////////////////////////////////////////////
AppMenuBar* const AppFrame::GetAppMenuBar() const
{
    return m_appMenuBar;
}
////////////////////////////////////////////////////////////////////////////////
AppToolBar* const AppFrame::GetAppToolBar() const
{
    return m_appToolBar;
}
////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl* const AppFrame::GetAppTreeCtrl() const
{
    return m_appTreeCtrl;
}
////////////////////////////////////////////////////////////////////////////////
AppNotebook* const AppFrame::GetAppNotebook() const
{
    return m_appNotebook;
}
////////////////////////////////////////////////////////////////////////////////
DBConnectionDialog* const AppFrame::GetDBConnectionDialog() const
{
    return m_dbConnectionDialog;
}
////////////////////////////////////////////////////////////////////////////////
VESConnectionDialog* const AppFrame::GetVESConnectionDialog() const
{
    return m_vesConnectionDialog;
}
////////////////////////////////////////////////////////////////////////////////
CorbaUnitManager* const AppFrame::GetCorbaUnitManager() const
{
    return m_corbaUnitManager;
}
////////////////////////////////////////////////////////////////////////////////
