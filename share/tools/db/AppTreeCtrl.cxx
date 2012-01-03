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
#include "AppTreeCtrl.h"
#include "AppFrame.h"
#include "AppNotebook.h"
#include "DBAppEnums.h"
#include "DBConnection.h"

#include "xpm/TreeCtrl/Database.xpm"
#include "xpm/TreeCtrl/MSAccessDatabase.xpm"
#include "xpm/TreeCtrl/MySQLDatabase.xpm"
#include "xpm/TreeCtrl/Table.xpm"

// --- wxWidgets Includes --- //
#include <wx/imaglist.h>

BEGIN_EVENT_TABLE( AppTreeCtrl, wxTreeCtrl )
EVT_TREE_SEL_CHANGED( APP_TREE_CTRL, AppTreeCtrl::SelectionChanged )
//EVT_TREE_ITEM_ACTIVATED( APP_TREE_CTRL, AppTreeCtrl::DoubleClick )
//EVT_TREE_ITEM_RIGHT_CLICK( APP_TREE_CTRL, AppTreeCtrl::RightClick )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl::AppTreeCtrl( wxWindow* parent )
    :
    wxTreeCtrl(
        parent,
        APP_TREE_CTRL,
        wxDefaultPosition,
        wxDefaultSize,
        wxTR_DEFAULT_STYLE | wxNO_BORDER ),
    m_appFrame( static_cast< AppFrame* >( parent ) )
{
    LoadBitmaps();

    CreateGUI();
}
////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl::~AppTreeCtrl()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::LoadBitmaps()
{
    ///The list of images
    wxImageList* imageList = new wxImageList( 16, 16 );
    imageList->Add( wxBitmap( Database_xpm ) );
    imageList->Add( wxBitmap( MSAccessDatabase_xpm ) );
    imageList->Add( wxBitmap( MySQLDatabase_xpm ) );
    imageList->Add( wxBitmap( Table_xpm ) );

    AssignImageList( imageList );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );

    //Add the root
    m_rootID = AddRoot( wxT( "Database Connections" ), DATABASE );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::AddDBConnection( DBConnection* dbConnection )
{
    DBConnectionData* dbConnectionData = new DBConnectionData();
    dbConnectionData->m_dbConnection = dbConnection;

    wxTreeItemId dbLeaf = AppendItem(
        m_rootID, 
        wxString( dbConnection->GetName().c_str(), wxConvUTF8 ), 
        dbConnection->GetDBType(), -1,
        dbConnectionData );

    const StringVector1D& tableNames =
        dbConnection->GetTableNames();
    for( size_t i = 0; i < tableNames.size(); ++i )
    {
        DBTableData* dbTableData = new DBTableData();
        dbTableData->m_dbTableName = tableNames.at( i );
        AppendItem(
            dbLeaf, 
            wxString( dbTableData->m_dbTableName.c_str(), wxConvUTF8 ), 
            TABLE, -1,
            dbTableData );
    }

    SelectItem( dbLeaf );
    Expand( m_rootID );
    Expand( dbLeaf );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::SelectionChanged( wxTreeEvent& WXUNUSED( event ) )
{
    wxTreeItemId selectedID = GetSelection();
    if( selectedID == m_selectionID )
    {
        return;
    }

    SetItemBold( selectedID );
    SetItemBold( m_selectionID, false );
    m_selectionID = selectedID;

    if( selectedID == m_rootID || GetItemParent( selectedID ) == m_rootID )
    {
        AppNotebook* appNotebook = m_appFrame->GetAppNotebook();
        appNotebook->ClearTableDetails();
        appNotebook->ClearTableData();

        return;
    }

    DBConnectionData* dbConnectionData =
        static_cast< DBConnectionData* >(
            GetItemData( GetItemParent( selectedID ) ) );
    DBConnection* dbConnection = dbConnectionData->m_dbConnection;

    DBTableData* dbTableData =
        static_cast< DBTableData* >( GetItemData( selectedID ) );
    std::string& dbTableName = dbTableData->m_dbTableName;

    const StringVector2D* tableDetails =
        dbConnection->GetTableDetails( dbTableName );
    //const StringVector1D* tableFieldNames =
        //dbConnection->GetTableFieldNames( dbTableName );
    //const StringVector2D* tableData =
        //dbConnection->GetTableData( dbTableName );

    AppNotebook* appNotebook = m_appFrame->GetAppNotebook();
    appNotebook->PopulateTableDetails( tableDetails );
    //appNotebook->PopulateTableData( tableFieldNames, tableData );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::RightClick( wxTreeEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::DoubleClick( wxTreeEvent& event )
{
    ;
}
///////////////////////////////////////////////////////////////////////////////
