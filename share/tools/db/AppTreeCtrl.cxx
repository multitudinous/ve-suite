
// --- VE-Suite Includes --- //
#include "AppTreeCtrl.h"
#include "AppFrame.h"
#include "DBConnection.h"
#include "DBAppEnums.h"

#include "xpm/TreeCtrl/Database.xpm"
#include "xpm/TreeCtrl/AccessDatabase.xpm"
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
        wxTR_DEFAULT_STYLE | wxHSCROLL | wxNO_BORDER | wxVSCROLL ),
    m_appFrame( static_cast< AppFrame* >( parent->GetParent() ) )
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
    imageList->Add( wxBitmap( AccessDatabase_xpm ) );
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

    SelectItem( m_rootID );
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

    const std::vector< std::string >& tableNames =
        dbConnection->GetTableNames();
    for( size_t i = 0; i < tableNames.size(); ++i )
    {
        AppendItem(
            dbLeaf, 
            wxString( tableNames.at( i ).c_str(), wxConvUTF8 ), 
            TABLE, -1,
            dbConnectionData );
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
        return;
    }

    DBConnectionData* dbConnectionData =
        static_cast< DBConnectionData* >( GetItemData( selectedID ) );
    DBConnection* dbConnection = dbConnectionData->m_dbConnection;
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
