
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
EVT_TREE_SEL_CHANGED( SELECTION_CHANGED_ATC, AppTreeCtrl::SelectionChanged )
//EVT_TREE_ITEM_ACTIVATED( DOUBLE_CLICK_ATC, AppTreeCtrl::DoubleClick )
//EVT_TREE_ITEM_RIGHT_CLICK( RIGHT_CLICK_ATC, AppTreeCtrl::RightClick )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl::AppTreeCtrl( wxWindow* parent )
    :
    wxTreeCtrl(
        parent,
        wxID_ANY,
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

    SetItemBold( dbLeaf );
    Expand( m_rootID );
    Expand( dbLeaf );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::SelectionChanged( wxTreeEvent& WXUNUSED( event ) )
{
    wxTreeItemId selectedId = GetSelection();
    if( selectedId == m_rootID || selectedId == m_selectionID )
    {
        return;
    }

    SetItemBold( selectedId );

    DBConnectionData* dbConnectionData =
        static_cast< DBConnectionData* >( GetItemData( selectedId ) );
    DBConnection* dbConnection = dbConnectionData->m_dbConnection;

    m_selectionID = selectedId;
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
