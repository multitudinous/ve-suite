
// --- VE-Suite Includes --- //
#include "AppTreeCtrl.h"
#include "AppFrame.h"
#include "DBAppEnums.h"

#include "xpm/TreeCtrl/MySQLDatabase.xpm"
#include "xpm/TreeCtrl/AccessDatabase.xpm"

// --- wxWidgets Includes --- //
#include <wx/imaglist.h>


BEGIN_EVENT_TABLE( AppTreeCtrl, wxTreeCtrl )
EVT_TREE_SEL_CHANGED( SELECTION_CHANGED_ATC, AppTreeCtrl::SelectionChanged )
//EVT_TREE_ITEM_EXPANDING( HIERARCHYTREE_CTRL, AppTreeCtrl::Expand )
//EVT_TREE_ITEM_ACTIVATED( HIERARCHYTREE_CTRL, AppTreeCtrl::DoubleClick )
//EVT_TREE_ITEM_RIGHT_CLICK( HIERARCHYTREE_CTRL, AppTreeCtrl::RightClick )
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
    wxImageList* imageList = new wxImageList( 32, 32 );
    imageList->Add( wxBitmap( MySQLDatabase_xpm ) );
    imageList->Add( wxBitmap( AccessDatabase_xpm ) );

    AssignImageList( imageList );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::CreateGUI()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );

    //Add the root
    AddRoot( wxT( "Database Connections" ), 0 );
    //SetItemImage( rootId, AVAILABLEMODULES_FOLDEROPENED, wxTreeItemIcon_Expanded );
    //SetItemFont( rootId, *wxNORMAL_FONT );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::SelectionChanged( wxTreeEvent& WXUNUSED( event ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::Expand( wxTreeEvent& WXUNUSED( event ) )
{
    ;
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
const DBConnection* const ConnectionData::GetDBConnection() const
{
    return m_dbConnection;
}
///////////////////////////////////////////////////////////////////////////////
