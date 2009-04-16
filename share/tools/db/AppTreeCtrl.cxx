
// --- VE-Suite Includes --- //
#include "AppTreeCtrl.h"
#include "DBAppEnums.h"


BEGIN_EVENT_TABLE( AppTreeCtrl, wxTreeCtrl )
/*
EVT_TREE_SEL_CHANGED( HIERARCHYTREE_CTRL, AppTreeCtrl::OnSelChanged )
EVT_TREE_ITEM_EXPANDING( HIERARCHYTREE_CTRL, AppTreeCtrl::OnExpanded )
//EVT_TREE_ITEM_ACTIVATED( HIERARCHYTREE_CTRL, AppTreeCtrl::OnDoubleClick )
EVT_TREE_ITEM_RIGHT_CLICK( HIERARCHYTREE_CTRL, AppTreeCtrl::OnRightClick )
EVT_MENU_RANGE( UIPLUGINBASE_BEGIN_MENU, UIPLUGINBASE_END_MENU, AppTreeCtrl::ProcessRightClickMenuEvents )
EVT_MENU_RANGE( PLUGIN_BEGIN_INDEX, PLUGIN_END_INDEX, AppTreeCtrl::ProcessRightClickMenuEvents )
*/
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl::AppTreeCtrl( wxWindow* parent )
    :
    wxTreeCtrl(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxTR_DEFAULT_STYLE | wxNO_BORDER | wxHSCROLL | wxVSCROLL )
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
}
////////////////////////////////////////////////////////////////////////////////
AppTreeCtrl::~AppTreeCtrl()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
/*
void AppTreeCtrl::OnSelChanged( wxTreeEvent& WXUNUSED( event ) )
{
    SelectNetworkPlugin( GetSelection() );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::OnExpanded( wxTreeEvent& WXUNUSED( event ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::OnRightClick( wxTreeEvent& event )
{
    //find selected item
    wxTreeItemId selected = event.GetItem();
    SelectItem( selected );
    ModuleData* tempModData = dynamic_cast< ModuleData* >( this->
        GetItemData( selected ));

    if( !tempModData )
    {
        return;
    }
    //activate correct network
    m_canvas->SetActiveNetwork( tempModData->systemId );

    //create popup menu
    wxMenu* popupMenu = m_canvas->GetActiveNetwork()->
        modules[tempModData->modId].GetPlugin()->GetPopupMenu();
    popupMenu->SetTitle( this->GetItemText( selected ) );
    m_canvas->GetActiveNetwork()->modules[tempModData->modId].
        GetPlugin()->SendActiveId();

    m_selection = selected;
    m_currentLevelId = GetItemParent( selected );
    PopupMenu( popupMenu );
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::OnDoubleClick( wxTreeEvent& event )
{
    //find selected item
    wxTreeItemId selected = event.GetItem();
    SelectItem( selected );
    
    if( selected != m_rootId )
    {
        ModuleData* tempModData = static_cast< ModuleData* >( this->
            GetItemData( selected ));
        m_canvas->SetActiveNetwork( tempModData->systemId );
        m_canvas->GetActiveNetwork()->modules[tempModData->modId].
            GetPlugin()->CreateUserDialog( wxPoint(0,0) );
        m_currentLevelId = GetItemParent( selected );
    }

    m_selection = selected;
}
////////////////////////////////////////////////////////////////////////////////
void AppTreeCtrl::ProcessRightClickMenuEvents( wxCommandEvent& event )
{
    ModuleData* tempModData = static_cast< ModuleData* >( this->
        GetItemData( m_selection ));

    ::wxPostEvent( m_canvas->GetActiveNetwork()->modules[tempModData->modId].
        GetPlugin(), event );
}
*/
///////////////////////////////////////////////////////////////////////////////
