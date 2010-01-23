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
#include <ves/conductor/util/CORBAServiceList.h>
#include "AvailableModules.h"
#include "AppFrame.h"
#include "HierarchyTree.h"
#include "PluginLoader.h"
#include "ConductorAppEnums.h"

#include <ves/conductor/Canvas.h>
#include <ves/conductor/Network.h>
#include <ves/conductor/UIPluginBase.h>
#include <ves/conductor/DefaultPlugin/DefaultPlugin.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

#include <ves/conductor/xpm/icon1.xpm>
#include <ves/conductor/xpm/icon2.xpm>
#include <ves/conductor/xpm/icon3.xpm>
#include <ves/conductor/xpm/icon4.xpm>
#include <ves/conductor/xpm/icon5.xpm>

#include <wx/intl.h>
#include <wx/tokenzr.h>

using namespace ves::conductor;
using namespace ves::conductor::util;
using namespace ves::open::xml;

BEGIN_EVENT_TABLE( AvailableModules, wxTreeCtrl )
    EVT_TREE_ITEM_RIGHT_CLICK( AVAILABLEMODULES_TREE_CTRL, AvailableModules::OnItemRightClick )
    EVT_TREE_SEL_CHANGED( AVAILABLEMODULES_TREE_CTRL, AvailableModules::OnSelChanged )
    EVT_MENU( AVAILABLEMODULES_DESC, AvailableModules::ShowDesc )
    EVT_MENU( AVAILABLEMODULES_HELP, AvailableModules::ShowHelp )
    EVT_TREE_ITEM_ACTIVATED( AVAILABLEMODULES_TREE_CTRL, AvailableModules::Instantiate )
END_EVENT_TABLE()

AvailableModules::AvailableModules( wxWindow *parent, const wxWindowID id, 
                             const wxPoint& pos, const wxSize& size,
                             long style )
    :
    wxTreeCtrl( parent, id, pos, size, style ),
    canvas( 0 )
{
    int image1 = AVAILABLEMODULES_FOLDER;
    //int image2 = AVAILABLEMODULES_FOLDERSELECTED;

    CreateImageList();
    
    rootId = AddRoot( wxT( "Available Plugins" ), image1, -1, NULL );
    SetItemImage( rootId, AVAILABLEMODULES_FOLDEROPENED, wxTreeItemIcon_Expanded );
    SetItemFont( rootId, *wxNORMAL_FONT );
    
    pl_loader = new PluginLoader();
    
    LoadModules();
}
////////////////////////////////////////////////////////////////////////////////
AvailableModules::~AvailableModules()
{
    delete pl_loader;
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::AddModule( UIPluginBase* plugin, wxClassInfo* clsi )
{
    std::vector<wxString> lnames;
    wxTreeItemIdValue cookie;
    wxString plname = plugin->GetConductorName();
    wxTreeItemId id, lastid;
    int i, lsize;

    getLeveledName( plname, lnames );

    int image1, image2, image3, image4;
    image1 = AVAILABLEMODULES_FOLDER;
    image2 = AVAILABLEMODULES_FOLDERSELECTED;
    image3 = AVAILABLEMODULES_FILE;
    image4 = AVAILABLEMODULES_FILESELECTED;

    id = rootId;
    lsize = lnames.size();

    //This parses the _ in the plugin name so that the proper folder hiearachy
    //can be created
    for( i = 0; i < ( lsize - 1 ); i++ )
    {
        lastid = id;
        id = GetFirstChild( id, cookie );

        while( 1 )
        {
            if( !id )
            {
                id = AppendItem( lastid, lnames[i], image1, image2, NULL );
                SetItemImage( id, AVAILABLEMODULES_FOLDEROPENED,
                    wxTreeItemIcon_Expanded );
                SetItemFont( id, *wxNORMAL_FONT );
                break;
            }

            if( GetItemText( id ) == lnames[i] )
                break;

            id = GetNextChild( lastid, cookie );
        }
    }

    //add item to tree
    images->Add( wxBitmap( plugin->GetIconImage()->ConvertToImage().
        Rescale( m_imageSize, m_imageSize ) ) );
    id = AppendItem( id, lnames[i], images->GetImageCount() - 1, -1,
        new ReiTreeItemData( plugin, clsi ) );
    SetItemFont( id, *wxNORMAL_FONT );
    //SetItemBold( id );
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::OnItemRightClick( wxTreeEvent& event )
{
    ReiTreeItemData* item_data;

    selection = event.GetItem();
    SelectItem( selection );

    if( !selection )
    {
        return;
    }

    item_data = dynamic_cast< ReiTreeItemData* >( GetItemData( selection ) );
    if( item_data == NULL )
    {
        return;
    }

    ShowMenu( selection, event.GetPoint() );
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::Instantiate( wxTreeEvent& WXUNUSED( event ) ) //Double click
{
    selection = GetSelection();
    if( !selection )
    {
        return;
    }

    ReiTreeItemData* item_data;
    item_data = dynamic_cast< ReiTreeItemData* >( GetItemData( selection ) );
    if( item_data == NULL )
    {
        return;
    }

    //add new plugin to current network and insert entry in hierarchy tree
    wxClassInfo* info = item_data->pl_clsi;
    if( info )
    {
        UIPluginBase* object =
#if wxCHECK_VERSION( 2, 9, 0 )
            dynamic_cast< UIPluginBase* >( (info->GetConstructor())() );
#else
            dynamic_cast< UIPluginBase* >( info->m_objectConstructor() );
#endif
        object->SetCanvas( canvas );
        object->SetNetwork( canvas->GetActiveNetwork() );
        object->SetDCScale( canvas->GetActiveNetwork()->GetUserScale() );
        canvas->GetActiveNetwork()->AddtoNetwork( object,
            std::string( wxString( info->GetClassName() ).mb_str() ) );
        frame->GetHierarchyTree()->AddtoTree( object );
    }
    else
    {
        UIPluginBase* object = new DefaultPlugin();
        object->SetCanvas( canvas );
        object->SetNetwork( canvas->GetActiveNetwork() );
        object->SetDCScale( canvas->GetActiveNetwork()->GetUserScale() );
        canvas->GetActiveNetwork()->AddtoNetwork( object,
            std::string( "DefaultPlugin" ) );
        frame->GetHierarchyTree()->AddtoTree( object );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::ShowMenu( wxTreeItemId id, const wxPoint& pt )
{
    wxString title;
    if( id.IsOk() )
    {
        title << wxT( "Menu for " ) << GetItemText( id );
    }
    else
    {
        title = wxT( "Menu for no particular item" );
    }

    wxMenu menu( title );
    menu.Append( AVAILABLEMODULES_DESC, wxT( "&Description" ) );
    menu.Append( AVAILABLEMODULES_HELP, wxT( "&Help" ) );

    PopupMenu( &menu, pt );
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::getLeveledName( wxString name,
    std::vector<wxString> & lnames )
{
    wxStringTokenizer tkz( name, wxT("_"));
    while( tkz.HasMoreTokens() )
    {
        wxString token = tkz.GetNextToken();
        lnames.push_back( token );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::CreateImageList( int size )
{
    if( size == -1 )
    {
        SetImageList( NULL );
        return;
    }
    if( size == 0 )
        size = m_imageSize;
    else
        m_imageSize = size;

    // Make an image list containing small icons
    images = new wxImageList( size, size, TRUE );

    // should correspond to TreeCtrlIcon_xxx enum
    wxBusyCursor wait;
    wxIcon icons[5];
    icons[0] = wxIcon( icon1_xpm );
    icons[1] = wxIcon( icon2_xpm );
    icons[2] = wxIcon( icon3_xpm );
    icons[3] = wxIcon( icon4_xpm );
    icons[4] = wxIcon( icon5_xpm );

    //add images to imagelist
    int sizeOrig = icons[0].GetWidth();
    for( size_t i = 0; i < WXSIZEOF( icons ); i++ )
    {
        if( size == sizeOrig )
        {
            images->Add( icons[i] );
        }
        else
        {
            images->Add( wxBitmap( wxBitmap( icons[i] ).ConvertToImage().
                Rescale( size, size ) ) );
        }
    }

    AssignImageList( images );
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::OnSelChanged( wxTreeEvent& WXUNUSED( event ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool AvailableModules::LoadModules()
{
    pl_loader->LoadPlugins( _( "Plugins/UI" ) );
    for( size_t i = 0; i < pl_loader->GetNumberOfPlugins(); i++ )
    {
        UIPluginBase* plugin = pl_loader->GetPluginDataPair( i ).first;
        wxClassInfo* clsi = pl_loader->GetPluginDataPair( i ).second;
        
        AddModule( plugin, clsi );
    }
    ExpandAll();
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::ShowDesc( wxCommandEvent& WXUNUSED( event ) )
{
    ReiTreeItemData* item_data;
    UIPluginBase* pl;
    wxString desc;
    wxString title;

    title << wxT( "Description for " ) << GetItemText( selection );

    if( !selection )
    {
        return;
    }

    item_data = dynamic_cast< ReiTreeItemData* >( GetItemData( selection ) );
    if( item_data == NULL )
    {
        return;
    }

    pl = item_data->plugin;
    desc = pl->GetDesc();

    wxMessageDialog( this, desc, title ).ShowModal();
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::ShowHelp( wxCommandEvent& WXUNUSED( event ) )
{
    UIPluginBase* pl;
    wxString help;
    wxString fgroot;
    wxString docdir;
    wxString cmd;
    ReiTreeItemData* item_data;
    item_data = dynamic_cast< ReiTreeItemData* >( GetItemData( selection ) );
    if( !item_data )
    {
        return;
    }

    pl = item_data->plugin;
}
////////////////////////////////////////////////////////////////////////////////
void AvailableModules::ResetPluginTree()
{
    //Destroy the old tree
    DeleteAllItems();
    int image1 = AVAILABLEMODULES_FOLDER;
    int image2 = AVAILABLEMODULES_FOLDERSELECTED;
    rootId = AddRoot( wxT( "Available Plugins" ), image1, image2, NULL );
    SetItemImage( rootId, AVAILABLEMODULES_FOLDEROPENED, wxTreeItemIcon_Expanded );
    SetItemFont( rootId, *wxNORMAL_FONT );

    //Remove all the old plugins and create the new one
    delete pl_loader;
    pl_loader = new PluginLoader();
    
    //Load the plugins now
    LoadModules();

    //Now tell Xplorer to reload plugins
    DataValuePairPtr dvp( new DataValuePair( std::string( "STRING" ) ) );
    dvp->SetData( "Reload_Plugin_Objects", "Reload" );
    ves::open::xml::CommandPtr vec( new ves::open::xml::Command() );
    vec->SetCommandName( "Plugin_Control" );
    vec->AddDataValuePair( dvp );
    CORBAServiceList::instance()->SendCommandStringToXplorer( vec );
}
