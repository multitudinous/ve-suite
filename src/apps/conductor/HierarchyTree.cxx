/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#include "HierarchyTree.h"

#include <ves/conductor/Network.h>
#include <ves/conductor/Canvas.h>
#include <ves/conductor/XMLDataBufferEngine.h>
#include <ves/conductor/AspenPlus2DIcons.h>
#include <ves/conductor/UIPluginBase.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/System.h>

#include <ves/conductor/xpm/icon1.xpm>
#include <ves/conductor/xpm/icon2.xpm>
#include <ves/conductor/xpm/icon3.xpm>
#include <ves/conductor/xpm/icon4.xpm>
#include <ves/conductor/xpm/icon5.xpm>
#include <ves/conductor/xpm/contour.xpm>
#include <ves/conductor/xpm/cad_tree_selected.xpm>
#include <ves/conductor/xpm/cad_tree_unselected.xpm>
#include <ves/conductor/xpm/cspline.xpm>
#include <ves/conductor/xpm/isosurface.xpm>
#include <ves/conductor/xpm/ROItb.xpm>
#include <ves/conductor/xpm/square.xpm>
#include <ves/conductor/xpm/streamlines.xpm>
#include <ves/conductor/xpm/vector.xpm>
#include <ves/conductor/xpm/vectortb.xpm>

#ifdef WIN32
#include <shellapi.h>
#endif

using namespace ves::open::xml;
using namespace ves::conductor::util;
using namespace ves::conductor;

BEGIN_EVENT_TABLE( HierarchyTree, wxTreeCtrl )
    EVT_TREE_SEL_CHANGED( TREE_CTRL, HierarchyTree::OnSelChanged )
    EVT_TREE_ITEM_EXPANDING( TREE_CTRL, HierarchyTree::OnExpanded )
    EVT_TREE_ITEM_ACTIVATED( TREE_CTRL, HierarchyTree::OnDoubleClick )
    EVT_TREE_ITEM_RIGHT_CLICK( TREE_CTRL, HierarchyTree::OnRightClick )
    //EVT_MENU_RANGE( UIPluginBase::BEGIN_MENU_ID, UIPluginBase::END_MENU_ID,
    //    HierarchyTree::ProcessRightClickMenuEvents )
END_EVENT_TABLE()

HierarchyTree::HierarchyTree( wxWindow *parent, const wxWindowID id,
                             const wxPoint& pos, const wxSize& size,
                             long style )
    :
    wxTreeCtrl( parent, id, pos, size, style ),
    m_canvas( 0 )
{
    //create an imagelist
    iconsize = 16;
    images = new wxImageList( iconsize, iconsize, TRUE );
    AssignImageList( images );

    //add default images
    wxIcon icons[5];
    icons[0] = wxIcon( icon1_xpm );
    icons[1] = wxIcon( icon2_xpm );
    icons[2] = wxIcon( icon3_xpm );
    icons[3] = wxIcon( icon4_xpm );
    icons[4] = wxIcon( icon5_xpm );
    AddtoImageList( wxBitmap( wxBitmap( icons[0] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[1] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[2] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[3] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[4] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );

    //setup root item
    m_rootId = AddRoot( wxT( "Top Sheet" ), 2, -1, NULL );
    SetItemImage( m_rootId, 4, wxTreeItemIcon_Expanded );

    //default selection to root item
    m_currentLevelId = m_rootId;
    m_selection = m_rootId;

    SetItemFont( m_rootId, *wxNORMAL_FONT );

    //default look up
    defaultIconMap[ "contour.xpm" ] = wxImage( contour_xpm );
    defaultIconMap[ "isosurface.xpm" ] = wxImage( isosurface_xpm );
    defaultIconMap[ "ROItb.xpm" ] = wxImage( ROItb_xpm );
    defaultIconMap[ "streamlines.xpm" ] = wxImage( streamlines_xpm );
    defaultIconMap[ "vector.xpm" ] = wxImage( vector_xpm );
    defaultIconMap[ "vectortb.xpm" ] = wxImage( vectortb_xpm );
}
////////////////////////////////////////////////////////////////////////////////
HierarchyTree::~HierarchyTree()
{
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AddtoImageList( wxBitmap icon )
{
    images->Add( icon );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateTree( )
{
    //Reset Tree
    Clear();

    //get the list of models
    std::string id = XMLDataBufferEngine::instance()->GetTopSystemId();
    ves::open::xml::model::SystemPtr tempSys = 
        XMLDataBufferEngine::instance()->GetXMLSystemDataObject( id );
    std::vector< ves::open::xml::model::ModelPtr > topLevelModels = 
        tempSys->GetModels();
    
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;
    std::multimap< std::string, ves::open::xml::model::ModelPtr > alphaTree;

    //alphabetize tree
    for( std::vector< ves::open::xml::model::ModelPtr >::iterator
        iter = topLevelModels.begin(); iter != topLevelModels.end(); ++iter )
    {
        ves::open::xml::model::ModelPtr tempModel = (*iter);
        //alphaTree[ tempModel->GetModelName() ] = tempModel;
        alphaTree.insert(
            std::pair< std::string, ves::open::xml::model::ModelPtr >
            ( tempModel->GetModelName(), tempModel ));
    }

    //loop over models and add them to the tree
    for( std::multimap< std::string, ves::open::xml::model::ModelPtr >::iterator
        iter = alphaTree.begin(); iter != alphaTree.end(); ++iter )
    {
        if( iter->second->GetIconHiddenFlag() == 0 )
        {
            //generate module data
            ModuleData* modData = new ModuleData();
            modData->modId = iter->second->GetModelID();
            modData->modName = iter->second->GetModelName();
            modData->systemId = id;
            if( iter->second->GetSubSystem() )
            {
                modData->subSystemId = iter->second->GetSubSystem()->GetID();
            }
            else
            {
                modData->subSystemId = -1;
            }

            //Add the icon to the image list
            fullPath = iter->second->GetIconFilename() + ".xpm";
            aspenIconIter = aspenPlusIconMap.find( fullPath );
            if( aspenIconIter != aspenPlusIconMap.end() )
            {
                AddtoImageList( wxBitmap( wxBitmap( aspenIconIter->second ).
                    ConvertToImage().Rescale( iconsize, iconsize ) ) );
            }
            else
            {
                wxIcon square ( square_xpm );
                AddtoImageList( wxBitmap( wxBitmap( square ).ConvertToImage().
                    Rescale( iconsize, iconsize ) ) );
            }

            //Add the new model to the tree
            wxTreeItemId leaf = AppendItem( m_rootId, 
                wxString( iter->second->GetModelName().c_str(), wxConvUTF8 ),
                images->GetImageCount() - 1 , -1, modData );
            SetItemFont( leaf, *wxNORMAL_FONT );
            //SetItemBold( leaf );

            //if there are subsystems parse those
            if( iter->second->GetSubSystem() )
            {
                PopulateLevel( leaf, iter->second->GetSubSystem()->GetModels(),
                    iter->second->GetSubSystem()->GetID() );
            }
        }
    }

    //set the current selection to the root item
    m_currentLevelId = m_rootId;
    m_selection = m_rootId;
}

////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateLevel( wxTreeItemId parentLeaf,
    std::vector< ves::open::xml::model::ModelPtr > models, std::string id )
{
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;

    //loop over subsystem models
    for( size_t i = 0; i < models.size(); ++i )
    {
        //is the model hidden
        if( models[i]->GetIconHiddenFlag() == 0 )
        {
            //generate module data
            ModuleData* modData = new ModuleData();
            modData->modId = models[i]->GetModelID();
            modData->modName = models[i]->GetModelName();
            modData->systemId = id;
            if( models[i]->GetSubSystem() )
            {
                modData->subSystemId = models[i]->GetSubSystem()->GetID();
            }
            else
            {
                modData->subSystemId = -1;
            }

            //Add the icon to the image list
            fullPath = models[i]->GetIconFilename() + ".xpm";
            aspenIconIter = aspenPlusIconMap.find( fullPath );
            if( aspenIconIter != aspenPlusIconMap.end() )
            {
                AddtoImageList( wxBitmap( wxBitmap( aspenIconIter->second ).
                    ConvertToImage().Rescale( iconsize, iconsize ) ) );
            }
            else
            {
                wxIcon square ( square_xpm );
                AddtoImageList( wxBitmap( wxBitmap( square ).ConvertToImage().
                    Rescale( iconsize, iconsize ) ) );
            }

            //add the new item to the list
            wxTreeItemId leaf = AppendItem( parentLeaf,
                wxString( models[i]->GetModelName().c_str(), wxConvUTF8 ),
                images->GetImageCount() - 1 , -1, modData );
            //SetItemBold( leaf );

            if( models[i]->GetSubSystem() )
            {
                PopulateLevel( leaf, models[i]->GetSubSystem()->GetModels(),
                               models[i]->GetSubSystem()->GetID() );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::Clear()
{
    DeleteAllItems();
    images->RemoveAll();
    
    wxIcon icons[5];
    icons[0] = wxIcon( icon1_xpm );
    icons[1] = wxIcon( icon2_xpm );
    icons[2] = wxIcon( icon3_xpm );
    icons[3] = wxIcon( icon4_xpm );
    icons[4] = wxIcon( icon5_xpm );
    AddtoImageList( wxBitmap( wxBitmap( icons[0] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[1] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[2] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[3] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    AddtoImageList( wxBitmap( wxBitmap( icons[4] ).ConvertToImage().
        Rescale( iconsize, iconsize ) ) );
    
    m_rootId = AddRoot( wxT( "Top Sheet" ), 2, -1, NULL );
    SetItemImage( m_rootId, 4, wxTreeItemIcon_Expanded );

    m_currentLevelId = m_rootId;
    m_selection = m_rootId;
    
    SetItemFont( m_rootId, *wxNORMAL_FONT );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnSelChanged( wxTreeEvent& WXUNUSED( event ) )
{
    SelectNetworkPlugin( GetSelection() );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::SelectNetworkPlugin( wxTreeItemId selectedId )
{
    //return if current selection is top folder
    //or the clicking the same node
    if( selectedId == m_rootId || selectedId == m_selection )
    {
        return;
    }

    //find and highlight selected module
    ModuleData* tempModData =
        static_cast< ModuleData* >(this->GetItemData( selectedId ) );
    m_canvas->SetActiveNetwork( tempModData->systemId );
    m_canvas->GetActiveNetwork()->HighlightCenter( tempModData->modId );

    //if a subnet redraw in conductor and xplorer
    m_currentLevelId = GetItemParent( m_selection );
    if( GetItemParent( selectedId ) != m_currentLevelId )
    {
        //tell xplorer to draw subnet
        CommandPtr veCommand( new ves::open::xml::Command() );
        veCommand->SetCommandName( std::string( "CHANGE_XPLORER_VIEW" ) );
        DataValuePairPtr dataValuePair2( 
            new DataValuePair( std::string( "UNSIGNED INT" ) ) );
        dataValuePair2->SetData( "SUBNET_ID", tempModData->systemId );
        veCommand->AddDataValuePair( dataValuePair2 );
        DataValuePairPtr dataValuePair( 
            new DataValuePair( std::string( "STRING" ) ) );
        dataValuePair->SetData( "UPDATE_XPLORER_VIEW",
            "CHANGE_XPLORER_VIEW_NETWORK" );
        veCommand->AddDataValuePair( dataValuePair );
        CORBAServiceList::instance()->SendCommandStringToXplorer( veCommand );
    }

    //keep track of previous selection
    m_selection = selectedId;
    m_currentLevelId = GetItemParent( m_selection );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnExpanded( wxTreeEvent& WXUNUSED( event ) )
{
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnRightClick( wxTreeEvent& event )
{
    //find selected item
    wxTreeItemId selected = event.GetItem();
    SelectItem( selected );
    ModuleData* tempModData = static_cast< ModuleData* >( this->
        GetItemData( selected ));

    //activate correct network
    m_canvas->SetActiveNetwork( tempModData->systemId );

    //create popup menu
    /*wxMenu* popupMenu = m_canvas->GetActiveNetwork()->
        modules[tempModData->modId].GetPlugin()->GetPopupMenu();
    popupMenu->SetTitle( this->GetItemText( selected ) );
    m_canvas->GetActiveNetwork()->modules[tempModData->modId].
        GetPlugin()->SendActiveId();

    m_selection = selected;
    m_currentLevelId = GetItemParent( selected );
    PopupMenu( popupMenu );*/
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnDoubleClick( wxTreeEvent& event )
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
void HierarchyTree::ProcessRightClickMenuEvents( wxCommandEvent& event )
{
    ModuleData* tempModData = static_cast< ModuleData* >( this->
        GetItemData( m_selection ));

    ::wxPostEvent( m_canvas->GetActiveNetwork()->modules[tempModData->modId].
        GetPlugin(), event );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AddtoTree( UIPluginBase* cur_module )
{
    ModuleData* modData = new ModuleData();
    modData->modId = cur_module->GetID();
    modData->modName = ConvertUnicode( cur_module->GetName() );
    modData->systemId = m_canvas->GetActiveNetworkID( );

    AddtoImageList( wxBitmap( cur_module->GetIconImage()->ConvertToImage().
        Rescale( iconsize, iconsize ) ) );

    wxTreeItemId leaf = AppendItem( m_currentLevelId, 
        wxString( cur_module->GetName().c_str(), wxConvUTF8 ), 
                images->GetImageCount() - 1, -1, modData );

    //SetItemBold( leaf );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::RemoveFromTree( unsigned int id )
{
    wxTreeItemId root = GetRootItem();
    wxTreeItemId selected = SearchTree( root, id );

    if( selected.IsOk() )
    {
        Delete( selected );
    }
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AppendToTree( unsigned int parentID, unsigned int id )
{
    wxTreeItemId root = GetRootItem();
    wxTreeItemId selected = SearchTree( root, parentID );

    if( selected.IsOk() )
    {
        ModuleData* modData = new ModuleData();

        modData->modId = id;
        modData->modName = "DefaultPlugin";
        modData->systemId = m_canvas->GetActiveNetworkID( );
        
        wxIcon square ( square_xpm );
        AddtoImageList( wxBitmap( wxBitmap( square ).ConvertToImage().
            Rescale( iconsize, iconsize ) ) );
        wxTreeItemId leaf = AppendItem( selected, _( "DefaultPlugin" ),
            images->GetImageCount() - 1 , -1, modData );

        SelectItem( leaf );
        //SetItemBold( leaf );
        //Refresh();
    }
}

///////////////////////////////////////////////////////////////////////////////
void HierarchyTree::SetTreeItemName( unsigned int id, wxString name )
{
    wxTreeItemId root = GetRootItem();
    wxTreeItemId selected = SearchTree( root, id );

    if( selected.IsOk() )
    {
        SetItemText( selected, name );
    }
}
///////////////////////////////////////////////////////////////////////////////
void HierarchyTree::ChangeLeafIcon( unsigned int id, std::string path )
{
    wxTreeItemId root = GetRootItem();
    wxTreeItemId selected = SearchTree(root, id);
  
    if( selected.IsOk() )
    {
        //Try and find default icons if needed
        std::map< std::string, wxImage >::iterator iter =
            defaultIconMap.find( path );
        if( iter != defaultIconMap.end() )
        {
            AddtoImageList( wxBitmap( wxBitmap( iter->second ).
                ConvertToImage().Rescale(iconsize, iconsize)));
            SetItemImage(selected, images->GetImageCount()-1);
            return;
        }

        //look in the aspen icon map
        std::string fullPath = path + ".xpm";
        std::map< std::string, char** > aspenPlusIconMap =
            GetAspenPlusIconMap();
        std::map< std::string, char** >::iterator aspenIconIter;
        aspenIconIter = aspenPlusIconMap.find( fullPath );
        if( aspenIconIter != aspenPlusIconMap.end() )
        {
            AddtoImageList( wxBitmap( wxBitmap( aspenIconIter->second ).
                ConvertToImage().Rescale(iconsize, iconsize)));
            SetItemImage(selected, images->GetImageCount()-1);
        }

        //Now see if the user has any jpgs in
        //the 2dicons directory for the application
        else
        {
            std::ifstream exists( fullPath.c_str() );
            if( exists.fail() )
            {
              return;
            }
            wxImage image;
            image.LoadFile( wxString( fullPath.c_str(), wxConvUTF8 ),
             wxBITMAP_TYPE_JPEG );
            AddtoImageList( wxBitmap( wxBitmap( image ).ConvertToImage().
                Rescale(iconsize, iconsize)));
            SetItemImage(selected, images->GetImageCount()-1);
        }

        return;
   }
}
///////////////////////////////////////////////////////////////////////////////
wxTreeItemId HierarchyTree::SearchTree( wxTreeItemId root, int id )
{
    wxTreeItemIdValue cookie;
    wxTreeItemId search;
    wxTreeItemId item = this->GetFirstChild( root, cookie );
    wxTreeItemId child;

    while( item.IsOk() )
    {
        ModuleData* modData = ( ModuleData* ) this->GetItemData( item );
        if( id == modData->modId )
        {
            return item;
        }
        if( this->ItemHasChildren( item ) )
        {
            wxTreeItemId search = SearchTree( item, id );
            if( search.IsOk() )
            {
                return search;
            }
        }
        item = this->GetNextChild( root, cookie );
    }

    /* Not found */
    wxTreeItemId dummy;
    return dummy;
}

