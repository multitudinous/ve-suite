/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/conductor/util/CORBAServiceList.h>
#include "Network.h"
#include "Canvas.h"
#include "HierarchyTree.h"
#include <ves/conductor/AspenPlus2DIcons.h>
#include <ves/conductor/UIPluginBase.h>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/model/System.h>

#include <wx/intl.h>

#include <ves/conductor/xpm/icon1.xpm>
#include <ves/conductor/xpm/icon2.xpm>
#include <ves/conductor/xpm/icon3.xpm>
#include <ves/conductor/xpm/icon4.xpm>
#include <ves/conductor/xpm/icon5.xpm>

#ifdef WIN32
#include <shellapi.h>
#endif

using namespace ves::open::xml;
using namespace ves::conductor::util;
using namespace ves::conductor;

BEGIN_EVENT_TABLE( HierarchyTree, wxTreeCtrl )
    EVT_TREE_SEL_CHANGED( TREE_CTRL, HierarchyTree::OnSelChanged )
    EVT_TREE_ITEM_EXPANDING( TREE_CTRL, HierarchyTree::OnExpanded )
END_EVENT_TABLE()

HierarchyTree::HierarchyTree( wxWindow *parent, const wxWindowID id,
                              const wxPoint& pos, const wxSize& size, long style )
        :
        wxTreeCtrl( parent, id, pos, size, style ),
        m_canvas( 0 )
{
    images = new wxImageList( 32, 32, TRUE );
    AssignImageList( images );
    AddtoImageList( wxIcon( icon3_xpm ) );
    AddtoImageList( wxIcon( icon4_xpm ) );
    AddtoImageList( wxIcon( icon5_xpm ) );
    m_rootId = AddRoot( wxT( "Top Sheet" ), 0, -1, NULL );
    m_currentNodeId = m_rootId;
    SetItemImage( m_rootId, 2, wxTreeItemIcon_Expanded );
    SetItemFont( m_rootId, *wxITALIC_FONT );
}
////////////////////////////////////////////////////////////////////////////////
HierarchyTree::~HierarchyTree()
{}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AddtoImageList( wxBitmap icon )
{
    images->Add( icon );
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateTree( std::map < std::string,
                                  ves::open::xml::model::ModelWeakPtr > tree, std::string id )
{
    ///Reset Tree
    Clear();

    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;

    for( std::map< std::string, ves::open::xml::model::ModelWeakPtr >::iterator
            iter = tree.begin(); iter != tree.end(); ++iter )
    {
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
        fullPath = "2DIcons/" + iter->second->GetIconFilename() + ".jpg";
        aspenIconIter = aspenPlusIconMap.find( fullPath );
        if( aspenIconIter != aspenPlusIconMap.end() )
        {
            AddtoImageList( wxBitmap( wxBitmap(
                                          aspenIconIter->second ).ConvertToImage().Rescale( 32, 32 ) ) );
        }
        else
        {
            AddtoImageList( wxIcon( icon1_xpm ) );
        }

        wxTreeItemId leaf = AppendItem( m_rootId,
                                        wxString( iter->second->GetModelName().c_str(), wxConvUTF8 ),
                                        images->GetImageCount() - 1 , -1, modData );
        SetItemImage( leaf, images->GetImageCount() - 1 );

        if( iter->second->GetSubSystem() )
        {
            PopulateLevel( leaf, iter->second->GetSubSystem()->GetModels(),
                           iter->second->GetSubSystem()->GetID() );
        }
    }
    m_currentNodeId = m_rootId;
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateLevel( wxTreeItemId parentLeaf,
                                   std::vector< ves::open::xml::model::ModelWeakPtr > models, std::string id )
{
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;

    for( size_t i = 0; i < models.size(); ++i )
    {
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
        fullPath = "2DIcons/" + models[i]->GetIconFilename() + ".jpg";
        aspenIconIter = aspenPlusIconMap.find( fullPath );
        if( aspenIconIter != aspenPlusIconMap.end() )
        {
            AddtoImageList( wxBitmap( wxBitmap(
                                          aspenIconIter->second ).ConvertToImage().Rescale( 32, 32 ) ) );
        }
        else
        {
            AddtoImageList( wxIcon( icon1_xpm ) );
        }

        wxTreeItemId leaf = AppendItem( parentLeaf,
                                        wxString( models[i]->GetModelName().c_str(), wxConvUTF8 ),
                                        images->GetImageCount() - 1 , -1, modData );
        SetItemImage( leaf, images->GetImageCount() - 1 );

        if( models[i]->GetSubSystem() )
        {
            PopulateLevel( leaf, models[i]->GetSubSystem()->GetModels(),
                           models[i]->GetSubSystem()->GetID() );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::Clear()
{
    DeleteAllItems();
    images->RemoveAll();
    AddtoImageList( wxIcon( icon3_xpm ) );
    AddtoImageList( wxIcon( icon4_xpm ) );
    AddtoImageList( wxIcon( icon5_xpm ) );
    m_rootId = AddRoot( wxT( "Top Sheet" ), 0, 1, NULL );
    SetItemImage( m_rootId, 2, wxTreeItemIcon_Expanded );
    SetItemFont( m_rootId, *wxITALIC_FONT );
    m_currentNodeId = m_rootId;
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnSelChanged( wxTreeEvent& WXUNUSED( event ) )
{
    if( GetSelection() != m_rootId )
    {
        ModuleData* tempModData =
            static_cast< ModuleData* >( this->GetItemData( GetSelection() ) );
        m_canvas->SetActiveNetwork( tempModData->systemId );
        m_canvas->GetActiveNetwork()->HighlightCenter( tempModData->modId );
        m_currentNodeId = this->GetItemParent( this->GetSelection() );
    }
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnExpanded( wxTreeEvent& WXUNUSED( event ) )
{
    /*if( GetSelection() != m_rootId )
    {
           ModuleData* tempModData = 
               static_cast< ModuleData* >( GetItemData( GetSelection() ) );
        m_canvas->SetActiveNetwork( tempModData->subSystemId );
    }*/
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AddtoTree( UIPluginBase* cur_module )
{
    ModuleData* modData = new ModuleData();
    modData->modId = cur_module->GetID();
    modData->modName = ConvertUnicode( cur_module->GetName() );
    modData->systemId = m_canvas->GetActiveNetworkID( );
    AddtoImageList( wxBitmap( square_xpm ) );

    wxTreeItemId leaf = AppendItem( m_currentNodeId, wxString(
                                        cur_module->GetName().c_str(), wxConvUTF8 ), -1 , -1, modData );
    SetItemImage( leaf, images->GetImageCount() - 1 );
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
