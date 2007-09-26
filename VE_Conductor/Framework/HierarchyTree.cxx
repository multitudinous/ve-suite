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
 * Date modified: $Date: 2007-09-14 21:37:05 -0400 (Fri, 14 Sep 2007) $
 * Version:       $Rev: 9051 $
 * Author:        $Author: mccdo $
 * Id:            $Id: HierarchyTree.cxx 9051 2007-09-15 01:37:05Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/CORBAServiceList.h"
#include "VE_Conductor/Framework/Network.h"
#include "VE_Conductor/Framework/HierarchyTree.h"
#include "VE_Conductor/GUIPlugin/AspenPlus2DIcons.h"

#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/Model/System.h"

#include <wx/intl.h>

#include "VE_Conductor/xpm/icon1.xpm"
#include "VE_Conductor/xpm/icon2.xpm"
#include "VE_Conductor/xpm/icon3.xpm"
#include "VE_Conductor/xpm/icon4.xpm"
#include "VE_Conductor/xpm/icon5.xpm"

#ifdef WIN32
#include <shellapi.h>
#endif

BEGIN_EVENT_TABLE(HierarchyTree, wxTreeCtrl)
  //EVT_TREE_SEL_CHANGED(TREE_CTRL, HierarchyTree::OnSelChanged)
  EVT_TREE_SEL_CHANGED(TREE_CTRL, HierarchyTree::OnSelChanged)
END_EVENT_TABLE()
   
HierarchyTree::HierarchyTree(wxWindow *parent, const wxWindowID id, 
    const wxPoint& pos, const wxSize& size,long style)
:
wxTreeCtrl(parent, id, pos, size, style), 
m_network(0)
{
  images = new wxImageList(32, 32, TRUE);
  AssignImageList(images);
  AddtoImageList(wxIcon( icon3_xpm ));
  AddtoImageList(wxIcon( icon4_xpm ));
  AddtoImageList(wxIcon( icon5_xpm ));
  m_rootId = AddRoot( wxT( "Top Sheet" ), 0, -1, NULL );
  SetItemImage(m_rootId, 2, wxTreeItemIcon_Expanded);
  SetItemFont(m_rootId, *wxITALIC_FONT);
}
////////////////////////////////////////////////////////////////////////////////
HierarchyTree::~HierarchyTree()
{
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::AddtoImageList(wxBitmap icon)
{    
	images->Add(icon);
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateTree(std::map< std::string, 
    VE_XML::VE_Model::Model > tree)
{
    ///Reset Tree
    Clear();
    
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;

	for( std::map< std::string, VE_XML::VE_Model::Model >::iterator 
        iter = tree.begin(); iter != tree.end(); ++iter )
	{
		ModuleData* modData = new ModuleData();
		modData->modId = iter->second.GetModelID();
		modData->modName = iter->second.GetModelName();
		
		//Add the icon to the image list
		fullPath = "2DIcons/" + iter->second.GetIconFilename() + ".jpg";
		aspenIconIter = aspenPlusIconMap.find( fullPath );
		if( aspenIconIter != aspenPlusIconMap.end() )
        {
			AddtoImageList( wxBitmap( wxBitmap( 
                aspenIconIter->second ).ConvertToImage().Rescale(32, 32)));
        }
		else
        {
			AddtoImageList(wxIcon( icon1_xpm ));
        }

		wxTreeItemId leaf = AppendItem(GetRootItem(), 
            wxString( iter->second.GetModelName().c_str(), wxConvUTF8 ),
			images->GetImageCount()-1 , -1, modData);
		SetItemImage(leaf, images->GetImageCount()-1);

		if( iter->second.GetSubSystem() )
        {
			PopulateLevel(leaf, iter->second.GetSubSystem()->GetModels());
        }
	}
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::PopulateLevel(wxTreeItemId parentLeaf, 
    std::vector< VE_XML::VE_Model::ModelWeakPtr > models)
{
    std::map< std::string, char** > aspenPlusIconMap = GetAspenPlusIconMap();
    std::map< std::string, char** >::iterator aspenIconIter;
    std::string fullPath;
    
	for( size_t i = 0; i < models.size(); ++i )
	{
		ModuleData* modData = new ModuleData();
		modData->modId = models[i]->GetModelID();
		modData->modName = models[i]->GetModelName();
		
		//Add the icon to the image list
		fullPath = "2DIcons/" + models[i]->GetIconFilename() + ".jpg";
		aspenIconIter = aspenPlusIconMap.find( fullPath );
		if( aspenIconIter != aspenPlusIconMap.end() )
		{	
            AddtoImageList( wxBitmap( wxBitmap( 
                aspenIconIter->second ).ConvertToImage().Rescale(32, 32)));
        }
		else
		{	
            AddtoImageList(wxIcon( icon1_xpm ));
        }

		wxTreeItemId leaf = AppendItem(parentLeaf, 
            wxString( models[i]->GetModelName().c_str(), wxConvUTF8 ), 
			images->GetImageCount()-1 , -1, modData);
		SetItemImage( leaf, images->GetImageCount()-1 );

		if( models[i]->GetSubSystem() )
        {
			PopulateLevel( leaf, models[i]->GetSubSystem()->GetModels() );
        }
	}
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::Clear()
{
  DeleteAllItems();
  images->RemoveAll();
  AddtoImageList(wxIcon( icon3_xpm ));
  AddtoImageList(wxIcon( icon4_xpm ));
  AddtoImageList(wxIcon( icon5_xpm ));
  m_rootId = AddRoot( wxT( "Top Sheet" ), 0, 1, NULL );
  SetItemImage(m_rootId, 2, wxTreeItemIcon_Expanded);
  SetItemFont(m_rootId, *wxITALIC_FONT);
}
////////////////////////////////////////////////////////////////////////////////
void HierarchyTree::OnSelChanged(wxTreeEvent& WXUNUSED(event))
{
	if( GetSelection() != m_rootId )
	{
        ModuleData* tempModData = 
            static_cast< ModuleData* >( GetItemData( GetSelection() ) );
	    m_network->HighlightCenter( tempModData->modId );
	}
}
////////////////////////////////////////////////////////////////////////////////
