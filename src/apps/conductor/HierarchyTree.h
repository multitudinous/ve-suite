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
#ifndef HIERARCHY_TREE_H
#define HIERARCHY_TREE_H
/*!\file HierarchyTree.h
HierarchyTree API
*/
/*!\class HierarchyTree
*
*/
#include <ves/open/xml/model/Model.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <vector>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/treectrl.h>

//Forward declaration of classes
namespace ves
{
    namespace conductor
    {
        class Canvas;
        class UIPluginBase;
    }
}
class PluginLoader;

class HierarchyTree : public wxTreeCtrl
{
public:
    
    ///Default constructor
    HierarchyTree()
    {
        ;
    }

    ///Normal constructor
    HierarchyTree( wxWindow *parent, const wxWindowID id, const wxPoint& pos,
        const wxSize& size, long style );

    ///Destructor
    virtual ~HierarchyTree();

    ///Populate the tree
    ///\param id The top level system id to use for this tree to populate
    void PopulateTree( );

    ///Create image list of size
    ///\param size Size of images
    void CreateImageList( int size = 16 );

    ///Add image to list
    ///\param wxBitmap image to be added
    void AddtoImageList( wxBitmap );

    ///Set the canvas to work with
    ///\param can canvas to work with
    void SetCanvas( ves::conductor::Canvas *can )
    {
        m_canvas = can;
    }
    
    ///add a module to the tree
    void AddtoTree( ves::conductor::UIPluginBase *cur_module );

    ///remove a module to the tree
    void RemoveFromTree( unsigned int id );
    
    ///used with making hierarchy blocks to add a defautl block to new level 
    void AppendToTree( unsigned int parentID, unsigned int id );
    
    ///change the item
    void SetTreeItemName( unsigned int id, wxString name );

    ///utility functions used to locate an item either for removal or appending
    wxTreeItemId SearchTree( wxTreeItemId root, int id );

    ///change the item's image - for use with icon chooser
	void ChangeLeafIcon( unsigned int id, std::string path );

    ///Clear the hierarchy tree
    ///This is called by default by PopulateTree
    void Clear();

protected:
    
    ///The size of the images
    int m_imageSize;
    
    ///single click selection
    void OnSelChanged( wxTreeEvent& event );
    
    //when expanding
    void OnExpanded( wxTreeEvent& event );
    
    //right clicking
    void OnRightClick( wxTreeEvent& event );
    
    //double clicking
    void OnDoubleClick( wxTreeEvent& event );
    
    ///used to select the plugin
    void SelectNetworkPlugin( wxTreeItemId treeId );
    
    ///post right click menu events up to plugin.
    void ProcessRightClickMenuEvents( wxCommandEvent& event );

    ///tree ids
    wxTreeItemId m_rootId;
    wxTreeItemId m_selection;
    wxTreeItemId m_currentLevelId;
    
    ///a pointer to the main canvas
    ves::conductor::Canvas* m_canvas;
    
    ///the list of images
    wxImageList *images;
    
    ///the default lookup
    std::map< std::string, wxImage > defaultIconMap;

    ///utility unicode conversion method
    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >
            ( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }

    ///the size to rescale of the images in the list
    int iconsize;

    ///used for recursively populating sub systems
    void PopulateLevel( wxTreeItemId parentLeaf,
        std::vector< ves::open::xml::model::ModelPtr > models,
        std::string id );

    DECLARE_EVENT_TABLE();
};

///class for storing information regarding modules
class ModuleData : public wxTreeItemData
{
public:
    unsigned int modId;
    std::string modelUUID;
    std::string modName;
    std::string systemId;
    std::string subSystemId;
};

#endif
