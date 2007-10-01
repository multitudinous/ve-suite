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
 * Date modified: $Date: 2007-08-24 12:53:30 -0400 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author: mccdo $
 * Id:            $Id: HierarchyTree.h 8827 2007-08-24 16:53:30Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef HIERARCHY_TREE_H
#define HIERARCHY_TREE_H
/*!\file HierarchyTree.h
HierarchyTree API
*/
/*!\class HierarchyTree
* 
*/
#include "VE_Open/XML/Model/Model.h"
#include "VE_Open/XML/Model/ModelPtr.h"
#include <vector>
#include <wx/image.h>
#include <wx/imaglist.h>
#include <wx/treectrl.h>

class Network;
class UIPluginBase;
class PluginLoader;

class HierarchyTree : public wxTreeCtrl
{
public:
    ///Default constructor
    HierarchyTree() {;}
    ///Normal constructor
    HierarchyTree(wxWindow *parent, const wxWindowID id, const wxPoint& pos, const wxSize& size,long style);
    ///Destructor
    virtual ~HierarchyTree();

    enum
    {
		TREE_CTRL=1000
    };

    ///Populate the tree
    ///\param tree The tree to populate
    void PopulateTree(std::map< std::string, VE_XML::VE_Model::ModelWeakPtr > tree);
    ///Create image list of size
    ///\param size Size of images
    void CreateImageList(int size=16);
	void AddtoImageList(wxBitmap);
    ///Set the network to work with
    ///\param nw Network to work with
    void SetNetwork(Network *nw) { m_network = nw; };
    ///Clear the hierarchy tree
    ///This is called by default by PopulateTree
    void Clear();

protected:
    ///The size of the images
    int m_imageSize;
	void PopulateLevel(wxTreeItemId parentLeaf, std::vector< VE_XML::VE_Model::ModelWeakPtr > models);
	void OnSelChanged(wxTreeEvent& event);
    
    wxTreeItemId m_rootId;
    wxTreeItemId m_selection;
    Network* m_network;
	wxImageList *images;

    DECLARE_EVENT_TABLE();
};

class ModuleData : public wxTreeItemData
{
 public:
  unsigned int modId;
  std::string modName;
};

#endif
