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
#include <ves/conductor/util/CADTreeBuilder.h>

#include "ves/open/xml/cad/CADNode.h" 
#include "ves/open/xml/cad/CADPart.h" 
#include "ves/open/xml/cad/CADAssembly.h" 
#include <ves/open/xml/cad/CADNodeTraverser.h>

#include <wx/imaglist.h>
#include <wx/image.h>
#include <wx/icon.h>

#include <ves/conductor/xpm/cad_tree_expanded.xpm>
#include <ves/conductor/xpm/cad_tree_selected.xpm>
#include <ves/conductor/xpm/cad_tree_unselected.xpm>

#include <string>

using namespace ves::open::xml::cad;
using namespace ves::conductor::util;

//////////////////////////////////////////
///Constructor                          //
//////////////////////////////////////////
CADTreeBuilder::CADTreeBuilder(CADNode* root,int id ,wxWindow* parent)
{
   SetRootNode(root);
   _parentWindow = parent;
   _treeCtrlCreator = new TreeGraphPreCallback();
   _parentPopper = new TreeGraphPostCallback();

   _treeCtrl =  new wxTreeCtrl(_parentWindow,id,wxDefaultPosition,wxDefaultSize,
										 wxTR_HAS_BUTTONS|
										 wxTR_EDIT_LABELS|
										 wxTR_HAS_BUTTONS|
										 wxTR_HAS_VARIABLE_ROW_HEIGHT|wxTR_DEFAULT_STYLE|wxVSCROLL);

   _createImageList();

   SetPreNodeTraverseCallback(_treeCtrlCreator);
   SetPostNodeTraverseCallback(_parentPopper);
}
//////////////////////////////////////////////////////////////////////////
///Copy Constructor                                                     //
//////////////////////////////////////////////////////////////////////////
/*CADTreeBuilder::CADTreeBuilder(const CADTreeBuilder& cfdNT)
{
   _treeCtrl = new wxTreeCtrl(*cfdNT._treeCtrl);
   _currentParent = cfdNT._currentParent;

   _treeCtrlCreator = cfdNT._treeCtrlCreator;
}*/
///////////////////////////////////////////
//Destructor                             //
///////////////////////////////////////////
CADTreeBuilder::~CADTreeBuilder()
{
   if(_treeCtrlCreator)
   {
      delete _treeCtrlCreator;
      _treeCtrlCreator = 0;
   }

   if(_parentPopper)
   {
      delete _parentPopper;
      _parentPopper = 0;
   }

   _nodeList.clear();
}
////////////////////////////////////////////////
///Get the created root node.                 //
////////////////////////////////////////////////
wxTreeCtrl* CADTreeBuilder::GetWXTreeCtrl()
{
   if(_treeCtrl)
   {
      return _treeCtrl;
   }

   return 0;
}
////////////////////////////////////////////////////
void CADTreeBuilder::SetWXTreeCtrl(wxTreeCtrl* tree)
{
   _treeCtrl = tree;
}
/////////////////////////////////////////////////////////////////////////////////////
///equal operator                                                                  //
/////////////////////////////////////////////////////////////////////////////////////
/*CADTreeBuilder& CADTreeBuilder::operator=(const CADTreeBuilder& cfdNT)
{
   if(this != &rhs)
   {
      if(_treeCtrl)
      {
         delete _treeCtrl;
         _treeCtrl = 0;
      }
      _treeCtrl = new wxTreeCtrl(*cfdNT._treeCtrl);
      _currentParent = cfdNT._currentParent;

      _treeCtrlCreator = cfdNT._treeCtrlCreator;
   }
   return *this;
}*/
//////////////////////////////////////////////////////////////////////////////////////
CADTreeBuilder::TreeNodeData::TreeNodeData(CADNode* node)
{
   _cadNode = node;
}
/////////////////////////////////////////////
CADTreeBuilder::TreeNodeData::~TreeNodeData()
{
   /*if(_cadNode)
   {
      delete _cadNode;
      _cadNode = 0;
   }*/
}
//////////////////////////////////////////////////////////
CADNode* CADTreeBuilder::GetCADNode(std::string name)
 {
    size_t nNodes = _nodeList.size();
    for(size_t i = 0; i < nNodes; i++)
    {
       if(_nodeList.at(i)->GetNodeName() == name)
          return _nodeList.at(i);
    }
    return 0;
 }
 ///////////////////////////////////////////////////////////
 wxTreeItemId CADTreeBuilder::GetCurrentParentNode()
 {
    if(_parentList.size())
    {
       return _parentList.back();
    }

 }
 //////////////////////////////////////
 /*void CADTreeBuilder::ClearParentList()
 {
    _parentList.clear();
 }*/
 //////////////////////////////
 bool CADTreeBuilder::HasRoot()
 {
    if(_parentList.size())
    {
       return true;
    }
    return false;
 }
 ///////////////////////////////////////////////////////////
 void CADTreeBuilder::SetCurrentParentNode(wxTreeItemId parent)
 {

    _parentList.push_back(parent);
 }
 ///////////////////////////////////////
 void CADTreeBuilder::PopCurrentParent()
 {
    if(_parentList.size())
    {
       _parentList.pop_back();
    }
 }
//////////////////////////////////////////////////////////////////////////////////////////
void CADTreeBuilder::TreeGraphPreCallback::Apply(CADNodeTraverser* treeBuilder, CADNode* node, void* currentParent)
{
   CADTreeBuilder* treeGraph = dynamic_cast<CADTreeBuilder*>(treeBuilder);
   if(!treeGraph)
      return;

   //add the transform to the current parent
   if(treeGraph->GetWXTreeCtrl()->GetRootItem().IsOk())
   {
      wxTreeItemId currentParentId;
        
      if(node->GetNodeType() == std::string("Assembly"))
      {
         currentParentId = treeGraph->GetWXTreeCtrl()->AppendItem(treeGraph->GetCurrentParentNode(),
                                                                     wxString(node->GetNodeName().c_str(), wxConvUTF8 )
                                                                     ,0,2,new TreeNodeData(node));
      
         //update the current parent
         treeGraph->SetCurrentParentNode(currentParentId);

         treeGraph->GetWXTreeCtrl()->SetItemImage(currentParentId, 2, wxTreeItemIcon_Expanded);
         treeGraph->GetWXTreeCtrl()->SetItemImage(currentParentId, 2, wxTreeItemIcon_SelectedExpanded);
      }
      else if(node->GetNodeType() == std::string("Part"))
      {
         currentParentId = treeGraph->GetWXTreeCtrl()->AppendItem(treeGraph->GetCurrentParentNode(),
                                                                     wxString(node->GetNodeName().c_str(), wxConvUTF8 )
                                                                     ,0,1,new TreeNodeData(node));
      }
   }
   else
   {
      //must be the root node
      wxTreeItemId currentParentId = treeGraph->GetWXTreeCtrl()->AddRoot(wxString(node->GetNodeName().c_str(), wxConvUTF8 ),
                                                                  0,2,new TreeNodeData(node));

      treeGraph->SetCurrentParentNode(currentParentId);

      treeGraph->GetWXTreeCtrl()->SetItemImage(currentParentId, 2, wxTreeItemIcon_Expanded);
      treeGraph->GetWXTreeCtrl()->SetItemImage(currentParentId, 2, wxTreeItemIcon_SelectedExpanded);
   }
}
///////////////////////////////////////////
void CADTreeBuilder::_createImageList()
{
    // Make an image list containing small icons
    wxImageList *images = new wxImageList(32, 32, true);

    // should correspond to TreeCtrlIcon_xxx enum
    wxBusyCursor wait;
    wxIcon icons[3];
    icons[0] = wxIcon(cad_tree_unselected_xpm);
    icons[1] = wxIcon(cad_tree_selected_xpm);
    icons[2] = wxIcon(cad_tree_expanded_xpm);

    int sizeOrig = icons[0].GetWidth();
    for ( size_t i = 0; i < WXSIZEOF(icons); i++ )
    {
        if ( 32 == sizeOrig )
        {
            images->Add(icons[i]);
        }
        else
        {
            images->Add(wxBitmap(wxBitmap(icons[i]).ConvertToImage().Rescale(32, 32)));
        }
    }

    _treeCtrl->AssignImageList(images);
}
//////////////////////////////////////////////////////////////////////////////////////////
void CADTreeBuilder::TreeGraphPostCallback::Apply( CADNodeTraverser* treeBuilder, CADNode* node, void* currentParent )
{
   CADTreeBuilder* treeGraph = dynamic_cast<CADTreeBuilder*>(treeBuilder);

   if( !treeGraph )
	{
      return;
	}

   if( node->GetNodeType() == std::string("Assembly") )
   {
      treeGraph->PopCurrentParent();
   }
}
