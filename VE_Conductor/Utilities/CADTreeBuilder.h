/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: CADTreeBuilder.h,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef CAD_TREE_BUILDER
#define CAD_TREE_BUILDER

/*!\file CADTreeBuilder.h
  CADTreeBuilder API
  */
/*!\class VE_CAD::CADTreeBuilder
 * Class for creating wxTreeCtrl and wxTreeItem from VE_CAD::CADNode.
 */
#include <wx/treectrl.h>
#include <wx/window.h>
#include <vector>
namespace VE_CAD
{
   class CADNode;
}
#include "VE_Installer/include/VEConfig.h"
#include "VE_Open/XML/CAD/CADNodeTraverser.h"
namespace VE_Conductor
{
namespace GUI_Utilities
{
   class /*VE_CAD_EXPORTS*/ CADTreeBuilder: public VE_CAD::CADNodeTraverser
   {
      public:
         enum TREE_IDS
         {
            TREE_ID=1010
         };
         ///Constructor
         ///\param cadNode The root CADNode.
         ///\param parent The parent wxWindow
         ///\param id The ID for the tree control.
         CADTreeBuilder(VE_CAD::CADNode* cadNode,wxWindowID id,wxWindow* parent);
         
         ///Copy Constructor
         //CADTreeBuilder(const CADTreeBuilder& cfdNT);
         ///Destructor
         virtual ~CADTreeBuilder();

         class TreeGraphPreCallback: public CADNodeTraverser::CADNodeTraverseCallback{
         public:
            ///Constructor
            TreeGraphPreCallback(){;}
	         
            ///Destructor
            virtual ~TreeGraphPreCallback(){;}

            ///This is the function to override to do something to a node
            ///before (pre) or after (post) encountering a CADNode in the graph.
            ///\param cadNodeTraverser The CADTreeBuilder that is doing the traversing.
            ///\param node The CADNode that is currently being encountered.
            ///\param currentParent The CADNode that is the parent of the node being encountered.
	         void Apply(CADNodeTraverser* sceneGraphBuilder,VE_CAD::CADNode* node,void* currentParent=0);
         protected:
      };

      class TreeGraphPostCallback: public CADNodeTraverser::CADNodeTraverseCallback{
         public:
            ///Constructor
            TreeGraphPostCallback(){;}
	         
            ///Destructor
            virtual ~TreeGraphPostCallback(){;}

            ///This is the function to override to do something to a node
            ///before (pre) or after (post) encountering a CADNode in the graph.
            ///\param cadNodeTraverser The CADTreeBuilder that is doing the traversing.
            ///\param node The CADNode that is currently being encountered.
            ///\param currentParent The CADNode that is the parent of the node being encountered.
	         void Apply(CADNodeTraverser* sceneGraphBuilder,VE_CAD::CADNode* node,void* currentParent=0);
         protected:
      };

      /*!\class VE_CAD::CADTreeBuilder::TreeNodeData
       * Class to pair the CADNode with and item in the tree.
       */
      class TreeNodeData : public wxTreeItemData
      {
      public:
         ///Constructor
         ///\param node The holder of the node data.
         TreeNodeData(VE_CAD::CADNode* node){ _cadNode = node;}

         ///Destructor
         virtual ~TreeNodeData();
            
         ///Get the CADNode.
         VE_CAD::CADNode* GetNode(){return _cadNode;}
      protected:
         VE_CAD::CADNode* _cadNode;///<The pointer to the CADNode this tree item represents.
      };

      ///\param Pop the current parent.
      void PopCurrentParent();

      ///Set the current parent in the wxTreeCtrl
      ///\param parent The new parent
      void SetCurrentParentNode(wxTreeItemId parent);

      ///Set the current wxTreeCtrl
      void SetWXTreeCtrl(wxTreeCtrl* tree);

      ///Get the current parent in the wxTreeCtrl
      wxTreeItemId GetCurrentParentNode();
      
      ///Search the tree for a CADNode.
      ///\param name The name to search for.
      VE_CAD::CADNode* GetCADNode(std::string name);
      
       ///Get the created root node.
      wxTreeCtrl* GetWXTreeCtrl();

      ///Check for a root node.
      bool HasRoot();

      //equal operator
      //CADTreeBuilder& operator=(const CADTreeBuilder& cfdNT);
   protected:
      ///Create the image list.
      void _createImageList();
      std::vector<VE_CAD::CADNode*> _nodeList;///<List of all the nodes in our tree.
      CADTreeBuilder::TreeGraphPreCallback* _treeCtrlCreator;///<The pre traverse callback that creates a wxTreeCtrl.
      CADTreeBuilder::TreeGraphPostCallback* _parentPopper;///<Pops the parent stack.
      wxWindow* _parentWindow;
      wxTreeCtrl* _treeCtrl; ///<The tree ctrl. 
      std::vector<wxTreeItemId> _parentList;///<The current parent node during traversal;
   };
}
}
#endif//CAD_TREE_BUILDER
