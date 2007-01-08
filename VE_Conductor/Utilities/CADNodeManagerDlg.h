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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CADNODE_MANAGER_DIALOG_H
#define CADNODE_MANAGER_DIALOG_H
/*!\file CADNodeManagerDialog.h
  CADNodeManagerDialog API
  */
/*!\class CADNodeManagerDialog
 * GUI class to manipulate CADNode tree.
 */
#ifndef STAND_ALONE
#include "VE_Open/skel/VjObsC.h"
#endif
#include "wx/dialog.h"
#include "wx/treectrl.h"
#include "wx/window.h"
#include "wx/button.h"
#include "VE_Conductor/Utilities/CADTreeBuilder.h"

#include "VE_Conductor/Utilities/CADNodePropsDlg.h"
#include "VE_Installer/include/VEConfig.h"

class wxFileName;

namespace VE_CAD
{
   class CADNode;
}
namespace VE_XML
{
   class DataValuePair;
}
#include <string>
#include <map>

namespace VE_Conductor
{
namespace GUI_Utilities
{
class CADTreeBuilder;

class VE_CONDUCTOR_UTILS_EXPORTS CADNodeManagerDlg: public wxDialog{
public:
   ///Constructor
   ///\param node The CADNode to manage with this dialog.
   ///\param parent The parent wxWindow.
   ///\param id The unique id for this window.
   CADNodeManagerDlg(VE_CAD::CADNode* node, wxWindow* parent, int id);
                       
   ///Destructor
   virtual ~CADNodeManagerDlg();
   enum GEOMETRY_DIALOG_IDS
   {
      TREE_ID=wxID_HIGHEST+1,///<The tree ID.
      PROPERTY_ID,///<The property ID.
      GEOM_SAVE///<The save ID.
   };
#ifndef STAND_ALONE
   ///Set the current vjObjs ptr for data passing.
   ///\param xplorerCom The communication interface w/ xplorer.
   void SetVjObsPtr(VjObs_ptr xplorerCom);
#endif

   ///Clear out the current queue of instructions.
   void ClearInstructions();

   ///Set the root CADNode to display.
   ///\param rootNode The root CADNode to display.
   void SetRootCADNode(VE_CAD::CADNode* rootNode);

   ///Get the root CADNode
   VE_CAD::CADNode* GetRootCADNode();
//protected:

   ///Create the dialog
   void _buildDialog();
   ///Create an image list
   void _createImageList();

   ///Edit a node name.
   ///\param cmd The tree event.
   void _editLabel(wxTreeEvent& cmd);

   ///Select the active node
   ///\param event The tree event.
   void _setActiveNode(wxTreeEvent& event);

   ///Launch the CADNode modifier menu.
   ///\param event The right-click event.
   void _popupCADNodeManipulatorMenu(wxTreeEvent& event);

   ///Add a node to a selected node in the tree based from a VEG file.
   ///\param event The command event.
   void _addNodeFromVEGFile(wxCommandEvent& event);

   ///Create a VEG file from the  current CAD graph.
   ///\param event The command event.
   void _saveCADFile(wxCommandEvent& event);

   ///Launch the properties dialog
   ///\param event The command event.
   void _showPropertiesDialog(wxCommandEvent& event);

   ///Delete the selected node.
   ///\param event The tree event.
   void _deleteNode(wxCommandEvent& event);

   ///Create a new assembly node
   ///\param event The command event.
   void _createNewAssembly(wxCommandEvent& event);

   ///Create a part node from a raw CAD file.
   ///\param event The command event.
   void _addNodeFromCADFile(wxCommandEvent& event);
   
   ///Create a cloned node from an exisiting node in the tree.
   ///\param event The command event.
   void _cloneNode(wxCommandEvent& event);

   ///Make sure the tree is updated properly
   void _ensureTree();

   ///Expand a node in the wxTreeCtrl
   ///\param nodeID The node to expand
   void _expandNode(wxTreeItemId nodeID);

   ///Toggle a node on/off.
   ///\param event The command event.
   void _toggleNode(wxCommandEvent& event);

   ///Ensure that we haven't loaded this file already.
   ///If we have, create a CADClone.
   ///\param filename The filename to check
   bool _ensureClones(wxString filename);
#ifndef STAND_ALONE
   ///Send CAD commands back to VE-Xplorer
   void _sendCommandsToXplorer();
   VjObs_var _vjObsPtr;///<The VjObj ptr.
#endif
   ///This function takes a cad file name and constructs a command to send
   ///to xplorer
   ///\param fileName The filename to send to xplorer
   void SendNewNodesToXplorer( wxString fileName );

   ///This function takes a veg file name and constructs a command to send
   ///to xplorer
   ///\param fileName The filename to send to xplorer
   void SendVEGNodesToXplorer( wxString fileName );
   
   wxTreeCtrl* _geometryTree;///<The tree control.
   wxButton* _quitButton;///<The button to close the dialog.
   wxButton* _saveButton;///<The button to save the current CADHierarchy.
   wxRect _geomPosition;///<The initial position of the dialog.

   VE_Conductor::GUI_Utilities::CADTreeBuilder::TreeNodeData* _activeTreeNode;///<The active tree item.

   std::vector<VE_XML::DataValuePair*> _dataValuePairList;///<The DataValuePair s for the current command.

   std::map<std::string,bool> _toggleNodeOnOff;///Flag determining whether or not to display the selected CADNode.
   std::string _commandName;///<The command name.

   CADNodePropertiesDlg* _propsDlg;///<The Property dialog.

   VE_CAD::CADNode* _activeCADNode;///<The active CADNode.
   
   bool _cloneFromSameFile;///<Flag for parent checking when cloning.
   VE_CAD::CADNode* _rootNode;///<The active CADNode.
   VE_Conductor::GUI_Utilities::CADTreeBuilder* _cadTreeBuilder;///<The tree manager.
   std::map<wxString,VE_CAD::CADNode*> _loadedCAD;///<The list of CAD/VEG files already loaded.
   //wxButton* _loadButton;
   std::string ConvertUnicode( const wxChar* data )
   {
         std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
         return tempStr;
   }
   DECLARE_EVENT_TABLE()
};
}
}
#endif// CADNODE_MANAGER_DIALOG_H
