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
 * File:          $RCSfile: SceneGraphBuilder.cxx,v $
 * Date modified: $Date: 2006-01-10 13:45:28 -0600 (Tue, 10 Jan 2006) $
 * Version:       $Rev: 3477 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Conductor/Utilities/CADNodeManagerDlg.h"
#include "VE_Conductor/Utilities/CADNodeMenu.h"
#include "VE_Conductor/Utilities/CADTreeBuilder.h"
#include "VE_Conductor/Utilities/CADNodePropsDlg.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/XMLReaderWriter.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"
#include "VE_Open/XML/Transform.h"
#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/filename.h>

using namespace VE_Conductor::GUI_Utilities;
using namespace VE_CAD;
using namespace VE_XML;

BEGIN_EVENT_TABLE(CADNodeManagerDlg,wxDialog)
   EVT_TREE_END_LABEL_EDIT(TREE_ID,CADNodeManagerDlg::_editLabel)
   EVT_TREE_SEL_CHANGED(TREE_ID,CADNodeManagerDlg::_setActiveNode)
   //EVT_TREE_ITEM_MENU(TREE_ID,CADNodeManagerDlg::_popupCADNodeManipulatorMenu)
   //EVT_TREE_ITEM_RIGHT_CLICK(TREE_ID, CADNodeManagerDlg::_popupCADNodeManipulatorMenu)
   //EVT_RIGHT_DOWN( CADNodeManagerDlg::_popupCADNodeManipulatorMenu )
   EVT_CONTEXT_MENU(CADNodeManagerDlg::_popupCADNodeManipulatorMenu)
   EVT_BUTTON(GEOM_SAVE,CADNodeManagerDlg::_saveCADFile)
   
   EVT_MENU(CADNodeMenu::GEOM_PROPERTIES,CADNodeManagerDlg::_showPropertiesDialog)
   EVT_MENU(CADNodeMenu::GEOM_DELETE,CADNodeManagerDlg::_deleteNode)
   EVT_MENU(CADNodeMenu::GEOM_ASSEMBLY_CREATE,CADNodeManagerDlg::_createNewAssembly)
   EVT_MENU(CADNodeMenu::GEOM_VEG_FILE_ADD,CADNodeManagerDlg::_addNodeFromVEGFile)
   EVT_MENU(CADNodeMenu::GEOM_CAD_FILE_ADD,CADNodeManagerDlg::_addNodeFromCADFile)
   EVT_MENU(CADNodeMenu::GEOM_CLONE_ADD,CADNodeManagerDlg::_cloneNode)
   EVT_MENU(CADNodeMenu::GEOM_TOGGLE_ON,CADNodeManagerDlg::_toggleNode)
   EVT_MENU(CADNodeMenu::GEOM_TOGGLE_OFF,CADNodeManagerDlg::_toggleNode)
END_EVENT_TABLE()


/////////////////////////////////////////////////////////////////////
//Constructor                                                      //
/////////////////////////////////////////////////////////////////////
CADNodeManagerDlg::CADNodeManagerDlg(CADNode* node, wxWindow* parent, 
                       wxWindowID id)
:wxDialog(parent,id,wxString("CADTree Manager"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),wxString("CADTree Manager"))
{
   _rootNode = 0;
   _toggleNodeOnOff = true;

   SetRootCADNode(node);

   _propsDlg = 0;
   _cadTreeBuilder =  new CADTreeBuilder(_rootNode,TREE_ID,this);
   _cadTreeBuilder->Traverse();
   _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();

   _quitButton = 0;
   _saveButton = 0;

   _activeTreeNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(_geometryTree->GetRootItem()));
   _activeCADNode = _rootNode;
   _commandName = std::string("CAD");

   _buildDialog();
}
/////////////////////////////////////////
CADNodeManagerDlg::~CADNodeManagerDlg()
{

   if(_cadTreeBuilder)
   {
      delete _cadTreeBuilder;
      _cadTreeBuilder = 0;
   }

   if(_propsDlg)
   {
      delete _propsDlg;
      _propsDlg = 0;
   }

   _dataValuePairList.clear();
}
#ifndef STAND_ALONE
//////////////////////////////////////////////////////////
void CADNodeManagerDlg::SetVjObsPtr(VjObs_ptr xplorerCom)
{
   _vjObsPtr = VjObs::_duplicate(xplorerCom);
}
#endif
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::SetRootCADNode(CADNode* rootNode)
{
   if(rootNode)
   {
      if(_rootNode)
      {
         delete _rootNode;
         _rootNode = 0;
      }

      //Change this from a copy after we migrate to getting this pointer from the model
      if(rootNode->GetNodeType() == std::string("Assembly"))
      {
         _rootNode = new CADAssembly(*dynamic_cast<CADAssembly*>(rootNode));
      }
      else if(rootNode->GetNodeType() == std::string("Part"))
      {
         _rootNode = new CADPart(*dynamic_cast<CADPart*>(rootNode));
      }
   }
   else
   {
      _rootNode = new CADAssembly(std::string("Empty Assembly"));
   }
}
///////////////////////////////////////
void CADNodeManagerDlg::_buildDialog()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxHORIZONTAL);
   wxBoxSizer* buttonRowSizer = new wxBoxSizer(wxVERTICAL);

   wxStaticBox* cadTree = new wxStaticBox(this, -1, wxT("Tree View"));
   wxStaticBoxSizer* cadTreePropSizer = new wxStaticBoxSizer(cadTree, wxVERTICAL);

   wxBoxSizer* treeSizer = new wxBoxSizer(wxHORIZONTAL);
  
   _quitButton = new wxButton(this,wxID_OK,wxString("Close"));
   _saveButton = new wxButton(this,GEOM_SAVE,wxString("Save As..."));

   treeSizer->Add(_geometryTree,1,wxALIGN_CENTER|wxEXPAND); 
   cadTreePropSizer->Add(treeSizer,1,wxALIGN_CENTER|wxEXPAND);

   buttonRowSizer->Add(_saveButton,0,wxALIGN_CENTER);
   buttonRowSizer->Add(_quitButton,0,wxALIGN_CENTER);

   mainSizer->Add(cadTreePropSizer, 1, wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(buttonRowSizer, 1, wxALIGN_CENTER);
   SetAutoLayout(true);
   SetSizer(mainSizer);
}
///////////////////////////////////////////////////////
void CADNodeManagerDlg::_editLabel(wxTreeEvent& event)
{
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(event.GetItem().IsOk())
   {
      cadNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(event.GetItem()));
      cadNode->GetNode()->SetNodeName(event.GetLabel().GetData());

      _commandName = std::string("CAD_SET_NODE_NAME");

      VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
      nodeID->SetDataType("UNSIGNED INT");
      nodeID->SetDataValue(cadNode->GetNode()->GetID());
      nodeID->SetDataName(std::string("Node ID"));
      _dataValuePairList.push_back(nodeID);

      VE_XML::DataValuePair* nodeType = new VE_XML::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetData(std::string("Node Type"),cadNode->GetNode()->GetNodeType());
      _dataValuePairList.push_back(nodeType);

      VE_XML::DataValuePair* nodeName = new VE_XML::DataValuePair();
      nodeName->SetDataType("STRING");
      nodeName->SetData(std::string("Node Name"),cadNode->GetNode()->GetNodeName());
      _dataValuePairList.push_back(nodeName);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
}
///////////////////////////////////////////////////////////
void CADNodeManagerDlg::_setActiveNode(wxTreeEvent& event)
{
   if(event.GetItem().IsOk())
   {
      _activeTreeNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(event.GetItem()));
      _activeCADNode = _activeTreeNode->GetNode();
   }
   else
   {
      std::cout<<"Error setting active node!!!"<<std::endl;
      _activeTreeNode = 0;
      _activeCADNode = 0;
   }
}
/////////////////////////////////////////////////////////////////////////
//void CADNodeManagerDlg::_popupCADNodeManipulatorMenu(wxTreeEvent& event)
//void CADNodeManagerDlg::_popupCADNodeManipulatorMenu(wxMouseEvent& event)
void CADNodeManagerDlg::_popupCADNodeManipulatorMenu(wxContextMenuEvent& event)
{
   wxTreeItemId item = _geometryTree->GetSelection();
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(item.IsOk())
   {
      cadNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData( item ));

      CADNodeMenu* cadNodeMenu = new CADNodeMenu();
	  cadNodeMenu->SetToggleNodeValue(_toggleNodeOnOff);
      if(cadNode)
      {
         if(cadNode->GetNode()->GetNodeType() == std::string("Assembly"))
         {
            cadNodeMenu->EnableGlobalMenus(true);
            cadNodeMenu->EnableAssemblyMenus(true);
         }
         else if(cadNode->GetNode()->GetNodeType() == std::string("Part"))
         {
            cadNodeMenu->EnableGlobalMenus(true);
            cadNodeMenu->EnablePartMenus(true);
         }
         else if(cadNode->GetNode()->GetNodeType() == std::string("Clone"))
         {
            cadNodeMenu->EnableGlobalMenus(true);
            cadNodeMenu->EnablePartMenus(true);
            cadNodeMenu->EnableCloneMenu(false);
         }
      }
      if(cadNode->GetNode()->GetID() == _rootNode->GetID())
      {
         cadNodeMenu->EnableCloneMenu(false);
         cadNodeMenu->EnableDeleteMenu(false);
      }
      PopupMenu(cadNodeMenu,wxDefaultPosition);
      delete cadNodeMenu;
   }
}
/////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_createNewAssembly(wxCommandEvent& WXUNUSED(event))
{
   if(!_rootNode)
   {
     _rootNode = new CADAssembly();
     _activeCADNode = _rootNode;

     _cadTreeBuilder->SetRootNode(_rootNode);//,TREE_ID,this);
     _cadTreeBuilder->Traverse();
     _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();
   }
   else
   {

      if(_activeCADNode->GetNodeType() == std::string("Assembly"))
      {
         //pop a text dialog to enter the name of the new assembly
         wxTextEntryDialog assemblyNameDlg(this, 
                                       wxString("New Assembly Name"),
                                       wxString("Enter name for new assembly:"),
                                       wxString("Assembly"),wxOK);
         assemblyNameDlg.ShowModal();
         CADAssembly* newAssembly = new CADAssembly((assemblyNameDlg.GetValue().GetData()));
         newAssembly->SetParent(_activeCADNode->GetID());
         dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newAssembly);
 
         _geometryTree->AppendItem(_activeTreeNode->GetId(),
                                 wxString(newAssembly->GetNodeName().c_str()),
                                 2,4,new CADTreeBuilder::TreeNodeData(newAssembly)); 
         ClearInstructions();

         _commandName = std::string("CAD_ADD_NODE");

         VE_XML::DataValuePair* cadNode = new VE_XML::DataValuePair();
         cadNode->SetDataType(std::string("XMLOBJECT"));
         cadNode->SetData("New Node",newAssembly);
         _dataValuePairList.push_back(cadNode);

         VE_XML::DataValuePair* parentNode = new VE_XML::DataValuePair();
         parentNode->SetDataType(std::string("UNSIGNED INT"));
         parentNode->SetDataValue(_activeCADNode->GetID());
         parentNode->SetDataName(std::string("Parent ID"));
         _dataValuePairList.push_back(parentNode);

         /*VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
         nodeID->SetDataType("UNSIGNED INT");
         nodeID->SetDataValue(newAssembly->GetID());
         nodeID->SetDataName(std::string("Node ID"));
         _dataValuePairList.push_back(nodeID);

         VE_XML::DataValuePair* addNode = new VE_XML::DataValuePair();
         addNode->SetDataType("STRING");
         addNode->SetData(std::string("Node Type"),std::string("Assembly"));
         _dataValuePairList.push_back(addNode);

         VE_XML::DataValuePair* nodeName = new VE_XML::DataValuePair();
         nodeName->SetDataType(std::string("STRING"));
         nodeName->SetDataString(newAssembly->GetNodeName());
         nodeName->SetDataName(std::string("Node Name"));
         _dataValuePairList.push_back(nodeName);
*/
         _sendCommandsToXplorer();
         ClearInstructions();
      }
      else
      {
         wxMessageDialog errorDlg(this, wxString("Cannot add children to a Part node!!"),wxString("Error"));
         errorDlg.ShowModal();
      }
   }
}
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::_toggleNode(wxCommandEvent& event)
{
   if(_activeCADNode)
   {
      _commandName = "CAD_TOGGLE_NODE";

      VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
      nodeID->SetDataType("UNSIGNED INT");
      nodeID->SetDataValue(_activeCADNode->GetID());
      nodeID->SetDataName(std::string("Node ID"));
      _dataValuePairList.push_back(nodeID);

      VE_XML::DataValuePair* nodeType = new VE_XML::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetData(std::string("Node Type"),_activeCADNode->GetNodeType());
      _dataValuePairList.push_back(nodeType);

      VE_XML::DataValuePair* toggleValue = new VE_XML::DataValuePair();
      toggleValue->SetDataType("STRING");
      if(event.GetId() == CADNodeMenu::GEOM_TOGGLE_ON)
      {
         //std::cout<<"Toggle on!!"<<std::endl;
         toggleValue->SetData(std::string("Toggle Value"),std::string("ON"));
		 _toggleNodeOnOff = true;
      }
      else if(event.GetId() == CADNodeMenu::GEOM_TOGGLE_OFF)
      {
         //std::cout<<"Toggle off!!"<<std::endl;
         toggleValue->SetData(std::string("Toggle Value"),std::string("OFF"));
         _toggleNodeOnOff = false;
	  }
      _dataValuePairList.push_back(toggleValue);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
}
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::_cloneNode(wxCommandEvent& WXUNUSED(event))
{
   if(_activeCADNode &&  (_activeTreeNode->GetId() != _geometryTree->GetRootItem()))
   {
      CADClone* newClone = new CADClone(_activeCADNode->GetNodeName()+std::string("_cloned"),_activeCADNode);

      wxTreeItemId parentID = _geometryTree->GetItemParent(_activeTreeNode->GetId());

      CADTreeBuilder::TreeNodeData* parentCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(parentID));

      dynamic_cast<CADAssembly*>(parentCADNode->GetNode())->AddChild(newClone);

      _cadTreeBuilder->SetRootNode(_rootNode);
      _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
      _cadTreeBuilder->Traverse();
      _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();

      _commandName = std::string("CAD_ADD_NODE");

      VE_XML::DataValuePair* cadNode = new VE_XML::DataValuePair();
      cadNode->SetDataType(std::string("XMLOBJECT"));
      cadNode->SetData("New Node",newClone);
      _dataValuePairList.push_back(cadNode);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
   else
   {
       wxMessageBox( "Error! Can't Clone root node!!!.", 
                        "CAD Clone Failure", wxOK | wxICON_INFORMATION );
   }
}
////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeFromVEGFile(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog(this,
                       _T("Open file"), 
                       _T(""), 
                       _T(""),
                       _T("VE-Geometry files (*.veg)|*.veg;"),
                       wxOPEN|wxFILE_MUST_EXIST|wxMULTIPLE,//|wxCHANGE_DIR, 
                       wxDefaultPosition);
   
   if (dialog.ShowModal() == wxID_OK)
   {
      wxArrayString fileNamesVector;
      dialog.GetPaths( fileNamesVector );
      for ( size_t i = 0; i < fileNamesVector.GetCount(); ++i )
      {
         SendVEGNodesToXplorer( fileNamesVector.Item( i ) );
      }
   }
}
////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::SendVEGNodesToXplorer( wxString fileName )
{
   VE_XML::XMLReaderWriter cadReader;
   cadReader.UseStandaloneDOMDocumentManager();
   cadReader.ReadFromFile();
   cadReader.ReadXMLData(std::string( fileName.c_str() ),"CAD","CADAssembly");

   //CADNode* loadedNode = 0;
   CADAssembly* newAssembly = 0;
   CADPart* newPart = 0;
   std::vector<VE_XML::XMLObject*> loadedNodes;
   loadedNodes = cadReader.GetLoadedXMLObjects();

   if(loadedNodes.size())
   {
      //std::cout<<"---Loaded Assembly---"<<std::endl;
      newAssembly = new CADAssembly(*dynamic_cast<CADAssembly*>(loadedNodes.at(0)));
   }
   else
   {
      cadReader.ReadXMLData(std::string( fileName.c_str() ),"CAD","CADPart");
      loadedNodes = cadReader.GetLoadedXMLObjects();
      if(loadedNodes.size())
      {
         //std::cout<<"---Loaded Part---"<<std::endl;
         newPart = new CADPart(*dynamic_cast<CADPart*>(loadedNodes.at(0)));
      }
   }
   if(newAssembly || newPart)
   {
      //std::cout<<"Number of children on current root: "<<dynamic_cast<CADAssembly*>(_rootNode)->GetNumberOfChildren()<<std::endl;
      if(newAssembly)
      {
         if(dynamic_cast<CADAssembly*>(_rootNode)->GetNumberOfChildren() == 0)
         {
            //std::cout<<"Reseting root CADNode"<<std::endl;
            SetRootCADNode(newAssembly);
         }
         else
         {
            dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newAssembly);
         }
      }
      else if(newPart)
      {
         dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newPart);
      }
      _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
      _cadTreeBuilder->SetRootNode(_rootNode);
      _cadTreeBuilder->Traverse();
      _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();

      _commandName = "CAD_ADD_NODE";
      VE_XML::DataValuePair* cadNode = new VE_XML::DataValuePair();
      cadNode->SetDataType(std::string("XMLOBJECT"));

      if(newAssembly)
         cadNode->SetData("New Node",newAssembly);
      else if(newPart)
         cadNode->SetData("New Node",newPart);
      _dataValuePairList.push_back(cadNode);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
}
////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeFromCADFile(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog(this,
                       _T("Open file"), 
                       _T(""), 
                       _T(""),
                       _T("OSG files (*.osg;*.ive)|*.osg;*.ive;|SLT files (*.stl)|*.stl;|VRML files (*.wrl)|*.wrl;|OBJ files (*.obj)|*.obj;|Performer Binary files (*.pfb)|*.pfb| Flight files (*.flt)|*.flt"),
                       //"BMP and GIF files (*.bmp;*.gif)|*.bmp;*.gif|PNG files (*.png)|*.png"
                       wxOPEN|wxFILE_MUST_EXIST|wxMULTIPLE,
                       wxDefaultPosition);
                       
   if (dialog.ShowModal() == wxID_OK)
   {
      wxArrayString fileNamesVector;
      dialog.GetPaths( fileNamesVector );
      for ( size_t i = 0; i < fileNamesVector.GetCount(); ++i )
      {
         SendNewNodesToXplorer( fileNamesVector.Item( i ) );
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::SendNewNodesToXplorer( wxFileName fileName )
{
   ClearInstructions();
   wxFileName vegFileName( fileName );
   vegFileName.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
   wxString vegFileNamePath( wxString( "./" ) + vegFileName.GetFullPath() );
   wxFileName cadFileName( vegFileNamePath.c_str());
   //pop a text dialog to enter the name of the new assembly
   wxTextEntryDialog partNameDlg(this, 
                        wxString("New Part Name"),
                        wxString("Enter name for new part:"),
                        cadFileName.GetName(),wxOK);
   partNameDlg.ShowModal();


   CADPart* newCADPart = new CADPart(partNameDlg.GetValue().GetData());
   newCADPart->SetCADFileName( vegFileNamePath.c_str() );

   dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newCADPart);

   _cadTreeBuilder->SetRootNode(_rootNode);
   //std::cout<<"Deleting tree"<<std::endl;
   _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
   _cadTreeBuilder->Traverse();
   _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();

   _commandName = std::string("CAD_ADD_NODE");

   newCADPart->SetParent(_activeCADNode->GetID());

   VE_XML::DataValuePair* cadNode = new VE_XML::DataValuePair();
   cadNode->SetDataType(std::string("XMLOBJECT"));
   cadNode->SetData("New Node",newCADPart);
   _dataValuePairList.push_back(cadNode);

   _sendCommandsToXplorer();
   ClearInstructions();
}
/////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_saveCADFile(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog(this,
		       _T("Save file as..."), 
		       _T(""), 
		       _T(""),
		       _T("VE-Geometry files (*.veg)|*.veg"),
		       wxSAVE); 
   if (dialog.ShowModal() == wxID_OK){
      if (!dialog.GetPath().IsEmpty()) 
      {
         if(dialog.GetPath().Find(".veg") != -1)
         {
            VE_XML::XMLReaderWriter cadReader;
            cadReader.UseStandaloneDOMDocumentManager();
            cadReader.WriteToFile();
              
            std::string tagName("VECADNode");
            CADTreeBuilder::TreeNodeData* rootCADNode =
                 dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(_geometryTree->GetRootItem()));

            if(rootCADNode->GetNode()->GetNodeType() == std::string("Assembly"))
            {
               tagName = std::string("CADAssembly");
            }
            else if(rootCADNode->GetNode()->GetNodeType() == std::string("Part"))
            {
               tagName = std::string("CADPart");
            }
            std::string outputFile = std::string(dialog.GetPath());

            std::pair<CADNode*,std::string> nodeTagPair;
            nodeTagPair.first = rootCADNode->GetNode();
            nodeTagPair.second = tagName;
            std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
            nodeToWrite.push_back(nodeTagPair);
          
            cadReader.WriteXMLDocument(nodeToWrite, outputFile, "Command");
         }
      }
   }
}
////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_showPropertiesDialog(wxCommandEvent& WXUNUSED(event))
{
   if(_activeCADNode)
   {
      CADNodePropertiesDlg propsDlg(this,PROPERTY_ID,_activeCADNode);
#ifndef STAND_ALONE
      propsDlg.SetVjObsPtr(_vjObsPtr);
#endif
      propsDlg.ShowModal();
   }
}
//////////////////////////////////////////////
void CADNodeManagerDlg::ClearInstructions()
{
   ///deleting the command deletes the memory but
   ///we need to insure that the vector is clear
   _dataValuePairList.clear();
}
////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_deleteNode(wxCommandEvent& WXUNUSED(event))
{
    //don't allow the user to delete the root node!!!
    if(_activeCADNode == _rootNode)
    {
       std::cout<<"Error!!!!"<<std::endl;
       std::cout<<"Can't delete root node!!!"<<std::endl;
       return;
    }
    if(_activeTreeNode)
    {
      CADNode* parentCADNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(_geometryTree->GetItemParent(_activeTreeNode->GetId())))->GetNode();
       _geometryTree->Delete(_activeTreeNode->GetId());
       _commandName = std::string("CAD_DELETE_NODE");

       VE_XML::DataValuePair* deleteNode = new VE_XML::DataValuePair();
       deleteNode->SetDataType("STRING");
       deleteNode->SetData(std::string("Node Type"),_activeCADNode->GetNodeType());
       _dataValuePairList.push_back(deleteNode);

       VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
       nodeID->SetDataType("UNSIGNED INT");
       nodeID->SetDataValue(_activeCADNode->GetID());
       nodeID->SetDataName(std::string("Node ID"));
       _dataValuePairList.push_back(nodeID);

       VE_XML::DataValuePair* parentNode = new VE_XML::DataValuePair();
       parentNode->SetDataType(std::string("UNSIGNED INT"));
       parentNode->SetDataValue(_activeCADNode->GetParent());
       parentNode->SetDataName(std::string("Parent ID"));
       _dataValuePairList.push_back(parentNode);

       _sendCommandsToXplorer();
       ClearInstructions();

       dynamic_cast<CADAssembly*>(parentCADNode)->RemoveChild(_activeCADNode->GetID()); 
    }
}
#ifndef STAND_ALONE
////////////////////////////////////////////////
void CADNodeManagerDlg::_sendCommandsToXplorer()
{
   VE_XML::Command* cadCommand = new VE_XML::Command();

   for(size_t i =0; i < _dataValuePairList.size(); i++)
   {
      cadCommand->AddDataValuePair(_dataValuePairList.at(i));
   }
   cadCommand->SetCommandName(_commandName);
   std::string commandString("returnString");

   VE_XML::XMLReaderWriter cadCommandWriter;
   cadCommandWriter.UseStandaloneDOMDocumentManager();
   cadCommandWriter.WriteToString();
   
   std::pair<VE_XML::Command*,std::string> nodeTagPair;
   nodeTagPair.first = cadCommand;
   nodeTagPair.second = std::string("vecommand");
   std::vector< std::pair<VE_XML::XMLObject*,std::string> > nodeToWrite;
   nodeToWrite.push_back(nodeTagPair);

   cadCommandWriter.WriteXMLDocument(nodeToWrite,commandString,"Command");

   //std::cout << "----Sending Command----" << std::endl;
   //std::cout << commandString << std::endl;

   if ( !CORBA::is_nil( _vjObsPtr ) && !commandString.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         _vjObsPtr->SetCommandString( CORBA::string_dup( commandString.c_str() ) );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
      }
   }
   //Clean up memory
   delete cadCommand;
   ClearInstructions();
}
#endif
////////////////////////////////////////
CADNode* CADNodeManagerDlg::GetRootCADNode()
{
   return _rootNode;
}
