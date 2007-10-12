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
#include <ves/conductor/util/CADNodeManagerDlg.h>
#include <ves/conductor/util/CADNodeMenu.h>
#include <ves/conductor/util/CADTreeBuilder.h>
#include <ves/conductor/util/CADNodePropsDlg.h>
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAssembly.h>
#include <ves/open/xml/cad/CADPart.h>
#include <ves/open/xml/XMLReaderWriter.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Transform.h>

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/filename.h>
#include <wx/intl.h>

using namespace ves::conductor::util;
using namespace ves::open::xml::cad;
using namespace ves::open::xml;

BEGIN_EVENT_TABLE(CADNodeManagerDlg,wxDialog)
   EVT_TREE_ITEM_COLLAPSING(TREE_ID, CADNodeManagerDlg::_selectOnExpandCollapse )
   EVT_TREE_ITEM_EXPANDING(TREE_ID, CADNodeManagerDlg::_selectOnExpandCollapse )
   EVT_TREE_END_LABEL_EDIT(TREE_ID,CADNodeManagerDlg::_editLabel)
   EVT_TREE_SEL_CHANGED(TREE_ID,CADNodeManagerDlg::_setActiveNode)
   EVT_TREE_ITEM_RIGHT_CLICK(TREE_ID, CADNodeManagerDlg::_popupCADNodeManipulatorMenu)
   EVT_BUTTON(GEOM_SAVE,CADNodeManagerDlg::_saveCADFile)
   EVT_TREE_END_DRAG(TREE_ID, CADNodeManagerDlg::_onEndNodeMove)
   EVT_TREE_BEGIN_DRAG(TREE_ID, CADNodeManagerDlg::_onBeginNodeMove)
   EVT_MENU(CADNodeMenu::GEOM_PROPERTIES,CADNodeManagerDlg::_showPropertiesDialog)
   EVT_MENU(CADNodeMenu::GEOM_DELETE,CADNodeManagerDlg::_deleteNode)
   EVT_MENU(CADNodeMenu::GEOM_ASSEMBLY_CREATE,CADNodeManagerDlg::_createNewAssembly)
   EVT_MENU(CADNodeMenu::GEOM_VEG_FILE_ADD,CADNodeManagerDlg::_addNodeFromVEGFile)
   EVT_MENU(CADNodeMenu::GEOM_CAD_FILE_ADD,CADNodeManagerDlg::_addNodeFromCADFile)
   EVT_MENU(CADNodeMenu::GEOM_CLONE_ADD,CADNodeManagerDlg::_cloneNode)
   EVT_MENU(CADNodeMenu::GEOM_TOGGLE_ON,CADNodeManagerDlg::_toggleNode)
   EVT_MENU(CADNodeMenu::GEOM_TOGGLE_OFF,CADNodeManagerDlg::_toggleNode)
   EVT_MENU(CADNodeMenu::GEOM_INITIALIZE_PHYSICS,CADNodeManagerDlg::_initializePhysics)
END_EVENT_TABLE()

using namespace ves::conductor::util;

/////////////////////////////////////////////////////////////////////
//Constructor                                                      //
/////////////////////////////////////////////////////////////////////
CADNodeManagerDlg::CADNodeManagerDlg(CADNode* node, wxWindow* parent, wxWindowID id)
:wxDialog(parent,id,_("CADTree Manager"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),_("CADTree Manager"))
{
   _rootNode = 0;
   _propsDlg = 0;
   _cadTreeBuilder = 0;
   
   _quitButton = 0;
   _saveButton = 0;
   _commandName = std::string("CAD");
   _cloneFromSameFile = false;
   SetRootCADNode(node);
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

   if(_dataValuePairList.size())
   {
      _dataValuePairList.clear();
   }
   if(_loadedCAD.size())
   {
      _loadedCAD.clear();
   }
}
/////////////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::SetRootCADNode(CADNode* rootNode)
{
   if(rootNode)
   {
      _rootNode = rootNode;
   }
   else
   {
      _rootNode = new CADAssembly(std::string("Model Geometry"));
   }

   _commandName = std::string("SET_ROOT_CAD_NODE");

   ClearInstructions();
   ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
   //cadNode->SetDataType(std::string("XMLOBJECT"));
   cadNode->SetData("Root Node ID",_rootNode->GetID());
   _dataValuePairList.push_back(cadNode);
   _sendCommandsToXplorer();
   ClearInstructions();
   _ensureTree();
  
}
/////////////////////////////////////////////
void CADNodeManagerDlg::ClearLoadedCADFiles()
{
   _loadedCAD.clear();
}
/////////////////////////////////////
void CADNodeManagerDlg::_ensureTree()
{
   if(!_cadTreeBuilder)
   {
      _cadTreeBuilder =  new CADTreeBuilder(_rootNode,TREE_ID,this);
   }
   else
   {
      _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
      _cadTreeBuilder->SetRootNode(_rootNode);
   }
   _cadTreeBuilder->Traverse();
   _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();
   _expandNode(_geometryTree->GetRootItem());
   _activeTreeNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(_geometryTree->GetRootItem()));
   _activeCADNode = _rootNode;
}
////////////////////////////////////////////////////////
void CADNodeManagerDlg::_expandNode( wxTreeItemId node )
{
    if ( node != _geometryTree->GetRootItem() )
        _geometryTree->Expand( node );

        _geometryTree->SetItemImage(node, 2, wxTreeItemIcon_Expanded);
        _geometryTree->SetItemImage(node, 2, wxTreeItemIcon_SelectedExpanded);

    wxTreeItemIdValue cookie;
    wxTreeItemId child = _geometryTree->GetFirstChild(node, cookie);

    while ( child.IsOk() )
    {
        _expandNode( child );
        child = _geometryTree->GetNextChild(node, cookie);
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
  
   _quitButton = new wxButton(this,wxID_OK,_("Close"));
   _saveButton = new wxButton(this,GEOM_SAVE,_("Save As..."));

   treeSizer->Add(_geometryTree,1,wxALIGN_CENTER|wxEXPAND); 
   cadTreePropSizer->Add(treeSizer,1,wxALIGN_CENTER|wxEXPAND);
   _geometryTree->Raise();
   
   buttonRowSizer->Add(_saveButton,0,wxALIGN_CENTER);
   buttonRowSizer->Add(_quitButton,0,wxALIGN_CENTER);

   //mainSizer->Add(treeSizer, 1, wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(cadTreePropSizer, 1, wxALIGN_CENTER|wxEXPAND);
   mainSizer->Add(buttonRowSizer, 1, wxALIGN_CENTER);
   SetAutoLayout(true);
   SetSizer(mainSizer);
}
///////////////////////////////////////////////////////
void CADNodeManagerDlg::_selectOnExpandCollapse(wxTreeEvent& event)
{
   if( event.GetItem().IsOk() )
   {
      //_geometryTree->SelectItem( event.GetItem() );
   }
}
///////////////////////////////////////////////////////
void CADNodeManagerDlg::_editLabel(wxTreeEvent& event)
{
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(event.GetItem().IsOk()&& (!event.IsEditCancelled()))
   {
      cadNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(event.GetItem()));
      cadNode->GetNode()->SetNodeName( ConvertUnicode( event.GetLabel().GetData() ) );

      _commandName = std::string("CAD_SET_NODE_NAME");

      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),cadNode->GetNode()->GetID());
      _dataValuePairList.push_back(nodeID);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetData(std::string("Node Type"),cadNode->GetNode()->GetNodeType());
      _dataValuePairList.push_back(nodeType);

      ves::open::xml::DataValuePair* nodeName = new ves::open::xml::DataValuePair();
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
   event.Skip();
}
/////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_popupCADNodeManipulatorMenu(wxTreeEvent& event)
{
   wxTreeItemId item = event.GetItem();
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(item.IsOk())
   {
      _geometryTree->SelectItem(item);
      cadNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData( item ));

      CADNodeMenu* cadNodeMenu = new CADNodeMenu();
      cadNodeMenu->SetToggleNodeValue(cadNode->GetNode()->GetVisibility()/*_toggleNodeOnOff[cadNode->GetNode()->GetID()]*/);
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

            if( !cadNode->GetNode()->HasPhysics() )
            {
                cadNodeMenu->Insert( 4, CADNodeMenu::GEOM_INITIALIZE_PHYSICS, _T( "Initialize Physics" ), _T( "" ), wxITEM_NORMAL );
                cadNodeMenu->InsertSeparator( 5 );
            }
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
     //_toggleNodeOnOff[_rootNode->GetID()] = true;
     _rootNode->SetVisibility(true);
     SetRootCADNode(_rootNode);
     /*_activeCADNode = _rootNode;
     
     _cadTreeBuilder->SetRootNode(_rootNode);//,TREE_ID,this);
     _cadTreeBuilder->Traverse();
     _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();*/
   }
   else
   {

      if(_activeCADNode->GetNodeType() == std::string("Assembly"))
      {
         //pop a text dialog to enter the name of the new assembly
         wxTextEntryDialog assemblyNameDlg(this, 
                                       _("New Assembly Name"),
                                       _("Enter name for new assembly:"),
                                       _("Assembly"),wxOK);

         assemblyNameDlg.CentreOnParent();
         assemblyNameDlg.ShowModal();

         CADAssembly* newAssembly = new CADAssembly( ConvertUnicode(assemblyNameDlg.GetValue().GetData() ) );
         newAssembly->SetParent(_activeCADNode->GetID());
         //_toggleNodeOnOff[newAssembly->GetID()] = true;
         newAssembly->SetVisibility(true);
         dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newAssembly);
 

         _geometryTree->SetItemImage(_activeTreeNode->GetId(), 2, wxTreeItemIcon_Expanded);
         _geometryTree->SetItemImage(_activeTreeNode->GetId(), 2, wxTreeItemIcon_SelectedExpanded);

         _geometryTree->AppendItem(_activeTreeNode->GetId(),
                                 wxString(newAssembly->GetNodeName().c_str(),wxConvUTF8),
                                 0,2,new CADTreeBuilder::TreeNodeData(newAssembly));


         ClearInstructions();

         _commandName = std::string("CAD_ADD_NODE");

         ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
         cadNode->SetDataType(std::string("XMLOBJECT"));
         cadNode->SetData("New Node",newAssembly);
         _dataValuePairList.push_back(cadNode);

         ves::open::xml::DataValuePair* parentNode = new ves::open::xml::DataValuePair();
         parentNode->SetDataType(std::string("STRING"));
         parentNode->SetData(std::string("Parent ID"),_activeCADNode->GetID());
         _dataValuePairList.push_back(parentNode);

         _sendCommandsToXplorer();
         ClearInstructions();
      }
      else
      {
         wxMessageDialog errorDlg(this, _("Cannot add children to a Part node!!"),_("Error"));
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

      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),_activeCADNode->GetID());
      _dataValuePairList.push_back(nodeID);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetData(std::string("Node Type"),_activeCADNode->GetNodeType());
      _dataValuePairList.push_back(nodeType);

      ves::open::xml::DataValuePair* toggleValue = new ves::open::xml::DataValuePair();
      toggleValue->SetDataType("STRING");
      if(event.GetId() == CADNodeMenu::GEOM_TOGGLE_ON)
      {
         //std::cout<<"Toggle on!!"<<std::endl;
         toggleValue->SetData(std::string("Toggle Value"),std::string("ON"));
		   //_toggleNodeOnOff[_activeCADNode->GetID()] = true;
         _activeCADNode->SetVisibility(true);
      }
      else if(event.GetId() == CADNodeMenu::GEOM_TOGGLE_OFF)
      {
         //std::cout<<"Toggle off!!"<<std::endl;
         toggleValue->SetData(std::string("Toggle Value"),std::string("OFF"));
         //_toggleNodeOnOff[_activeCADNode->GetID()] = false;
         _activeCADNode->SetVisibility(false);
	  }
      _dataValuePairList.push_back(toggleValue);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
}
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::_initializePhysics( wxCommandEvent& event )
{
    if( _activeCADNode )
    {
        _commandName = "INITIALIZE_PHYSICS";

        ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
        nodeID->SetDataType( "STRING" );
        nodeID->SetData(std::string( "Node ID" ), _activeCADNode->GetID() );
        _dataValuePairList.push_back( nodeID );

        ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
        nodeType->SetDataType( "STRING" );
        nodeType->SetData(std::string( "Node Type" ), _activeCADNode->GetNodeType() );
        _dataValuePairList.push_back( nodeType );

        _activeCADNode->EnablePhysics();

        _sendCommandsToXplorer();
        ClearInstructions();
    }
}
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::_cloneNode(wxCommandEvent& WXUNUSED(event))
{
   if(_activeCADNode)
   {
      CADNode* newClone = 0;
      if(_activeCADNode->GetNodeType()== "Part")
      {
         newClone = new CADPart(*dynamic_cast<CADPart*>(_activeCADNode),true);
      }
      else if(_activeCADNode->GetNodeType()== "Assembly")
      {
         newClone = new CADAssembly(*dynamic_cast<CADAssembly*>(_activeCADNode),true);
      }
      else
      {
         return;
      }
      newClone->SetNodeName(_activeCADNode->GetNodeName()+std::string("_cloned"));

      wxTreeItemId parentID;
      if(_activeTreeNode->GetId() == _geometryTree->GetRootItem())
      {
         parentID = _geometryTree->GetRootItem();
      }
      else if(_cloneFromSameFile)
      {
         //need to set the parent from as the current active node
         parentID = _activeTreeNode->GetId();
      }
      else
      {
         parentID = _geometryTree->GetItemParent(_activeTreeNode->GetId());
      }
      CADTreeBuilder::TreeNodeData* parentCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(parentID));

     dynamic_cast<CADAssembly*>(parentCADNode->GetNode())->AddChild(newClone);

     if(newClone->GetNodeType() == std::string("Assembly"))
     {
        _geometryTree->AppendItem(parentID,
                               wxString(newClone->GetNodeName().c_str(), wxConvUTF8 )
                               ,0,2,new CADTreeBuilder::TreeNodeData(newClone));

        _geometryTree->SetItemImage(parentID, 2, wxTreeItemIcon_Expanded);
        _geometryTree->SetItemImage(parentID, 2, wxTreeItemIcon_SelectedExpanded);
        _ensureTree();
     }
     else if(newClone->GetNodeType() == std::string("Part"))
     {
        _geometryTree->AppendItem(parentID,
                               wxString(newClone->GetNodeName().c_str(), wxConvUTF8 )
                               ,0,1,new CADTreeBuilder::TreeNodeData(newClone));
     }
      _commandName = std::string("CAD_ADD_NODE");

      ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
      cadNode->SetDataType(std::string("XMLOBJECT"));
      cadNode->SetData("New Node",newClone);
      _dataValuePairList.push_back(cadNode);

      _sendCommandsToXplorer();
      ClearInstructions();
   }
   else
   {
       wxMessageBox( _("Error! Invalid node!!!"), 
                        _("CAD Clone Failure"), wxOK | wxICON_INFORMATION );
   }
}
////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeFromVEGFile(wxCommandEvent& WXUNUSED(event))
{
   wxFileDialog dialog(this,
                       _T("Open file"), 
                       ::wxGetCwd(), 
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
         if(!_ensureClones(fileNamesVector.Item(i)))
         {
            SendVEGNodesToXplorer( fileNamesVector.Item( i ) );
         }
      }
   }
   _expandNode(_activeTreeNode->GetId());
}
///////////////////////////////////////////////////////
bool CADNodeManagerDlg::_ensureClones(wxString filename)
{
	std::map<wxString,ves::open::xml::cad::CADNode*>::iterator loadedFile;
   loadedFile = _loadedCAD.find(filename);
   if(loadedFile != _loadedCAD.end())
   {
      //Clone this node
      //Just need to set the node we are going to clone
      //and call the cloneNode event
	   ves::open::xml::cad::CADNode* originalActiveNode = _activeCADNode;
      ///need to keep track of the parent node
      if(originalActiveNode->GetNodeType() == "Assembly")
      {
         _cloneFromSameFile = true;
      }
      _activeCADNode = loadedFile->second;
      wxCommandEvent emptyEvent;
      _cloneNode(emptyEvent);
      //reset our temp pointers
      _activeCADNode = originalActiveNode;
      _cloneFromSameFile = false;
      return true;
   }
   return false;
}
//////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::SendVEGNodesToXplorer( wxString fileName )
{
   ves::open::xml::XMLReaderWriter cadReader;
   cadReader.UseStandaloneDOMDocumentManager();
   cadReader.ReadFromFile();
   cadReader.ReadXMLData( ConvertUnicode( fileName.c_str() ),"CAD","CADAssembly");

   //CADNode* loadedNode = 0;
   CADAssembly* newAssembly = 0;
   CADPart* newPart = 0;
   std::vector<ves::open::xml::XMLObject*> loadedNodes;
   loadedNodes = cadReader.GetLoadedXMLObjects();
   
   if(loadedNodes.size())
   {
      //std::cout<<"---Loaded Assembly---"<<std::endl;
      newAssembly = new CADAssembly(*dynamic_cast<CADAssembly*>(loadedNodes.at(0)));
      //_toggleNodeOnOff[newAssembly->GetID()] = true;
      //newAssembly->SetVisibility(true);
   }
   else
   {
      cadReader.ReadXMLData( ConvertUnicode( fileName.c_str() ),"CAD","CADPart");
      loadedNodes = cadReader.GetLoadedXMLObjects();
      if(loadedNodes.size())
      {
         //std::cout<<"---Loaded Part---"<<std::endl;
         newPart = new CADPart(*dynamic_cast<CADPart*>(loadedNodes.at(0)));
         //_toggleNodeOnOff[newPart->GetID()] = true;
         //newPart->SetVisibility(true);
      }
   }
   if(newAssembly || newPart)
   {
      //std::cout<<"Number of children on current root: "<<dynamic_cast<CADAssembly*>(_rootNode)->GetNumberOfChildren()<<std::endl;
      if(newAssembly)
      {
         _loadedCAD[fileName] = newAssembly;
         if(dynamic_cast<CADAssembly*>(_rootNode)->GetNumberOfChildren() == 0)
         {
            //std::cout<<"Reseting root CADNode"<<std::endl;
            SetRootCADNode(newAssembly);
         }
         else
         {
            dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newAssembly);
            SetRootCADNode(_rootNode);
         }
      }
      else if(newPart)
      {
         _loadedCAD[fileName] = newPart;
         dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newPart);
         SetRootCADNode(_rootNode);
      }
      _commandName = "CAD_ADD_NODE";
      ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
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
                       ::wxGetCwd(), 
                       _T(""),
                       _T("All Supported Files (*.osg;*.ive;*.stl;*.wrl;*.iv;*.obj;*.pfb;*.flt;*.dxf;*.3ds)|*.osg;*.ive;*.stl;*.wrl;*.iv;*.obj;*.pfb;*.flt;*.dxf;*.3ds|OSG files (*.osg;*.ive)|*.osg;*.ive;|STL files (*.stl)|*.stl;|VRML/Inventor files (*.wrl;*.iv)|*.wrl;*.iv;|OBJ files (*.obj)|*.obj;|Performer Binary files (*.pfb)|*.pfb;|Flight files (*.flt)|*.flt;|DXF files (*.dxf)|*.dxf;|3DS files (*.3ds)|*.3ds;|All Files (*.*)|*.*"),
                       //"BMP and GIF files (*.bmp;*.gif)|*.bmp;*.gif|PNG files (*.png)|*.png"
                       wxOPEN|wxFILE_MUST_EXIST|wxMULTIPLE,
                       wxDefaultPosition);
                       
   if (dialog.ShowModal() == wxID_OK)
   {
      wxArrayString fileNamesVector;
      dialog.GetPaths( fileNamesVector );
      for ( size_t i = 0; i < fileNamesVector.GetCount(); ++i )
      {
         if(!_ensureClones(fileNamesVector.Item(i)))
         {
            SendNewNodesToXplorer( fileNamesVector.Item( i ) );
         }
      }
   }
   _expandNode(_activeTreeNode->GetId());
}
////////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::SendNewNodesToXplorer( wxString fileName )
{
   ClearInstructions();
   wxFileName vegFileName( fileName);
   vegFileName.MakeRelativeTo( ::wxGetCwd());
   wxString vegFileNamePath( vegFileName.GetFullPath() );
   vegFileNamePath.Replace( _("\\"), _("/"), true );
   wxFileName cadFileName( vegFileNamePath.c_str());

   //pop a text dialog to enter the name of the new assembly
   wxTextEntryDialog partNameDlg(this, 
                        _("New Part Name"),
                        _("Enter name for new part:"),
                        cadFileName.GetName(),wxOK);

   partNameDlg.CentreOnParent();
   partNameDlg.ShowModal();

   CADPart* newCADPart = new CADPart( ConvertUnicode( partNameDlg.GetValue().GetData() ) );
   newCADPart->SetCADFileName( ConvertUnicode( vegFileNamePath.c_str() ) );
   //_toggleNodeOnOff[newCADPart->GetID()] = true;
   newCADPart->SetVisibility(true);

   dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newCADPart);

   _geometryTree->AppendItem(_activeTreeNode->GetId(),wxString(newCADPart->GetNodeName().c_str(), wxConvUTF8),
                                             0,1,new CADTreeBuilder::TreeNodeData(newCADPart));
   _commandName = std::string("CAD_ADD_NODE");

   newCADPart->SetParent(_activeCADNode->GetID());
   _loadedCAD[fileName] = newCADPart;
   ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
   cadNode->SetDataType(std::string("XMLOBJECT"));
   cadNode->SetData("New Node",newCADPart);
   _dataValuePairList.push_back(cadNode);

   _sendCommandsToXplorer();
   ClearInstructions();
}
/////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_saveCADFile(wxCommandEvent& WXUNUSED(event))
{
   wxFileName vegFileName;
   int answer = 0;
   do
   {
      wxTextEntryDialog newDataSetName(this, 
                                       _("Enter the prefix for *.veg filename:"),
                                       _("Save VEG file as..."),
                                       _("geometry"),wxOK|wxCANCEL);

      if ( newDataSetName.ShowModal() == wxID_OK )
      {
         vegFileName.ClearExt();
         vegFileName.SetName( newDataSetName.GetValue() ); 
         vegFileName.SetExt( wxString( "veg", wxConvUTF8 ) );
      }
      else
      {
         break;
      }

      if ( vegFileName.FileExists() )
      {
         wxString tempMessage = _("Do you want to replace ") + vegFileName.GetFullName() + _("?");
         wxMessageDialog promptDlg( this, 
                                    tempMessage, 
                                    _("Overwrite File Warning"), 
                                    wxYES_NO|wxNO_DEFAULT|wxICON_QUESTION, 
                                    wxDefaultPosition);
         answer = promptDlg.ShowModal();
      }
   }
   while ( answer == wxID_NO );
   
   if ( vegFileName.HasName() ) 
   {
      ves::open::xml::XMLReaderWriter cadReader;
      cadReader.UseStandaloneDOMDocumentManager();
        
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

      std::string outputFile = ConvertUnicode( vegFileName.GetFullPath( wxPATH_NATIVE ).c_str() );

      std::pair<CADNode*,std::string> nodeTagPair;
      nodeTagPair.first = rootCADNode->GetNode();
      nodeTagPair.second = tagName;
      std::vector< std::pair<ves::open::xml::XMLObject*,std::string> > nodeToWrite;
      nodeToWrite.push_back(nodeTagPair);
    
      cadReader.WriteXMLDocument(nodeToWrite, outputFile, "Command");
   }
}
////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_showPropertiesDialog(wxCommandEvent& WXUNUSED(event))
{
   if(_activeCADNode)
   {
      CADNodePropertiesDlg propsDlg(this,PROPERTY_ID,_activeCADNode);
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
       CADNode* parentCADNode = 
                dynamic_cast<CADTreeBuilder::TreeNodeData*>
                            (_geometryTree->GetItemData(_geometryTree->GetItemParent(_activeTreeNode->GetId())))->GetNode();
       _activeCADNode = _activeTreeNode->GetNode();

       _commandName = std::string("CAD_DELETE_NODE");

       ves::open::xml::DataValuePair* deleteNode = new ves::open::xml::DataValuePair();
       deleteNode->SetDataType("STRING");
       deleteNode->SetData(std::string("Node Type"),_activeCADNode->GetNodeType());
       _dataValuePairList.push_back(deleteNode);

       ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
       nodeID->SetDataType("STRING");
       nodeID->SetData(std::string("Node ID"),_activeCADNode->GetID());
       _dataValuePairList.push_back(nodeID);

       ves::open::xml::DataValuePair* parentNode = new ves::open::xml::DataValuePair();
       parentNode->SetDataType(std::string("STRING"));
       parentNode->SetData(std::string("Parent ID"),_activeCADNode->GetParent());
       _dataValuePairList.push_back(parentNode);

       _sendCommandsToXplorer();
       ClearInstructions();
       
       dynamic_cast<CADAssembly*>(parentCADNode)->RemoveChild(_activeCADNode->GetID());
       
       for(std::map<wxString,ves::open::xml::cad::CADNode* >::iterator deletedNode = _loadedCAD.begin();
          deletedNode != _loadedCAD.end();
          ++deletedNode)
       {
          if(deletedNode->second == _activeCADNode)
          {
             _loadedCAD.erase(deletedNode);
             break;
          }
       }
       
       _geometryTree->Delete(_activeTreeNode->GetId()); 
	    //_ensureTree();
    }
}/////////////////////////////////////////////////////
void CADNodeManagerDlg::_onBeginNodeMove(wxTreeEvent& event)
{
    if ( event.GetItem() != _geometryTree->GetRootItem() )
    {
       m_movingNode = event.GetItem();

       CADTreeBuilder::TreeNodeData* movingCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>
             (_geometryTree->GetItemData(m_movingNode));

        _activeCADNode = movingCADNode->GetNode();
        wxPoint clientpt = event.GetPoint();
        wxPoint screenpt = ClientToScreen(clientpt);
        m_movingNodeType = _activeCADNode->GetNodeType();
        m_movingNodeName = _activeCADNode->GetNodeName();
        
        event.Allow();
    }
}

/////////////////////////////////////////////////////
void CADNodeManagerDlg::_onEndNodeMove(wxTreeEvent& event)
{
    wxTreeItemId newParentNode = event.GetItem();
    wxTreeItemId oldParentNode = _geometryTree->GetItemParent(m_movingNode);
    CADTreeBuilder::TreeNodeData* movingCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>
             (_geometryTree->GetItemData(m_movingNode));

    CADTreeBuilder::TreeNodeData* newParentCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>
             (_geometryTree->GetItemData(newParentNode));

    if ( !newParentNode.IsOk()||
        ( newParentCADNode->GetNode()->GetNodeType() != "Assembly" ) )
    {
        return;
    }
    //Move the node from it's old parent and add it to the new one
    _moveNodeToNewParent( movingCADNode->GetNode( ),
                          oldParentNode,
                          newParentNode);

    ///ensure that the parent node has an assembly icon
    _geometryTree->SetItemImage(newParentNode, 2, wxTreeItemIcon_Expanded);
    _geometryTree->SetItemImage(newParentNode, 2, wxTreeItemIcon_SelectedExpanded);
}
/////////////////////////////////////////////////////////////////////////// `
void CADNodeManagerDlg::_moveNodeToNewParent( ves::open::xml::cad::CADNode* movingChild,
                                              wxTreeItemId oldParentTreeID,
                                              wxTreeItemId newParentTreeID)
{
    //The CADAssembly representing the new parent
    CADTreeBuilder::TreeNodeData* newParentCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>
             (_geometryTree->GetItemData(newParentTreeID));

    CADAssembly* newParent = 
        dynamic_cast<CADAssembly*>( newParentCADNode->GetNode( ) );

    _commandName = std::string("CAD_MOVE_NODE");

    ves::open::xml::DataValuePair* deleteNode = new ves::open::xml::DataValuePair();
    deleteNode->SetDataType("STRING");
    deleteNode->SetData(std::string("Move Node Type"),movingChild->GetNodeType());
    _dataValuePairList.push_back(deleteNode);

    ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
    nodeID->SetDataType("STRING");
    nodeID->SetData(std::string("Move Node ID"),movingChild->GetID());
    _dataValuePairList.push_back(nodeID);

    ves::open::xml::DataValuePair* oldParentNode = new ves::open::xml::DataValuePair();
    oldParentNode->SetDataType(std::string("STRING"));
    oldParentNode->SetData(std::string("Old Parent ID"),movingChild->GetParent());
    _dataValuePairList.push_back( oldParentNode );

    ves::open::xml::DataValuePair* newParentNode = new ves::open::xml::DataValuePair();
    newParentNode ->SetDataType(std::string("STRING"));
    newParentNode ->SetData(std::string("New Parent ID"),newParent->GetID());
    _dataValuePairList.push_back( newParentNode );

    _sendCommandsToXplorer();
    ClearInstructions();

    //update the xml of the old parent
    CADTreeBuilder::TreeNodeData* oldParentTreeNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>
             (_geometryTree->GetItemData(oldParentTreeID));

    dynamic_cast<CADAssembly*>
        (oldParentTreeNode->GetNode())->RemoveChild(movingChild->GetID());
    
    newParent->AddChild( movingChild );

   _geometryTree->AppendItem(newParentTreeID,
                             wxString(movingChild->GetNodeName().c_str(), wxConvUTF8),
                             0, ( (movingChild->GetNodeType() == "Assembly" )?2:1),
                             new CADTreeBuilder::TreeNodeData(movingChild));
    //Remove the old reference in the tree
    //Not sure if this should this be passed in 
    //to make the function more generic???????
    _geometryTree->Delete( m_movingNode );
}
///////////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeToParent(ves::open::xml::cad::CADAssembly* parent,
										 ves::open::xml::cad::CADNode* childToAdd,
                                         wxTreeItemId parentTreeID)
{
    parent->AddChild( childToAdd );

   _geometryTree->AppendItem(parentTreeID,
                             wxString(childToAdd->GetNodeName().c_str(), wxConvUTF8),
                             0, ( (childToAdd->GetNodeType() == "Assembly" )?2:1),
                             new CADTreeBuilder::TreeNodeData(childToAdd));

   _commandName = std::string("CAD_ADD_NODE");

   childToAdd->SetParent( parent->GetID( ) );

   ves::open::xml::DataValuePair* cadNode = new ves::open::xml::DataValuePair();
   cadNode->SetDataType( std::string( "XMLOBJECT" ) );
   cadNode->SetData("New Node", childToAdd );
   _dataValuePairList.push_back( cadNode );

   _sendCommandsToXplorer( );
   ClearInstructions( );
}
////////////////////////////////////////////////
void CADNodeManagerDlg::_sendCommandsToXplorer()
{
   ves::open::xml::Command* cadCommand = new ves::open::xml::Command();

   for(size_t i =0; i < _dataValuePairList.size(); i++)
   {
      cadCommand->AddDataValuePair(_dataValuePairList.at(i));
   }
   cadCommand->SetCommandName(_commandName);
   
   try
   {
	   ves::conductor::util::CORBAServiceList::instance()->SendCommandStringToXplorer(cadCommand);
   }
   catch ( ... )
   {
      wxMessageBox( _("Send data to VE-Xplorer failed. Probably need to disconnect and reconnect."), 
                     _("Communication Failure"), wxOK | wxICON_INFORMATION );
   }

   //Clean up memory
   delete cadCommand;
   ClearInstructions();
}
////////////////////////////////////////
CADNode* CADNodeManagerDlg::GetRootCADNode()
{
   return _rootNode;
}
