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
#include "VE_Conductor/Framework/CADNodeManagerDlg.h"
#include "VE_Conductor/Framework/CADNodeMenu.h"
#include "VE_Conductor/Utilities/CADTreeBuilder.h"
#include "VE_Conductor/Framework/CADNodePropsDlg.h"

#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAssembly.h"
#include "VE_Open/XML/CAD/CADPart.h"
#include "VE_Open/XML/CAD/CADClone.h"
#include "VE_Open/XML/CAD/CADXMLReaderWriter.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

#include <wx/sizer.h>
#include <wx/statbox.h>
#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
using namespace VE_Conductor::GUI_Utilities;
using namespace VE_CAD;

BEGIN_EVENT_TABLE(CADNodeManagerDlg,wxDialog)
   EVT_TREE_END_LABEL_EDIT(TREE_ID,CADNodeManagerDlg::_editLabel)
   EVT_TREE_SEL_CHANGED(TREE_ID,CADNodeManagerDlg::_setActiveNode)
   EVT_TREE_ITEM_MENU(TREE_ID,CADNodeManagerDlg::_popupCADNodeManipulatorMenu)
   EVT_BUTTON(GEOM_SAVE,CADNodeManagerDlg::_saveCADFile)
   EVT_MENU(CADNodeMenu::GEOM_PROPERTIES,CADNodeManagerDlg::_showPropertiesDialog)
   EVT_MENU(CADNodeMenu::GEOM_DELETE,CADNodeManagerDlg::_deleteNode)
   EVT_MENU(CADNodeMenu::GEOM_ASSEMBLY_CREATE,CADNodeManagerDlg::_createNewAssembly)
   EVT_MENU(CADNodeMenu::GEOM_VEG_FILE_ADD,CADNodeManagerDlg::_addNodeFromVEGFile)
   EVT_MENU(CADNodeMenu::GEOM_CAD_FILE_ADD,CADNodeManagerDlg::_addNodeFromCADFile)
   EVT_MENU(CADNodeMenu::GEOM_CLONE_ADD,CADNodeManagerDlg::_cloneNode)
END_EVENT_TABLE()


/////////////////////////////////////////////////////////////////////
//Constructor                                                      //
/////////////////////////////////////////////////////////////////////
CADNodeManagerDlg::CADNodeManagerDlg(/*VjObs_ptr*/CADNode* node, wxWindow* parent, 
                       wxWindowID id)
:wxDialog(parent,id,wxString("CADTree Manager"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),wxString("CADTree Manager"))
{
   SetRootCADNode(node);

   _cadTreeBuilder =  new CADTreeBuilder(_rootNode,TREE_ID,this);
   _cadTreeBuilder->Traverse();
   _geometryTree = _cadTreeBuilder->GetWXTreeCtrl();

   _quitButton = 0;
   _saveButton = 0;

   _activeTreeNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(_geometryTree->GetRootItem()));
   _activeCADNode = _rootNode;

   _buildDialog();
}
/////////////////////////////////////////
CADNodeManagerDlg::~CADNodeManagerDlg()
{
   if(_rootNode)
   {
      delete _rootNode;
      _rootNode;
   }
   if(_cadTreeBuilder)
   {
      delete _cadTreeBuilder;
      _cadTreeBuilder = 0;
   }

}
#ifndef STAND_ALONE
//////////////////////////////////////////////////////////
void CADNodeManagerDlg::SetVjObsPtr(VjObs_var xplorerCom)
{
   _vjObsPtr = xplorerCom.ptr();
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
   }
}
///////////////////////////////////////////////////////////
void CADNodeManagerDlg::_setActiveNode(wxTreeEvent& event)
{
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(event.GetItem().IsOk())
   {
      _activeTreeNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(event.GetItem()));
      _activeCADNode = _activeTreeNode->GetNode();
   }
}
/////////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_popupCADNodeManipulatorMenu(wxTreeEvent& event)
{
   CADTreeBuilder::TreeNodeData* cadNode = 0;
   if(event.GetItem().IsOk())
   {
      cadNode = dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(event.GetItem()));
      
      CADNodeMenu* cadNodeMenu = new CADNodeMenu();
      if(cadNode)
      {
         //cadNodeMenu->EnableCreateMenu(false);
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
      if(cadNode->GetNode() == _rootNode)
      {
         cadNodeMenu->EnableCloneMenu(false);
      }
      PopupMenu(cadNodeMenu);
   }
}
/////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_createNewAssembly(wxCommandEvent& event)
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
                                       wxString("Enter name for new assembly:"));
         assemblyNameDlg.ShowModal();
         CADAssembly* newAssembly = new CADAssembly((assemblyNameDlg.GetValue().GetData()));
         newAssembly->SetParent(_activeCADNode->GetID());
         dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newAssembly);
 
         _geometryTree->AppendItem(_activeTreeNode->GetId(),
                                 wxString(newAssembly->GetNodeName().c_str()),
                                 2,4,new CADTreeBuilder::TreeNodeData(newAssembly)); 
      }
      else
      {
         wxMessageDialog errorDlg(this, wxString("Cannot add children to a Part node!!"),wxString("Error"));
         errorDlg.ShowModal();
      }

   }
}
/////////////////////////////////////////////////////////
void CADNodeManagerDlg::_cloneNode(wxCommandEvent& event)
{
   if(_activeCADNode)
   {
      CADClone* newClone = new CADClone(_activeCADNode->GetNodeName(),_activeCADNode);

      wxTreeItemId parentID = _geometryTree->GetItemParent(_activeTreeNode->GetId());

      CADTreeBuilder::TreeNodeData* parentCADNode = 
         dynamic_cast<CADTreeBuilder::TreeNodeData*>(_geometryTree->GetItemData(parentID));

      dynamic_cast<CADAssembly*>(parentCADNode->GetNode())->AddChild(newClone);

      _cadTreeBuilder->SetRootNode(_rootNode);
      _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
      _cadTreeBuilder->Traverse();
   }
}
///////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeFromVEGFile(wxCommandEvent& event)
{
   wxFileDialog dialog(this,
		       _T("Open file"), 
		       _T(""), 
		       _T(""),
		       _T("VE-Geometry files (*.veg)|*.veg;"),
		       wxOPEN); 
    if (dialog.ShowModal() == wxID_OK) {
       if ((!dialog.GetPath().IsEmpty()) 
           && wxFileExists(dialog.GetPath())) 
        {         
           if(dialog.GetPath().find(".veg") != -1)
           {
              VE_CAD::CADXMLReaderWriter cadReader;
              cadReader.UseStandaloneDOMDocumentManager();
              cadReader.ReadFromFile();
              cadReader.ReadXMLData(std::string(dialog.GetPath()));
              CADNode* loadedNode = cadReader.GetRootNode();

              dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(loadedNode);

              _cadTreeBuilder->SetRootNode(_rootNode);
              _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
              _cadTreeBuilder->Traverse();
           }      
        }
    }
}
///////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_addNodeFromCADFile(wxCommandEvent& event)
{
   wxFileDialog dialog(this,
		       _T("Open file"), 
		       _T(""), 
		       _T(""),
		       _T("CAD files (*.*)|*.*;"),
		       wxOPEN); 
    if (dialog.ShowModal() == wxID_OK) {
       if ((!dialog.GetPath().IsEmpty()) 
           && wxFileExists(dialog.GetPath())) 
        {         
           if(dialog.GetPath())
           {
              //pop a text dialog to enter the name of the new assembly
              wxTextEntryDialog partNameDlg(this, 
                                       wxString("New Part Name"),
                                       wxString("Enter name for new part:"));
              partNameDlg.ShowModal();
              
              CADPart* newCADPart = new CADPart(partNameDlg.GetValue().GetData());
              newCADPart->SetCADFileName(dialog.GetPath().c_str());

              dynamic_cast<CADAssembly*>(_activeCADNode)->AddChild(newCADPart);

              _cadTreeBuilder->SetRootNode(_rootNode);
              _cadTreeBuilder->GetWXTreeCtrl()->DeleteAllItems();
              _cadTreeBuilder->Traverse();
           }      
        }
    }
}
///////////////////////////////////////////////////////////
void CADNodeManagerDlg::_saveCADFile(wxCommandEvent& event)
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
         if(dialog.GetPath().find(".veg") != -1)
         {
            VE_CAD::CADXMLReaderWriter cadReader;
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
            cadReader.WriteXMLDocument(rootCADNode->GetNode(), outputFile, tagName );
         }
      }
   }
}
////////////////////////////////////////////////////////////////////
void CADNodeManagerDlg::_showPropertiesDialog(wxCommandEvent& event)
{
   if(_activeTreeNode)
   {
      CADNodePropertiesDlg propsDlg(this,PROPERTY_ID,_activeCADNode);
#ifndef STAND_ALONE
      propsDlg.SetVjObsPtr(_vjObsPtr.in());
#endif
      propsDlg.ShowModal();
   }
}
///////////////////////////////////////////////////////////
void CADNodeManagerDlg::_deleteNode(wxCommandEvent& event)
{
    CADTreeBuilder::TreeNodeData* cadNode = 0;
    //don't allow the user to delete the root node!!!
    if(_activeCADNode == _rootNode)
    {
       std::cout<<"Error!!!!"<<std::endl;
       std::cout<<"Can't delete root node!!!"<<std::endl;
       return;
    }
    if(_activeTreeNode)
    {
       if(_activeTreeNode->GetNode()->GetNodeType() == std::string("Assembly"))
       {
         //delete the node
         _geometryTree->Delete(_activeTreeNode->GetId());
       }
       else
       {
          //delete the node
          _geometryTree->Delete(_activeTreeNode->GetId());
         
          if(_activeCADNode)
          {
             //parent->RemoveChild(_activeCADNode);
          }
       }
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

   std::string commandString("returnString");
   VE_CAD::CADXMLReaderWriter cadCommandWriter;
   cadCommandWriter.UseStandaloneDOMDocumentManager();
   cadCommandWriter.WriteToString();
   cadCommandWriter.WriteXMLDocument(cadCommand,commandString,std::string("vecommands"));

   char* tempDoc = new char[ commandString.size() + 1 ];
   tempDoc = CORBA::string_dup( commandString.c_str() );

   if ( !CORBA::is_nil( _vjObsPtr ) && !commandString.empty() )
   {
      try
      {
         // CORBA releases the allocated memory so we do not have to
         _vjObsPtr->SetCommandString( tempDoc );
      }
      catch ( ... )
      {
         wxMessageBox( "Send data to VE-Xplorer failed. Probably need to disconnect and reconnect.", 
                        "Communication Failure", wxOK | wxICON_INFORMATION );
         delete [] tempDoc;
      }
   }
   else
   {
      delete [] tempDoc;
   }
   //Clean up memory
   delete cadCommand;
}
#endif
