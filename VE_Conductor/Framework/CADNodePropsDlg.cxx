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

#include "VE_Conductor/Framework/CADNodePropsDlg.h"

#include <wx/sizer.h>
#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/spinctrl.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/filedlg.h>
#include <wx/panel.h>
#include <wx/combobox.h>
#include <wx/listbox.h>
#include <wx/arrstr.h>
#include <wx/listbox.h>
#include <wx/msgdlg.h>

#include <iostream>
#include "VE_Builder/Utilities/gui/spinctld.h"
#include "VE_Open/XML/CAD/CADNode.h"
#include "VE_Open/XML/CAD/CADAttribute.h"
#include "VE_Open/XML/CAD/CADXMLReaderWriter.h"
#include "VE_Open/XML/Transform.h"
#include "VE_Open/XML/FloatArray.h"
#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

using namespace VE_CAD;

BEGIN_EVENT_TABLE(CADNodePropertiesDlg,wxDialog)
   EVT_SPINCTRL(TRANSFORM_PANEL_ID,CADNodePropertiesDlg::_updateTransform)
   EVT_COMBOBOX(ATTRIBUTE_TYPE,CADNodePropertiesDlg::_updateAttributeType)
   EVT_LISTBOX(ACTIVE_ATTRIBUTE,CADNodePropertiesDlg::_setActiveAttribute)
END_EVENT_TABLE()
////////////////////////////////////////////////////
//Here is the constructor with passed in pointers //
////////////////////////////////////////////////////
CADNodePropertiesDlg::CADNodePropertiesDlg (wxWindow* parent,
                                       int id,CADNode* activeNode)

:wxDialog((wxWindow *) parent, id, "CAD Properties",wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),wxString("CADTree Properties"))
{
   if(activeNode)
   {
      _cadNode = activeNode;
   }
   else
   {
      _cadNode = 0;
   }
   
   _propertyTabs = 0;
   _transformPanel = 0;
   _attributePanel = 0;
   _attributeType = 0;
   _attributeSelection = 0;
   _addAttributeButton = 0;
   _removeAttributeButton = 0;
   
   _buildGUI();
}

/////////////////////////////////////////////////////
CADNodePropertiesDlg::~CADNodePropertiesDlg()
{
}
#ifndef STAND_ALONE
//////////////////////////////////////////////////////////
void CADNodePropertiesDlg::SetVjObsPtr(VjObs_ptr xplorerCom)
{
   _vjObsPtr = xplorerCom;
}
#endif
///////////////////////////////////
void CADNodePropertiesDlg::_buildGUI()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* notebookSizer = new wxBoxSizer(wxVERTICAL);
   //wxBoxSizer* bottomRow = new wxBoxSizer(wxHORIZONTAL);

   _buildTabs();
   notebookSizer->Add(_propertyTabs,2,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);


   mainSizer->Add(notebookSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //set this flag and let wx handle alignment  
   SetAutoLayout(true);

   //assign the group to the panel              
   SetSizer(mainSizer);
   mainSizer->Fit(this); 
}
////////////////////////////////////
void CADNodePropertiesDlg::_buildTabs()
{
   if(!_propertyTabs)
   {
      _propertyTabs = new wxNotebook(this,-1);
   }

   _propertyTabs->AddPage(GetTransformPanel(),_T("Transform"), true);
   _propertyTabs->AddPage(GetAttributePanel(),_T("Attributes"), false);
}
//////////////////////////////////////////////////
wxPanel* CADNodePropertiesDlg::GetTransformPanel()
{
   if(!_transformPanel)
   {
      _buildTransformPanel();
   }
   return _transformPanel;
}
//////////////////////////////////////////////////
wxPanel* CADNodePropertiesDlg::GetAttributePanel()
{
   if(!_attributePanel)
   {
      _buildAttributePanel();
   }
   return _attributePanel;
}
//////////////////////////////////////////////////
/*wxPanel* CADNodePropertiesDlg::GetAttributePanel()
{
   if(!_firePanel)
   {
      _buildFirePanel();
   }
   return _firePanel;
}*/
///////////////////////////////////////////////////
void CADNodePropertiesDlg::_buildTransformPanel()
{
   _transformPanel = new wxPanel(_propertyTabs,TRANSFORM_PANEL_ID);

   wxBoxSizer* transformPanelSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* transformProperties = new wxStaticBox(_transformPanel, -1, wxT("CADNode Transform Properties"));
   wxStaticBoxSizer* transformPropSizer = new wxStaticBoxSizer(transformProperties, wxVERTICAL);


   ///translation
   wxStaticBox* translation = new wxStaticBox(_transformPanel, -1, wxT("Translation "));
   wxStaticBoxSizer* transSizer = new wxStaticBoxSizer(translation, wxHORIZONTAL);
   _xTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _xTransformCtrl->SetValue(0);
   _xTransformCtrl->SetRange(-1000.0,1000.0);
   _xTransformCtrl->SetIncrement(1.0);

   _yTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _yTransformCtrl->SetValue(0);
   _yTransformCtrl->SetRange(-1000.0,1000.0);
   _yTransformCtrl->SetIncrement(1.0);

   _zTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _zTransformCtrl->SetValue(0);
   _zTransformCtrl->SetRange(-1000.0,1000.0);
   _zTransformCtrl->SetIncrement(1.0);

   transSizer->Add(_xTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   transSizer->Add(_yTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   transSizer->Add(_zTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);

   transformPropSizer->Add(transSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //rotation
   wxStaticBox* rotation = new wxStaticBox(_transformPanel, -1, wxT("Rotation"));
   wxStaticBoxSizer* rotationSizer = new wxStaticBoxSizer(rotation, wxHORIZONTAL);
   _xRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _xRotationCtrl->SetValue(0);
   _xRotationCtrl->SetRange(0.0,360.0);
   _xRotationCtrl->SetIncrement(1.0);

   _yRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _yRotationCtrl->SetValue(0);
   _yRotationCtrl->SetRange(0.0,360.0);
   _yRotationCtrl->SetIncrement(1.0);

   _zRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _zRotationCtrl->SetValue(0);
   _zRotationCtrl->SetRange(0.0,360.0);
   _zRotationCtrl->SetIncrement(1.0);

   rotationSizer->Add(_xRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   rotationSizer->Add(_yRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   rotationSizer->Add(_zRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);

   transformPropSizer->Add(rotationSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //scale
   wxStaticBox* scale = new wxStaticBox(_transformPanel, -1, wxT("Scale "));
   wxStaticBoxSizer* scaleSizer = new wxStaticBoxSizer(scale, wxHORIZONTAL);
   _xScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _yScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _zScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   
   _xScaleCtrl->SetValue(1.0);
   _xScaleCtrl->SetRange(0.0,100.0);
   _xScaleCtrl->SetIncrement(1.0);

   _yScaleCtrl->SetValue(1.0);
   _yScaleCtrl->SetRange(0.0,100.0);
   _yScaleCtrl->SetIncrement(1.0);

   _zScaleCtrl->SetValue(1.0);
   _zScaleCtrl->SetRange(0.0,100.0);
   _zScaleCtrl->SetIncrement(1.0);

   scaleSizer->Add(_xScaleCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   scaleSizer->Add(_yScaleCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   scaleSizer->Add(_zScaleCtrl,1,wxALIGN_CENTER_HORIZONTAL);

   if(_cadNode)
   {
      _xTransformCtrl->SetValue(_cadNode->GetTransform()->GetTranslationArray()->GetElement(0));
      _yTransformCtrl->SetValue(_cadNode->GetTransform()->GetTranslationArray()->GetElement(1));
      _zTransformCtrl->SetValue(_cadNode->GetTransform()->GetTranslationArray()->GetElement(2));
   
      _xScaleCtrl->SetValue(_cadNode->GetTransform()->GetScaleArray()->GetElement(0));
      _yScaleCtrl->SetValue(_cadNode->GetTransform()->GetScaleArray()->GetElement(1));
      _zScaleCtrl->SetValue(_cadNode->GetTransform()->GetScaleArray()->GetElement(2));
   
      _xRotationCtrl->SetValue(_cadNode->GetTransform()->GetRotationArray()->GetElement(0));
      _yRotationCtrl->SetValue(_cadNode->GetTransform()->GetRotationArray()->GetElement(1));
      _zRotationCtrl->SetValue(_cadNode->GetTransform()->GetRotationArray()->GetElement(2));
   }
   
   transformPropSizer->Add(scaleSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   transformPanelSizer->Add(transformPropSizer,1,wxEXPAND|wxALIGN_CENTER);
   _transformPanel->SetAutoLayout(true);
   _transformPanel->SetSizer(transformPanelSizer);
}
///////////////////////////////////////////////////
void CADNodePropertiesDlg::_buildAttributePanel()
{
   _attributePanel = new wxPanel(_propertyTabs,ATTRIBUTE_PANEL_ID);

   wxBoxSizer* attributePanelSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* attributeProperties = new wxStaticBox(_attributePanel, -1, wxT("CADNode Attributes"));
   wxStaticBoxSizer* attributePropSizer = new wxStaticBoxSizer(attributeProperties, wxHORIZONTAL);

   //The type selection
   wxStaticBox* attributeType = new wxStaticBox(_attributePanel, -1, wxT("Attribute Type"));
   wxStaticBoxSizer* attributeTypeSizer = new wxStaticBoxSizer(attributeType, wxVERTICAL);
  
   wxString choices [] = {"Materials","Shaders"};
   _attributeType = new wxComboBox(_attributePanel, ATTRIBUTE_TYPE, 
                               wxString("Materials"), 
                               wxDefaultPosition, 
                               wxDefaultSize,
                               2, 
                               choices,wxCB_DROPDOWN);
   attributeTypeSizer->Add(_attributeType,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   attributePropSizer->Add(attributeTypeSizer,1,wxEXPAND|wxALIGN_CENTER);


   //Active attribute selection
   wxStaticBox* activeAttribute = new wxStaticBox(_attributePanel, -1, wxT("Available Attributes"));
   wxStaticBoxSizer* activeAttributeSizer = new wxStaticBoxSizer(activeAttribute , wxVERTICAL);
   _attributeSelection = new wxListBox(_attributePanel,ACTIVE_ATTRIBUTE);

   if(_cadNode)
   {
      _updateAvailableAttributes();
      _attributeSelection->Set(_availableMaterials);
      
   }
   activeAttributeSizer->Add(_attributeSelection);
   attributePropSizer->Add(activeAttributeSizer,1,wxEXPAND|wxALIGN_CENTER);


   attributePanelSizer->Add(attributePropSizer,1,wxEXPAND|wxALIGN_CENTER);

   _attributePanel->SetAutoLayout(true);
   _attributePanel->SetSizer(attributePanelSizer);
}
///////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAvailableAttributes()
{
   if(_cadNode->GetAttributeList().size())
   {
      _availableShaders.clear();
      _availableMaterials.clear();

      std::vector<CADAttribute*> attributes = _cadNode->GetAttributeList();
      size_t nAttributes = _cadNode->GetAttributeList().size();
      std::string attributeType;
      for(size_t i = 0; i < nAttributes; i++)
      {
         attributeType = attributes.at(i)->GetAttributeType(); 
         if( attributeType == std::string("Material"))
         {   
            _availableMaterials.Add(attributes.at(i)->GetAttributeName().c_str());
         }
         else if( attributeType == std::string("Program"))
         {
            _availableShaders.Add(attributes.at(i)->GetAttributeName().c_str());
         }
      }
   }
}
//////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAttributeType(wxCommandEvent& WXUNUSED(event))
{
   if(_attributeType->GetValue() == wxString("Materials"))
   {
      _attributeSelection->Set(_availableMaterials);
   }
   else if(_attributeType->GetValue() == wxString("Shaders"))
   {
      _attributeSelection->Set(_availableShaders);
   }
}
/////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_setActiveAttribute(wxCommandEvent& WXUNUSED(event))
{
   if(_cadNode)
   {
      wxString attributeName = _attributeSelection->GetStringSelection();
      _cadNode->SetActiveAttribute(attributeName.GetData());
   }
}
//////////////////////////////////////////////
void CADNodePropertiesDlg::ClearInstructions()
{
   ///deleting the command deletes the memory but
   ///we need to insure that the vector is clear
   _instructions.clear();
}
///////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateTransform(wxSpinEvent& WXUNUSED(event))
{
   if(_cadNode)
   {
      ClearInstructions();
      std::vector<double> temp;

      temp.push_back(_xTransformCtrl->GetValue());
      temp.push_back(_yTransformCtrl->GetValue());
      temp.push_back(_zTransformCtrl->GetValue());
      _cadNode->GetTransform()->GetTranslationArray()->SetArray(temp);

      temp.clear();

      temp.push_back(_xScaleCtrl->GetValue());
      temp.push_back(_yScaleCtrl->GetValue());
      temp.push_back(_zScaleCtrl->GetValue());
      _cadNode->GetTransform()->GetScaleArray()->SetArray(temp);

      temp.clear();

      temp.push_back(_xRotationCtrl->GetValue());
      temp.push_back(_yRotationCtrl->GetValue());
      temp.push_back(_zRotationCtrl->GetValue());
      _cadNode->GetTransform()->GetRotationArray()->SetArray(temp);

      temp.clear();

      _commandName = std::string("CAD_TRANSFORM_UPDATE");

      VE_XML::DataValuePair* nodeID = new VE_XML::DataValuePair();
      nodeID->SetDataType("UNSIGNED INT");
      nodeID->SetDataName(std::string("Node ID"));
      nodeID->SetDataValue(_cadNode->GetID());
      _instructions.push_back(nodeID);
      
      VE_XML::DataValuePair* updateTransform = new VE_XML::DataValuePair();
      updateTransform->SetDataType("XMLOBJECT");
      updateTransform->SetData("Transform",_cadNode->GetTransform());
      _instructions.push_back(updateTransform);

      VE_XML::DataValuePair* nodeType = new VE_XML::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetDataName(std::string("Node Type"));
      nodeType->SetDataString(_cadNode->GetNodeType());
      _instructions.push_back(nodeType);

      _sendCommandsToXplorer();
   }
}
///////////////////////////////////////////////////////////////////
/*void CADNodePropertiesDlg::_loadNewAttribute(int style,int fileType)
{
   wxString lastFile;
   wxTextCtrl* currentTextCtrl = 0;
   if(fileType == TERRAIN_TEXTURE){
      std::cout<<"Loading terrain texture!"<<std::endl;
      currentTextCtrl = _terrainTextureFileCtrl;
   }else if(fileType == HEIGHT_MAP){
      std::cout<<"Loading Height Map!"<<std::endl;
      currentTextCtrl = _heightMapFileCtrl;
   }else if(fileType == FIRE_FILE){
      currentTextCtrl = _firePositionFileCtrl;
   }

   wxFileDialog fileBrowser(this,wxT("Choose a file"),  wxString(""),
                currentTextCtrl->GetValue(), wxString("*.*"),style);   

   wxString fileName;
   wxString filePath;
   wxString fullPath;
   if(fileBrowser.ShowModal() == wxID_OK){
      fileName = fileBrowser.GetFilename();
      filePath = fileBrowser.GetDirectory();
      fullPath = filePath + wxString("/") + fileName;
      currentTextCtrl->SetValue(fullPath);
   }
}*/
#ifndef STAND_ALONE
///////////////////////////////////////////////////
void CADNodePropertiesDlg::_sendCommandsToXplorer()
{
   VE_XML::Command* cadCommand = new VE_XML::Command();

   for(size_t i =0; i < _instructions.size(); i++)
   {
      cadCommand->AddDataValuePair(_instructions.at(i));
   }

   cadCommand->SetCommandName(_commandName);

   std::string commandString("returnString");
   VE_CAD::CADXMLReaderWriter cadCommandWriter;
   cadCommandWriter.UseStandaloneDOMDocumentManager();
   cadCommandWriter.WriteToString();
   cadCommandWriter.WriteXMLDocument(cadCommand,commandString,std::string("vecommand"));

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

