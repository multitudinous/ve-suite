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

#include <ves/conductor/util/CADNodePropsDlg.h>
#include <sstream>
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
#include <wx/listctrl.h>
#include <wx/arrstr.h>
#include <wx/filedlg.h>
#include <wx/textdlg.h>
#include <wx/msgdlg.h>
#include <wx/cmndata.h>
#include <wx/colordlg.h>
#include <wx/choicdlg.h>
#include <wx/intl.h>
#include <wx/filename.h>
#include <wx/checkbox.h>
#include <wx/radiobox.h>

#include <iostream>
#include <ves/conductor/util/spinctld.h>

#include <ves/conductor/util/CADMaterialEditMenu.h>
#include <ves/conductor/util/CADOpacitySliderDlg.h>
#include <ves/conductor/util/TransformUI.h>
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/XMLReaderWriter.h>
#include <ves/open/xml/Transform.h>
#include <ves/open/xml/FloatArray.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

#include <ves/open/xml/cad/CADNode.h>
#include <ves/open/xml/cad/CADAttribute.h>
#include <ves/open/xml/cad/CADMaterial.h>
#include <ves/open/xml/cad/CADNodeAnimation.h>

#include <ves/open/xml/shader/Program.h>

using namespace ves::open::xml::cad;
using namespace ves::open::xml::shader;
using namespace ves::conductor::util;
BEGIN_EVENT_TABLE(CADNodePropertiesDlg,wxDialog)
   EVT_BUTTON(ADD_ANIMATION,CADNodePropertiesDlg::_addAnimation)
   EVT_BUTTON(ADD_ATTRIBUTE,CADNodePropertiesDlg::_addAttribute)
   EVT_BUTTON(RESTORE_DEFAULT_ATTRIBUTE,CADNodePropertiesDlg::_restoreDefaultAttribute)
   EVT_BUTTON(REMOVE_ATTRIBUTE,CADNodePropertiesDlg::_removeAttribute)
   EVT_SPINCTRL(TRANSFORM_PANEL_ID,CADNodePropertiesDlg::_updateTransform)
   EVT_SPINCTRL( PHYSICS_MASS_ID, CADNodePropertiesDlg::_updatePhysicsProperties )
   EVT_SPINCTRL( PHYSICS_FRICTION_ID, CADNodePropertiesDlg::_updatePhysicsProperties )
   EVT_SPINCTRL( PHYSICS_RESTITUTION_ID, CADNodePropertiesDlg::_updatePhysicsProperties )
   EVT_RADIOBOX( PHYSICS_MESH_ID, CADNodePropertiesDlg::_updatePhysicsMesh )
   EVT_COMBOBOX(ATTRIBUTE_TYPE,CADNodePropertiesDlg::_updateAttributeType)
   EVT_LIST_ITEM_SELECTED(ACTIVE_ATTRIBUTE,CADNodePropertiesDlg::_setActiveAttribute)
   EVT_LIST_ITEM_RIGHT_CLICK(ACTIVE_ATTRIBUTE, CADNodePropertiesDlg::_editAttribute)
   //EVT_CHECKBOX(ASSOCIATE_CHECKBOX,CADNodePropertiesDlg::_onAssociateCheckBox)
   EVT_MENU(CADMaterialEditMenu::DIFFUSE_ID,CADNodePropertiesDlg::_showColorDialog)
   EVT_MENU(CADMaterialEditMenu::AMBIENT_ID,CADNodePropertiesDlg::_showColorDialog)
   EVT_MENU(CADMaterialEditMenu::SPECULAR_ID,CADNodePropertiesDlg::_showColorDialog)
   EVT_MENU(CADMaterialEditMenu::EMISSIVE_ID,CADNodePropertiesDlg::_showColorDialog)
   EVT_MENU(CADMaterialEditMenu::FACE_ID,CADNodePropertiesDlg::_showFaceSelectDialog)
   EVT_MENU(CADMaterialEditMenu::COLOR_MODE_ID,CADNodePropertiesDlg::_showColorModeSelectDialog)
   EVT_MENU(CADMaterialEditMenu::OPACITY_ID,CADNodePropertiesDlg::_showOpacityDialog)
   EVT_CHECKBOX(UNIFORM_SCALE, CADNodePropertiesDlg::UpdateUniformScale)
END_EVENT_TABLE()
////////////////////////////////////////////////////
//Here is the constructor with passed in pointers //
////////////////////////////////////////////////////
CADNodePropertiesDlg::CADNodePropertiesDlg (wxWindow* parent,
                                       int id,CADNode* activeNode)

:wxDialog((wxWindow *) parent, id, _("CAD Properties"),wxDefaultPosition,wxDefaultSize,
(wxDEFAULT_DIALOG_STYLE|wxRESIZE_BORDER|wxMAXIMIZE_BOX|wxMINIMIZE_BOX),_("CADTree Properties"))
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
   _physicsPanel = 0;
   _animationPanel = 0;

   _attributeType = 0;
   _attributeSelection = 0;
   _addAttributeButton = 0;
   _editAttributeButton = 0;
   
   _addAnimationButton = 0;
   _restoreDefaultAttributeButton = 0;
   //_associateWithDataCheck = 0;
   _nShaders = 0;
   _nMaterials = 0;

   tempX = 1.0;
   tempY = 1.0;
   tempZ = 1.0;

   _buildGUI();
   
   CentreOnParent();
}

/////////////////////////////////////////////////////
CADNodePropertiesDlg::~CADNodePropertiesDlg()
{
}
///////////////////////////////////
void CADNodePropertiesDlg::_buildGUI()
{
   wxBoxSizer* mainSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* notebookSizer = new wxBoxSizer(wxVERTICAL);
   wxBoxSizer* bottomRow = new wxBoxSizer(wxVERTICAL);

   _buildTabs();
   notebookSizer->Add(_propertyTabs,2,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   
   mainSizer->Add(notebookSizer,3,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   bottomRow->Add(new wxButton(this,wxID_OK,_("OK")),0,wxALIGN_CENTER);

   mainSizer->Add(bottomRow,0,wxALIGN_CENTER);
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
   if( ( _cadNode->GetNodeType() == "Part" ) && ( _cadNode->HasPhysics() ) )
   {
       _propertyTabs->AddPage( GetPhysicsPanel(), _T( "Physics" ), false );
   }
   
   //_propertyTabs->AddPage(GetAnimationPanel(),_T("Animation"), false);
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
wxPanel* CADNodePropertiesDlg::GetPhysicsPanel()
{
   if(!_physicsPanel)
   {
      _buildPhysicsPanel();
   }
   return _physicsPanel;
}
//////////////////////////////////////////////////
wxPanel* CADNodePropertiesDlg::GetAnimationPanel()
{
   if(!_animationPanel)
   {
      _buildAnimationPanel();
   }
   return _animationPanel;
}
///////////////////////////////////////////////////
void CADNodePropertiesDlg::_buildTransformPanel()
{
   _transformPanel = new wxPanel(_propertyTabs,TRANSFORM_PANEL_ID);

   wxBoxSizer* transformPanelSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* transformProperties = new wxStaticBox(_transformPanel, -1, wxT("CADNode Transform Properties"));
   wxStaticBoxSizer* transformPropSizer = new wxStaticBoxSizer(transformProperties, wxVERTICAL);


   ///translation
   wxStaticBox* translation = new wxStaticBox(_transformPanel, -1, wxT("Translation (ft)"));
   wxStaticBoxSizer* transSizer = new wxStaticBoxSizer(translation, wxHORIZONTAL);
   _xTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _xTransformCtrl->SetValue(0);
   _xTransformCtrl->SetRange(-30000.0,30000.0);
   _xTransformCtrl->SetIncrement(1.0);
   _xTransformCtrl->Raise();
   
   _yTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _yTransformCtrl->SetValue(0);
   _yTransformCtrl->SetRange(-30000.0,30000.0);
   _yTransformCtrl->SetIncrement(1.0);
   _yTransformCtrl->Raise();

   _zTransformCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _zTransformCtrl->SetValue(0);
   _zTransformCtrl->SetRange(-30000.0,30000.0);
   _zTransformCtrl->SetIncrement(1.0);
   _zTransformCtrl->Raise();

   transSizer->Add(_xTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   transSizer->Add(_yTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   transSizer->Add(_zTransformCtrl,1,wxALIGN_CENTER_HORIZONTAL);

   transformPropSizer->Add(transSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //rotation
   wxStaticBox* rotation = new wxStaticBox(_transformPanel, -1, wxT("Rotation (deg)"));
   wxStaticBoxSizer* rotationSizer = new wxStaticBoxSizer(rotation, wxHORIZONTAL);
   _xRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID,
                    wxEmptyString,
                    wxDefaultPosition,
                    wxSize(95,-1),
                    wxSP_ARROW_KEYS|wxSP_WRAP);
   //_xRotationCtrl->SetStyle(wxSP_ARROW_KEYS|wxSP_WRAP);
   _xRotationCtrl->SetValue(0);
   _xRotationCtrl->SetRange(-360.0,360.0);
   _xRotationCtrl->SetIncrement(1.0);
   _xRotationCtrl->Raise();

   _yRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID,
                    wxEmptyString,
                    wxDefaultPosition,
                    wxSize(95,-1),
                    wxSP_ARROW_KEYS|wxSP_WRAP);
   _yRotationCtrl->SetValue(0);
   _yRotationCtrl->SetRange(-360.0,360.0);
   _yRotationCtrl->SetIncrement(1.0);
   _yRotationCtrl->Raise();

   _zRotationCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID,
                    wxEmptyString,
                    wxDefaultPosition,
                    wxSize(95,-1),
                    wxSP_ARROW_KEYS|wxSP_WRAP);
   _zRotationCtrl->SetValue(0);
   _zRotationCtrl->SetRange(-360.0,360.0);
   _zRotationCtrl->SetIncrement(1.0);
   _zRotationCtrl->Raise();

   rotationSizer->Add(_xRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   rotationSizer->Add(_yRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);
   rotationSizer->Add(_zRotationCtrl,1,wxALIGN_CENTER_HORIZONTAL);

   transformPropSizer->Add(rotationSizer,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   //scale
   wxBoxSizer* scaleInfo = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* scale = new wxStaticBox(_transformPanel, -1, wxT("Scale "));
   wxStaticBoxSizer* scaleSizer = new wxStaticBoxSizer(scale, wxHORIZONTAL);

   _xScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _yScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   _zScaleCtrl =  new wxSpinCtrlDbl(_transformPanel, TRANSFORM_PANEL_ID);
   
   _xScaleCtrl->SetValue(1.0);
   _xScaleCtrl->SetRange(0.0,30000.0);
   _xScaleCtrl->SetIncrement(1.0);
   _xScaleCtrl->Raise();

   _yScaleCtrl->SetValue(1.0);
   _yScaleCtrl->SetRange(0.0,30000.0);
   _yScaleCtrl->SetIncrement(1.0);
   _yScaleCtrl->Raise();

   _zScaleCtrl->SetValue(1.0);
   _zScaleCtrl->SetRange(0.0,30000.0);
   _zScaleCtrl->SetIncrement(1.0);
   _zScaleCtrl->Raise();

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
   
   m_uniformScale = new wxCheckBox(_transformPanel, UNIFORM_SCALE, wxT("Uniform Scaling"), wxDefaultPosition, wxDefaultSize, wxCHK_2STATE );
   m_uniformScale->SetValue( true );

   scaleInfo->Add(scaleSizer, 1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
   scaleInfo->Add(m_uniformScale, 1, wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

   transformPropSizer->Add(scaleInfo,2,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);

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
  
   wxString choices [] = {_("Materials"),_("Shaders")};
   _attributeType = new wxComboBox(_attributePanel, ATTRIBUTE_TYPE, 
                               _("Materials"), 
                               wxDefaultPosition, 
                               wxDefaultSize,
                               2, 
                               choices,wxCB_DROPDOWN);
   attributeTypeSizer->Add(_attributeType,1,wxEXPAND|wxALIGN_CENTER_HORIZONTAL);
    
   _addAttributeButton = new wxButton(_attributePanel, ADD_ATTRIBUTE,_("Add..."));
   /*_associateWithDataCheck = new wxCheckBox(_attributePanel, ASSOCIATE_CHECKBOX,
	                                        _T("Use Last Seed Point"), 
											wxDefaultPosition, wxDefaultSize, 0 );
   _associateWithDataCheck->SetValue(false);*/
   attributeTypeSizer->Add(_addAttributeButton,0,wxALIGN_CENTER);

   
   _removeAttributeButton = new wxButton(_attributePanel, REMOVE_ATTRIBUTE,_("Remove..."));
   attributeTypeSizer->Add(_removeAttributeButton,0,wxALIGN_CENTER);

   _restoreDefaultAttributeButton = new wxButton(_attributePanel, RESTORE_DEFAULT_ATTRIBUTE,_("Restore Defaults"));
   attributeTypeSizer->Add(_restoreDefaultAttributeButton ,0,wxALIGN_CENTER);

   //_editAttributeButton = new wxButton(_attributePanel, EDIT_ATTRIBUTE,wxString("Edit..."));
   //attributeTypeSizer->Add(_editAttributeButton,0,wxALIGN_CENTER);

   attributePropSizer->Add(attributeTypeSizer,1,wxEXPAND|wxALIGN_CENTER);

   //Active attribute selection
   wxStaticBox* activeAttribute = new wxStaticBox(_attributePanel, -1, wxT("Available Attributes"));
   wxStaticBoxSizer* activeAttributeSizer = new wxStaticBoxSizer(activeAttribute , wxVERTICAL);
   //_attributeSelection = new wxListBox(_attributePanel,ACTIVE_ATTRIBUTE);
   _attributeSelection = new wxListCtrl(_attributePanel,ACTIVE_ATTRIBUTE,wxDefaultPosition,wxDefaultSize,wxLC_SINGLE_SEL|wxLC_LIST);

   if(_cadNode)
   {
      _updateAvailableAttributes();
      _updateAttributeList(_availableMaterials);
      //_attributeSelection->Set(_availableMaterials);
      
   }
   activeAttributeSizer->Add(_attributeSelection,1,wxEXPAND|wxALIGN_CENTER);

   attributePropSizer->Add(activeAttributeSizer,1,wxEXPAND|wxALIGN_CENTER);


   attributePanelSizer->Add(attributePropSizer,1,wxEXPAND|wxALIGN_CENTER);

   _attributePanel->SetAutoLayout(true);
   _attributePanel->SetSizer(attributePanelSizer);
}
//////////////////////////////////////////////////
void CADNodePropertiesDlg::_buildPhysicsPanel()
{
   _physicsPanel = new wxPanel( _propertyTabs, PHYSICS_PANEL_ID );

   wxBoxSizer* physicsPanelSizer = new wxBoxSizer( wxVERTICAL );

   wxStaticBox* physicsProperties = new wxStaticBox( _physicsPanel, -1, wxT( "Physics Properties" ) );
   wxStaticBoxSizer* physicsPropSizer = new wxStaticBoxSizer( physicsProperties, wxHORIZONTAL );

   wxStaticBox* mass = new wxStaticBox( _physicsPanel, -1, wxT( "Mass" ) );
   wxStaticBoxSizer* massSizer = new wxStaticBoxSizer( mass, wxVERTICAL );
   _physicsMassCtrl =  new wxSpinCtrlDbl( _physicsPanel, PHYSICS_MASS_ID );
   _physicsMassCtrl->SetValue( 1.0 );
   _physicsMassCtrl->SetRange( 0.0, 100.0 );
   _physicsMassCtrl->SetIncrement( 1.0 );
   _physicsMassCtrl->Raise();

   wxStaticBox* friction = new wxStaticBox( _physicsPanel, -1, wxT( "Friction" ) );
   wxStaticBoxSizer* frictionSizer = new wxStaticBoxSizer( friction, wxVERTICAL );
   _physicsFrictionCtrl =  new wxSpinCtrlDbl( _physicsPanel, PHYSICS_FRICTION_ID );
   _physicsFrictionCtrl->SetValue( 1.0 );
   _physicsFrictionCtrl->SetRange( 0.0, 1.0 );
   _physicsFrictionCtrl->SetIncrement( 0.1 );
   _physicsFrictionCtrl->Raise();

   wxStaticBox* restitution = new wxStaticBox( _physicsPanel, -1, wxT( "Restitution" ) );
   wxStaticBoxSizer* restitutionSizer = new wxStaticBoxSizer( restitution, wxVERTICAL );
   _physicsRestitutionCtrl =  new wxSpinCtrlDbl( _physicsPanel, PHYSICS_RESTITUTION_ID );
   _physicsRestitutionCtrl->SetValue( 0.0 );
   _physicsRestitutionCtrl->SetRange( 0.0, 1.0 );
   _physicsRestitutionCtrl->SetIncrement( 0.1 );
   _physicsRestitutionCtrl->Raise();

   massSizer->Add( _physicsMassCtrl, 1, wxALIGN_CENTER );
   frictionSizer->Add( _physicsFrictionCtrl, 1, wxALIGN_CENTER );
   restitutionSizer->Add( _physicsRestitutionCtrl, 1, wxALIGN_CENTER );
   
   physicsPropSizer->Add( massSizer, 1, wxALIGN_CENTER_HORIZONTAL );
   physicsPropSizer->Add( frictionSizer, 1, wxALIGN_CENTER_HORIZONTAL );
   physicsPropSizer->Add( restitutionSizer, 1, wxALIGN_CENTER_HORIZONTAL );

   wxString meshStrings[] = { _T( "Bounding Box" ), _T( "Convex" ), _T( "Static Concave" ) };
   meshProperties = new wxRadioBox( _physicsPanel, PHYSICS_MESH_ID, wxT( "Physics Mesh Type" ), 
                                    wxDefaultPosition, wxDefaultSize, 3,
                                    meshStrings, 0, wxRA_SPECIFY_ROWS );

   if( _cadNode )
   {
      _physicsMassCtrl->SetValue( _cadNode->GetMass() );
      _physicsFrictionCtrl->SetValue( _cadNode->GetFriction() );
      _physicsRestitutionCtrl->SetValue( _cadNode->GetRestitution() );

      meshProperties->SetStringSelection( wxString( _cadNode->GetPhysicsMesh().c_str(), wxConvUTF8 ) );
   }
   
   physicsPanelSizer->Add( physicsPropSizer, 1, wxEXPAND|wxALIGN_CENTER );
   physicsPanelSizer->Add( meshProperties, 1, wxEXPAND|wxALIGN_CENTER );

   _physicsPanel->SetAutoLayout( true );
   _physicsPanel->SetSizer( physicsPanelSizer );
}
//////////////////////////////////////////////////
void CADNodePropertiesDlg::_buildAnimationPanel()
{
   _animationPanel = new wxPanel(_propertyTabs,ANIMATION_PANEL_ID);

   wxBoxSizer* animationPanelSizer = new wxBoxSizer(wxVERTICAL);
   wxStaticBox* animationProperties = new wxStaticBox(_animationPanel, -1, wxT("CADNode Animations"));
   wxStaticBoxSizer* animationPropSizer = new wxStaticBoxSizer(animationProperties, wxHORIZONTAL);

   //Active animation selection
   wxStaticBox* activeAnimation = new wxStaticBox(_animationPanel, -1, wxT("Available Animations"));
   wxStaticBoxSizer* activeAnimationSizer = new wxStaticBoxSizer(activeAnimation , wxVERTICAL);
   _animationSelection = new wxListCtrl(_animationPanel,ACTIVE_ANIMATION,wxDefaultPosition,wxDefaultSize,wxLC_SINGLE_SEL|wxLC_LIST);

   if(_cadNode)
   {
      _updateAvailableAnimations();
      
   }
   _addAnimationButton = new wxButton(_animationPanel, ADD_ANIMATION,_("Add..."));
   animationPropSizer->Add(_addAnimationButton,0,wxALIGN_CENTER);

   _removeAnimationButton = new wxButton(_animationPanel, REMOVE_ANIMATION,_("Remove..."));
   animationPropSizer->Add(_removeAnimationButton,0,wxALIGN_CENTER);
   
   activeAnimationSizer->Add(_animationSelection,1,wxEXPAND|wxALIGN_CENTER);

   animationPropSizer->Add(activeAnimationSizer,1,wxEXPAND|wxALIGN_CENTER);


   animationPanelSizer->Add(animationPropSizer,1,wxEXPAND|wxALIGN_CENTER);

   _animationPanel->SetAutoLayout(true);
   _animationPanel->SetSizer(animationPanelSizer);
}
///////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_addAnimation(wxCommandEvent& event)
{
   if(_cadNode)
   {
      ClearInstructions();
    
      {
         
         wxFileDialog dialog(this,
		          _T("Add New Animation File"), 
		          _T(""), 
		          _T(""),
		          _T("Animation Files(*.txt)|*.txt;"),
		          wxOPEN|wxFILE_MUST_EXIST); 
         try
         {
            if(dialog.ShowModal() == wxID_OK) 
            {
               wxFileName animationFileName( dialog.GetPath() );
               animationFileName.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
               wxString animationFileNamePath( wxString( "./",wxConvUTF8 ) + animationFileName.GetFullPath() );

               wxTextEntryDialog animationNameDlg(this, 
                        _("New Animation Name"),
                        _("Enter name for new node animation:"),
                        animationFileName.GetName(),wxOK);

               animationNameDlg.CentreOnParent();
               animationNameDlg.ShowModal();
            
               while(AnimationExists(animationNameDlg.GetValue().GetData()))
               {
                  wxMessageBox( _("Animation with this name is already loaded."), 
                  animationNameDlg.GetValue(), wxOK | wxICON_INFORMATION );
               }
               _cadNode->AddAnimation( ConvertUnicode( animationNameDlg.GetValue().GetData() ),
                                   ConvertUnicode( animationFileNamePath.c_str() ) );
               _updateAvailableAnimations();
                  
               _commandName = std::string("CAD_ADD_ANIMATION_TO_NODE");
                  
               ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
               nodeID->SetDataType("STRING");
               nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
               _instructions.push_back(nodeID);
      
               ves::open::xml::DataValuePair* addAnimation = new ves::open::xml::DataValuePair();
               addAnimation->SetDataType("XMLOBJECT");
               addAnimation->SetData("Animation Info",
                                  &_cadNode->GetAnimation( ConvertUnicode( animationNameDlg.GetValue().GetData() ) ) );
               _instructions.push_back(addAnimation);

              _sendCommandsToXplorer();
            }
         }
      
         catch(...)
         {
            wxMessageBox( _("Couldn't load animation file."), 
                       dialog.GetPath(), wxOK | wxICON_INFORMATION );
                     return;
         }
      }
   }

}
//////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_removeAnimation(wxCommandEvent& event)
{
}
//////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_setActiveAnimation(wxListEvent& event)
{
}
/////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAnimationList(wxArrayString animations)
{  
   _animationSelection->ClearAll();
   for(size_t i = 0; i < animations.GetCount(); i++)
   {
      _animationSelection->InsertItem(i,animations[i]);
   }
}
//////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAvailableAnimations()
{
   
   if(_cadNode->GetNumberOfAnimations() > 0)
   {
      _animationFiles.clear();

      size_t nAnimations = _cadNode->GetNumberOfAnimations();

      for(size_t i = 0; i < nAnimations; i++)
      {
         _animationFiles.Add( wxString( _cadNode->GetAnimation(i).GetAnimationName().c_str(), wxConvUTF8 ) );   
      }
      
   }
   _updateAnimationList(_animationFiles);
   
}
///////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAttributeList(wxArrayString listOfAttributes)
{
   _attributeSelection->ClearAll();
   for(size_t i = 0; i < listOfAttributes.GetCount(); i++)
   {
      _attributeSelection->InsertItem(i,listOfAttributes[i]);
   }
}
///////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAvailableAttributes()
{
   _availableShaders.clear();
   _availableMaterials.clear();
   _nShaders = 0;
   _nMaterials = 0;
   if(_cadNode->GetAttributeList().size())
   {
      _availableShaders.clear();
      _availableMaterials.clear();
      _nShaders = 0;
      _nMaterials = 0;

      std::vector<CADAttribute> attributes = _cadNode->GetAttributeList();
      size_t nAttributes = _cadNode->GetAttributeList().size();
      std::string attributeType;
      for(size_t i = 0; i < nAttributes; i++)
      {
         attributeType = attributes.at(i).GetAttributeType(); 
         if( attributeType == std::string("Material"))
         {   
            _nMaterials++;
            _availableMaterials.Add( wxString( attributes.at(i).GetAttributeName().c_str(), wxConvUTF8 ) );
         }
         else if( attributeType == std::string("Program"))
         {
            _nShaders++;
            _availableShaders.Add( wxString(attributes.at(i).GetAttributeName().c_str(), wxConvUTF8) );
         }
      }
      
   }
   if(_attributeType->GetValue() == wxString("Materials",wxConvUTF8))
   {
      //_attributeSelection->Set(_availableMaterials);
      _updateAttributeList(_availableMaterials);
   }
   else if(_attributeType->GetValue() == wxString("Shaders",wxConvUTF8))
   {
       //_attributeSelection->Set(_availableShaders);
       _updateAttributeList(_availableShaders);
   }
}
//////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updateAttributeType(wxCommandEvent& WXUNUSED(event))
{
   if(_attributeType->GetValue() == wxString("Materials",wxConvUTF8))
   {
      _updateAttributeList(_availableMaterials);
      //_attributeSelection->Set(_availableMaterials);
   }
   else if(_attributeType->GetValue() == wxString("Shaders",wxConvUTF8))
   {
      //_attributeSelection->Set(_availableShaders);
      _updateAttributeList(_availableShaders);
   }
}
/////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_editAttribute(wxListEvent& event)
{
   if(_cadNode)
   {
      ClearInstructions();
      wxString attributeName = event.GetText();
      //std::cout<<"Editting attribute"<<attributeName<<std::endl;
      if(_attributeType->GetValue() == wxString("Materials",wxConvUTF8))
      {
         CADMaterialEditMenu* materialMenu = new CADMaterialEditMenu();
         PopupMenu(materialMenu);
      }
   }
}
/////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_setActiveAttribute(wxListEvent& event)
{
   if(_cadNode)
   {
      ClearInstructions();
      wxString attributeName = event.GetText();
      //wxString attributeName = _attributeSelection->GetStringSelection();
      _cadNode->SetActiveAttribute( ConvertUnicode( attributeName.GetData() ) );
      _commandName = std::string("CAD_SET_ACTIVE_ATTRIBUTE_ON_NODE");

      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
      _instructions.push_back(nodeID);

      ves::open::xml::DataValuePair* activeAttribute = new ves::open::xml::DataValuePair();
      activeAttribute->SetDataType("STRING");
      activeAttribute->SetData("Active Attribute",_cadNode->GetActiveAttribute().GetAttributeName());
      _instructions.push_back(activeAttribute);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetDataName(std::string("Node Type"));
      nodeType->SetDataString(_cadNode->GetNodeType());
      _instructions.push_back(nodeType);


      _sendCommandsToXplorer();

   }
}
//////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_restoreDefaultAttribute(wxCommandEvent& event)
{
   if(_cadNode)
   {
      ClearInstructions();
      _commandName = std::string("CAD_SET_ACTIVE_ATTRIBUTE_ON_NODE");

      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
      _instructions.push_back(nodeID);

      ves::open::xml::DataValuePair* activeAttribute = new ves::open::xml::DataValuePair();
      activeAttribute->SetDataType("STRING");
      activeAttribute->SetData("Active Attribute","Default Attribute");
      _instructions.push_back(activeAttribute);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetDataName(std::string("Node Type"));
      nodeType->SetDataString(_cadNode->GetNodeType());
      _instructions.push_back(nodeType);

      _sendCommandsToXplorer();
      _updateAvailableAttributes();
   }
}
//////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_removeAttribute(wxCommandEvent& event)
{
   if(_cadNode)
   {
      ClearInstructions();
      _commandName = "CAD_REMOVE_ATTRIBUTE";
      std::string attributeName = _cadNode->GetActiveAttribute().GetAttributeName();
      _cadNode->RemoveAttribute(attributeName);
      _updateAvailableAttributes();
      
      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
      _instructions.push_back(nodeID);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType ->SetDataType("STRING");
      nodeType ->SetData("Node Type",_cadNode->GetNodeType());
      _instructions.push_back(nodeType );
      
      ves::open::xml::DataValuePair* addAttribute = new ves::open::xml::DataValuePair();
      addAttribute->SetDataType("STRING");
      addAttribute->SetData("Attribute Name",attributeName);
      _instructions.push_back(addAttribute);

      _sendCommandsToXplorer();
   }
}
/////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_addAttribute(wxCommandEvent& WXUNUSED(event))
{
   if(_cadNode)
   {
      ClearInstructions();
      wxString newAttributeName("Attribute",wxConvUTF8);
      if(_attributeType->GetValue() == wxString("Materials",wxConvUTF8))
      {
         std::stringstream nMaterials;
         nMaterials<<_nMaterials;

         ves::open::xml::cad::CADAttribute newAttribute;
         newAttribute.SetAttributeType("Material");
         
         ves::open::xml::cad::CADMaterial newMaterial;
         
         wxTextEntryDialog materialNameDlg(this, 
                                       _("New Material Name"),
                                       _("Enter name for new material:"),
                                       _("Material")+wxString(nMaterials.str().c_str(),wxConvUTF8),wxOK);

         materialNameDlg.CentreOnParent();
         materialNameDlg.ShowModal();
         if(AttributeExists( ConvertUnicode( materialNameDlg.GetValue().GetData() ) ) )
         {
            wxMessageBox( _("Attribute with this name is already loaded."), 
                          materialNameDlg.GetValue(), wxOK | wxICON_INFORMATION );
                              return;
         }
         
         newMaterial.SetMaterialName( ConvertUnicode( materialNameDlg.GetValue().GetData() ) );
         newAttribute.SetMaterial(newMaterial);
         _cadNode->AddAttribute(newAttribute);
         _updateAvailableAttributes();
         //_attributeSelection->SetSelection(_nMaterials-1);

         _commandName = std::string("CAD_ADD_ATTRIBUTE_TO_NODE");
                        
         ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
         nodeID->SetDataType("STRING");
         nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
         _instructions.push_back(nodeID);
      
         ves::open::xml::DataValuePair* addAttribute = new ves::open::xml::DataValuePair();
         addAttribute->SetDataType("XMLOBJECT");
         addAttribute->SetData("Attribute",&_cadNode->GetAttribute(newAttribute.GetAttributeName()));
         _instructions.push_back(addAttribute);

         _sendCommandsToXplorer();
      }
      else if(_attributeType->GetValue() == wxString("Shaders",wxConvUTF8))
      {
         wxFileDialog dialog(this,
		       _T("Add New Attribute"), 
		       _T(""), 
		       _T(""),
		       _T("VE-Attribute files (*.vea)|*.vea;"),
		       wxOPEN|wxFILE_MUST_EXIST); 
         if(dialog.ShowModal() == wxID_OK) 
         {
            {
               {
                  ves::open::xml::cad::CADAttribute newAttribute;// = new CADAttribute();
                  newAttribute.SetAttributeType("Program");
                  
                  wxFileName veaFileName( dialog.GetPath() );
                  veaFileName.MakeRelativeTo( ::wxGetCwd(), wxPATH_NATIVE );
                  wxString veaFileNamePath( wxString( "./", wxConvUTF8 ) + veaFileName.GetFullPath() );

                  ves::open::xml::XMLReaderWriter shaderLoader;
                  shaderLoader.UseStandaloneDOMDocumentManager();
                  shaderLoader.ReadFromFile();
                  shaderLoader.ReadXMLData( ConvertUnicode( veaFileNamePath ),"Shader","Program");
              
                  ves::open::xml::shader::Program* loadedShader = 0;
                  if(shaderLoader.GetLoadedXMLObjects().at(0))
                  {
                     try
                     {
                        loadedShader = dynamic_cast<ves::open::xml::shader::Program*>(shaderLoader.GetLoadedXMLObjects().at(0));
                        if(AttributeExists(loadedShader->GetProgramName().c_str()))
                        {
                           wxMessageBox( _("Attribute with this name is already loaded."), 
                                  dialog.GetPath(), wxOK | wxICON_INFORMATION );
                              return;
                        }
                        
                        newAttribute.SetProgram(*loadedShader);
                        _cadNode->AddAttribute(newAttribute);
                        _updateAvailableAttributes();
                        //_attributeSelection->SetSelection(_nShaders-1);

                        _commandName = std::string("CAD_ADD_ATTRIBUTE_TO_NODE");
                        
                        ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
                        nodeID->SetDataType("STRING");
                        nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
                        _instructions.push_back(nodeID);
      
                        ves::open::xml::DataValuePair* addAttribute = new ves::open::xml::DataValuePair();
                        addAttribute->SetDataType("XMLOBJECT");
                        addAttribute->SetData("Attribute",&_cadNode->GetAttribute(newAttribute.GetAttributeName()));
                        _instructions.push_back(addAttribute);

                        _sendCommandsToXplorer();
                     }
                     catch(...)
                     {
                        wxMessageBox( _("Couldn't load shader file."), 
                                      dialog.GetPath(), wxOK | wxICON_INFORMATION );
                        return;
                     }
                  }
               }      
            }
         }
      }
   }
}
////////////////////////////////////////////////////////////
bool CADNodePropertiesDlg::AnimationExists(wxString name)
{
   for(unsigned int i = 0; _animationFiles.GetCount(); i++)
   {
      if(name == _animationFiles[i])
      {
         return true;
      }
   }
   return false;
}
//////////////////////////////////////////////////////////
bool CADNodePropertiesDlg::AttributeExists(std::string name)
{
   for(unsigned int i = 0; i < _nShaders; i++)
   {
      if(name.c_str() == ConvertUnicode( _availableShaders[i]) )
         return true;
   }
   
   for(unsigned int i = 0; i < _nMaterials; i++)
   {
      if(name.c_str() ==ConvertUnicode( _availableMaterials[i]) )
         return true;
   }
   return false;
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

	  double xScale = _xScaleCtrl->GetValue();
	  double yScale = _yScaleCtrl->GetValue();
	  double zScale = _zScaleCtrl->GetValue();

	  if( m_uniformScale->IsChecked() == true )
	  {
		 if( tempX != xScale )
		 {
			double scaleBy = _xScaleCtrl->GetValue();
			_yScaleCtrl->SetValue(scaleBy);
			_zScaleCtrl->SetValue(scaleBy);
		 }
		 else if( tempY != yScale )
		 {
			double scaleBy = _yScaleCtrl->GetValue();
			_xScaleCtrl->SetValue(scaleBy);
			_zScaleCtrl->SetValue(scaleBy);
		 }
		 else if( tempZ != zScale )
		 {
			double scaleBy = _zScaleCtrl->GetValue();
			_xScaleCtrl->SetValue(scaleBy);
			_yScaleCtrl->SetValue(scaleBy);
		 }
	     temp.push_back(_xScaleCtrl->GetValue());
         temp.push_back(_yScaleCtrl->GetValue());
         temp.push_back(_zScaleCtrl->GetValue());
	  }
	  else
	  {
	     temp.push_back(_xScaleCtrl->GetValue());
         temp.push_back(_yScaleCtrl->GetValue());
         temp.push_back(_zScaleCtrl->GetValue());	  
	  }
      _cadNode->GetTransform()->GetScaleArray()->SetArray(temp);

	  tempX = _xScaleCtrl->GetValue();
	  tempY = _yScaleCtrl->GetValue();
	  tempZ = _zScaleCtrl->GetValue();

      temp.clear();

      temp.push_back(_xRotationCtrl->GetValue());
      temp.push_back(_yRotationCtrl->GetValue());
      temp.push_back(_zRotationCtrl->GetValue());
      _cadNode->GetTransform()->GetRotationArray()->SetArray(temp);

      temp.clear();

      _commandName = std::string("CAD_TRANSFORM_UPDATE");

      ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
      nodeID->SetDataType("STRING");
      nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
      _instructions.push_back(nodeID);
      
      ves::open::xml::DataValuePair* updateTransform = new ves::open::xml::DataValuePair();
      updateTransform->SetDataType("XMLOBJECT");
      updateTransform->SetData("Transform",_cadNode->GetTransform());
      _instructions.push_back(updateTransform);

      ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
      nodeType->SetDataType("STRING");
      nodeType->SetDataName(std::string("Node Type"));
      nodeType->SetDataString(_cadNode->GetNodeType());
      _instructions.push_back(nodeType);


      _sendCommandsToXplorer();
   }
}
///////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updatePhysicsProperties( wxSpinEvent& event )
{
    if( _cadNode )
    {
        _commandName = std::string( "PHYSICS_PROPERTIES" );

        ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
        nodeID->SetDataType( "STRING" );
        nodeID->SetData( std::string( "Node ID" ), _cadNode->GetID() );
        _instructions.push_back( nodeID );

        ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
        nodeType->SetDataType( "STRING" );
        nodeType->SetDataName(std::string( "Node Type" ) );
        nodeType->SetDataString( _cadNode->GetNodeType() );
        _instructions.push_back( nodeType );

        ves::open::xml::DataValuePair* physicsPropertyValue = new ves::open::xml::DataValuePair();
        physicsPropertyValue->SetDataType( "DOUBLE" );

        if( event.GetId() == PHYSICS_MASS_ID )
        {
            physicsPropertyValue->SetData( "Mass", _physicsMassCtrl->GetValue() );
            _cadNode->SetMass( _physicsMassCtrl->GetValue() );
        }
        else if( event.GetId() == PHYSICS_FRICTION_ID )
        {
            physicsPropertyValue->SetData( "Friction", _physicsFrictionCtrl->GetValue() );
            _cadNode->SetFriction( _physicsFrictionCtrl->GetValue() );
        }
        else if( event.GetId() == PHYSICS_RESTITUTION_ID )
        {
            physicsPropertyValue->SetData( "Restitution", _physicsRestitutionCtrl->GetValue() );
            _cadNode->SetRestitution( _physicsRestitutionCtrl->GetValue() );
        }

        _instructions.push_back( physicsPropertyValue );

        _sendCommandsToXplorer();
        ClearInstructions();
    }
}
///////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_updatePhysicsMesh( wxCommandEvent& event )
{
    if( _cadNode )
    {
        _commandName = std::string( "PHYSICS_MESH" );

        ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
        nodeID->SetDataType( "STRING" );
        nodeID->SetData( std::string( "Node ID" ), _cadNode->GetID() );
        _instructions.push_back( nodeID );

        ves::open::xml::DataValuePair* nodeType = new ves::open::xml::DataValuePair();
        nodeType->SetDataType( "STRING" );
        nodeType->SetDataName(std::string( "Node Type" ) );
        nodeType->SetDataString( _cadNode->GetNodeType() );
        _instructions.push_back( nodeType );

        ves::open::xml::DataValuePair* meshType = new ves::open::xml::DataValuePair();
        meshType->SetDataType( "STRING" );
        meshType->SetDataName( std::string( "Mesh Type" ) );
        meshType->SetDataString( ConvertUnicode( meshProperties->GetStringSelection() ) );
        _cadNode->SetPhysicsMesh( ConvertUnicode( meshProperties->GetStringSelection() ) );
        _instructions.push_back( meshType );

        _sendCommandsToXplorer();
        ClearInstructions();
    }
}
///////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::UpdateUniformScale( wxCommandEvent& WXUNUSED(event) )
{

}
///////////////////////////////////////////////////////////////////////////////
unsigned char CADNodePropertiesDlg::_convertToUnsignedCharColor(double value)
{
   return (unsigned char)(255.0 - 255.0*(1.0-value));
}
///////////////////////////////////////////////////////////////////////////////
double CADNodePropertiesDlg::_convertToDoubleColor(unsigned char value)
{
   return ((double)(value))/255.0;
}
/////////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_showFaceSelectDialog(wxCommandEvent& WXUNUSED(event))
{
   //We should only arrive in here if the attribute is a CADMaterial!!!!
   if(_cadNode)
   {
      wxArrayString faceModes;
      faceModes.Add(_("Front"));
      faceModes.Add(_("Front_and_Back"));
      faceModes.Add(_("Back"));

      CADMaterial* material = _cadNode->GetActiveAttribute().GetMaterial();
      wxSingleChoiceDialog faceSelector(this, _T("Select Face to apply material"), _T("Material Face"),
                           faceModes);

      if (faceSelector.ShowModal() == wxID_OK)
      {
         //std::cout<<"Selecting face: "<<faceSelector.GetStringSelection()<<std::endl;
         material->SetFace( ConvertUnicode(faceSelector.GetStringSelection().GetData()));     
         //send the data to Xplorer
         ClearInstructions(); 
         _commandName = std::string("CAD_ATTRIBUTE_MATERIAL_MODE");

         ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
         nodeID->SetDataType("STRING");
         nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
         _instructions.push_back(nodeID);

         ves::open::xml::DataValuePair* componentToUpdate = new ves::open::xml::DataValuePair();
         componentToUpdate->SetDataType("STRING");
         componentToUpdate->SetData("Mode","Face");
         _instructions.push_back(componentToUpdate);

         ves::open::xml::DataValuePair* materialToUpdate = new ves::open::xml::DataValuePair();
         materialToUpdate->SetDataType("XMLOBJECT");
         materialToUpdate->SetData("Material", material);
         _instructions.push_back(materialToUpdate);

         _sendCommandsToXplorer();
      }
   }
}
/////////////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_showOpacityDialog(wxCommandEvent& WXUNUSED(event))
{
   //We should only arrive in here if the attribute is a CADMaterial!!!!
   if(_cadNode)
   {
      CADMaterial* material = _cadNode->GetActiveAttribute().GetMaterial();
      CADOpacitySliderDlg opacityDlg(this,-1,_cadNode->GetID(),_cadNode->GetActiveAttribute().GetMaterial());
      if (opacityDlg.ShowModal() == (wxID_OK|wxID_CANCEL))
      {
         material->SetOpacity(opacityDlg.GetOpacity());
      }
   }
}
//////////////////////////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_showColorModeSelectDialog(wxCommandEvent& WXUNUSED(event))
{
   //We should only arrive in here if the attribute is a CADMaterial!!!!
   if(_cadNode)
   {
      wxArrayString colorModes;
      colorModes.Add( _("Ambient") );
      colorModes.Add( _("Ambient_and_Diffuse") );
      colorModes.Add( _("Diffuse") );
      colorModes.Add( _("Emissive") );
      colorModes.Add( _("Specular") );
      colorModes.Add( _("Off") );

      CADAttribute activeAttribute = _cadNode->GetActiveAttribute();
      CADMaterial* material = activeAttribute.GetMaterial();

      wxSingleChoiceDialog colorSelector(this, _T("Select Color Mode"), _T("Material Color Mode"),
                           colorModes);

      if (colorSelector.ShowModal() == wxID_OK)
      {
         //std::cout<<"Selecting color mode: "<<colorSelector.GetStringSelection()<<std::endl;
         material->SetColorMode( ConvertUnicode(colorSelector.GetStringSelection().GetData()));     

         //send the data to Xplorer
         ClearInstructions(); 
         //_commandName = std::string("CAD_ATTRIBUTE_MATERIAL_COLOR_MODE");
         _commandName = std::string("CAD_ATTRIBUTE_MATERIAL_MODE");

         ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
         nodeID->SetDataType("STRING");
         nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
         _instructions.push_back(nodeID);

         ves::open::xml::DataValuePair* componentToUpdate = new ves::open::xml::DataValuePair();
         componentToUpdate->SetDataType("STRING");
         componentToUpdate->SetData("Mode","Color");
         _instructions.push_back(componentToUpdate);

         ves::open::xml::DataValuePair* materialToUpdate = new ves::open::xml::DataValuePair();
         materialToUpdate->SetDataType("XMLOBJECT");
         materialToUpdate->SetData("Material",material);
         _instructions.push_back(materialToUpdate);

         _sendCommandsToXplorer();
      }
   }
}
//////////////////////////////////////////////////////////////////
void CADNodePropertiesDlg::_showColorDialog(wxCommandEvent& event)
{
   //We should only arrive in here if the attribute is a CADMaterial!!!!
   if(_cadNode)
   {
      CADAttribute* activeAttribute = &_cadNode->GetActiveAttribute();
      CADMaterial* material = activeAttribute->GetMaterial();
      ves::open::xml::FloatArray* activeComponent = 0;
      std::string updateComponent = "";

      std::vector<double> currentColor;
      wxColour color;

      unsigned char R = 0;
      unsigned char G = 0;
      unsigned char B = 0;

      //get the current color of the material
      if(event.GetId() == CADMaterialEditMenu::DIFFUSE_ID)
      {
         activeComponent = material->GetDiffuse();
         updateComponent = "Diffuse";
      }
      else if(event.GetId() == CADMaterialEditMenu::AMBIENT_ID)
      {
         activeComponent = material->GetAmbient();
         updateComponent = "Ambient";
      }
      else if(event.GetId() == CADMaterialEditMenu::EMISSIVE_ID)
      {
         activeComponent = material->GetEmissive();
         updateComponent = "Emissive";
      }
      else if(event.GetId() == CADMaterialEditMenu::SPECULAR_ID)
      {
         activeComponent = material->GetSpecular();
         updateComponent = "Specular";
      }

      //convert to wx compatible color
      currentColor = activeComponent->GetArray();
      R = _convertToUnsignedCharColor(currentColor.at(0));
      G = _convertToUnsignedCharColor(currentColor.at(1));
      B = _convertToUnsignedCharColor(currentColor.at(2));

      color.Set(R,G,B);

      //this is kinda confusing...thanks wx!!!
      wxColourData data;
      data.SetChooseFull(true);
      data.SetColour(color);

      wxColourDialog colorDlg(this,&data);

      colorDlg.SetTitle(wxString(updateComponent.c_str(),wxConvUTF8));
      if (colorDlg.ShowModal() == wxID_OK)
      {
         wxColourData retData = colorDlg.GetColourData();
         wxColour col = retData.GetColour();
          
         //set the color on the material to the user selected color
         currentColor.at(0) = _convertToDoubleColor(col.Red());
         currentColor.at(1) = _convertToDoubleColor(col.Green());
         currentColor.at(2) = _convertToDoubleColor(col.Blue());

         material->SetComponent(updateComponent,currentColor);
         //send the data to Xplorer
         ClearInstructions(); 
         _commandName = std::string("CAD_ATTRIBUTE_MATERIAL_UPDATE");

         ves::open::xml::DataValuePair* nodeID = new ves::open::xml::DataValuePair();
         nodeID->SetData(std::string("Node ID"),_cadNode->GetID());
         _instructions.push_back(nodeID);

         ves::open::xml::DataValuePair* componentToUpdate = new ves::open::xml::DataValuePair();
         componentToUpdate->SetDataType("STRING");
         componentToUpdate->SetData("Material Component",updateComponent);
         _instructions.push_back(componentToUpdate);

         ves::open::xml::DataValuePair* materialToUpdate = new ves::open::xml::DataValuePair();
         materialToUpdate->SetDataType("XMLOBJECT");
         materialToUpdate->SetData("Material",material);
         _instructions.push_back(materialToUpdate);

         _sendCommandsToXplorer();

      }
   }

}
#ifndef STAND_ALONE
///////////////////////////////////////////////////
void CADNodePropertiesDlg::_sendCommandsToXplorer()
{
   //std::cout<<"---Sending commands to Xplorer---"<<std::endl;
   ves::open::xml::Command* cadCommand = new ves::open::xml::Command();

   for(size_t i =0; i < _instructions.size(); i++)
   {
      cadCommand->AddDataValuePair(_instructions.at(i));
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
      //delete [] tempDoc;
   }
   //Clean up memory
   delete cadCommand;
}
#endif

