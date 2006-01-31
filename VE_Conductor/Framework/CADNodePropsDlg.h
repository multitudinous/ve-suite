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

#ifndef CAD_PROPERTIES_DIALOG_H
#define CAD_PROPERTIES_DIALOG_H
#include <vector>
#include <string>
#include <wx/window.h>
#include <wx/dialog.h>
#include <wx/spinctrl.h>
class wxBoxSizer;
class wxNotebook;
class wxButton;
class wxSpinCtrlDbl;
class wxPanel;
class wxTextCtrl;
class wxComboBox;
class wxListBox;
class wxArrayString;

namespace VE_CAD
{
   class CADNode;
}

class CADNodePropertiesDlg : public wxDialog
{
public:
   enum CAD_PROPERTY_IDS
   {
      ACTIVE_ATTRIBTUTE,///<The active attribute ID.
      ATTRIBUTE_PANEL_ID,///<The attribute panel ID.
      ATTRIBUTE_TYPE,///<The attribute type ID.
      TRANSFORM_PANEL_ID///<The transform panel ID.
   };
   ///Constructor
   ///\param parent The parent window.
   ///\param id The ID for the dialog.
   ///\param node The CADNode to display properties of.
   CADNodePropertiesDlg( wxWindow* parent, int id, VE_CAD::CADNode* node);

   ///Destructor
   virtual ~ CADNodePropertiesDlg();

   ///Return the transform panel.
   wxPanel* GetTransformPanel();
   
   ///Return the attribute panel.
   wxPanel* GetAttributePanel();
protected:
   ///Internally build the GUI.
   void _buildGUI();

   ///Build the tabs
   void _buildTabs();

   ///Build the transform panel.
   void _buildTransformPanel();
   
   ///Build the transform panel.
   void _buildAttributePanel();

   ///Update the transform of a node
   ///\param event The wxCommand event.
   void _updateTransform(wxSpinEvent& event);

   ///Update the attribute type and available attributes.
   ///\param event The wxCommand event.
   void _updateAttributeType(wxCommandEvent& event);

   ///Update the available attributes for this CADNode.
   void _updateAvailableAttributes();

   wxNotebook* _propertyTabs;///<The tabs for modifying the node properties.
   wxPanel* _transformPanel;///<The transform panel.
   wxPanel* _attributePanel;///<The attribute panel.

   ///Transform panel controls
   wxSpinCtrlDbl* _xTransformCtrl;///<X translation control
   wxSpinCtrlDbl* _yTransformCtrl;///<Y translation control
   wxSpinCtrlDbl* _zTransformCtrl;///<Z translation control

   wxSpinCtrlDbl* _xRotationCtrl;///<X rotation control
   wxSpinCtrlDbl* _yRotationCtrl;///<Y rotation control
   wxSpinCtrlDbl* _zRotationCtrl;///<Z rotation control

   wxSpinCtrlDbl* _xScaleCtrl;///<X scale control
   wxSpinCtrlDbl* _yScaleCtrl;///<Y scale control
   wxSpinCtrlDbl* _zScaleCtrl;///<Z scale control

   ///Attribute panel controls
   wxComboBox* _attributeType;///<The attribute type selection box.
   wxListBox* _attributeSelection;///<The box listing the available attributes.
   wxButton* _addAttributeButton;///<The button for adding attributes.
   wxButton* _removeAttributeButton;///<The button for removing attributes.
   wxArrayString _availableShaders;///<The shader names.
   wxArrayString _availableMaterials;///<The material names.

   VE_CAD::CADNode* _cadNode;///<The current CADNode.


   /*void _buildTerrainUpdateButton();
   void _buildFireUpdateButton();
   void _buildTerrainPanel();
   void _buildFirePanel();
   void _onBrowseCallback(wxCommandEvent& event);
   void _updateTerrainButtonCallback(wxCommandEvent& event);
   void _updateFireButtonCallback(wxCommandEvent& event);
   void _chooseFile(int style,int fileType);
   
   wxPanel* _terrainPanel;
   wxTextCtrl* _heightMapFileCtrl;
   wxTextCtrl* _terrainTextureFileCtrl;
  
   wxButton* _heightMapBrowseButton;
   wxButton* _terrainTextureBrowseButton;

   wxButton* _updateTerrainDataButton;
   wxSpinCtrl* _treeCoverageCtrl;
   wxSpinCtrl* _minNumberOfTreesCtrl;
   wxSpinCtrl* _maxNumberOfTreesCtrl;
   wxSpinCtrl* _treeGroupDiameterCtrl;
   

   //fire tab ctrls
   wxPanel* _firePanel;
   wxButton* _updateFireDataButton;
   wxTextCtrl* _firePositionFileCtrl;
   wxButton* _fireFileBrowseButton;
   wxSpinCtrl* _fireSpreadRateCtrl;
   wxSpinCtrl* _fireDurationCtrl;
   wxSpinCtrl* _xFireOriginCtrl;
   wxSpinCtrl* _yFireOriginCtrl;
   wxSpinCtrl* _zFireOriginCtrl;*/
   DECLARE_EVENT_TABLE()

};

#endif//CAD_PROPERTIES_DIALOG_H

