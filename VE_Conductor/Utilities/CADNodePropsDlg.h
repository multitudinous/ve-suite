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
#ifndef STAND_ALONE
#include "VE_Open/skel/VjObsC.h"
#include "VE_Installer/include/VEConfig.h"
#endif
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
class wxListEvent;
class wxListCtrl;
class wxArrayString;

namespace VE_XML
{
   class DataValuePair;
}
namespace VE_CAD
{
   class CADNode;
}
namespace VE_Conductor
{
namespace GUI_Utilities
{
class VE_CONDUCTOR_UTILS_EXPORTS CADNodePropertiesDlg : public wxDialog
{
public:
   enum CAD_PROPERTY_IDS
   {
      ACTIVE_ATTRIBUTE,///<The active attribute ID.
      ATTRIBUTE_PANEL_ID,///<The attribute panel ID.
      ATTRIBUTE_TYPE,///<The attribute type ID.
      TRANSFORM_PANEL_ID,///<The transform panel ID.
      EDIT_ATTRIBUTE,///<The edit attribute ID.
      REMOVE_ATTRIBUTE,///<The remove attribute ID.
      ADD_ATTRIBUTE///<The add attribute button ID.
   };
   ///Constructor
   ///\param parent The parent window.
   ///\param id The ID for the dialog.
   ///\param node The CADNode to display properties of.
   CADNodePropertiesDlg( wxWindow* parent, int id, VE_CAD::CADNode* node);

   ///Destructor
   virtual ~CADNodePropertiesDlg();

   ///Check for existing attribute name. 
   ///\param attributeName The name to check for.
   bool AttributeExists(std::string attributeName);

#ifndef STAND_ALONE
   ///Set the current vjObjs ptr for data passing.
   ///\param xplorerCom The communication interface w/ xplorer.
   void SetVjObsPtr(VjObs_ptr xplorerCom);
#endif

   ///Clear out the current queue of instructions.
   void ClearInstructions();

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

   ///Update the active attribute.
   ///\param event The wxListEvent event.
   void _setActiveAttribute(wxListEvent& event);

   ///Edit an attribute.
   ///\param event The wxList event.
   void _editAttribute(wxListEvent& event);

   ///Add an attribute to the node.
   ///\param event The wxCommand event.
   void _addAttribute(wxCommandEvent& event);

   ///Remove an attribute from the node.
   ///\param event The wxCommand event.
   void _removeAttribute(wxCommandEvent& event);

   ///Show the color selector dialog
   ///\param event wxCommand event
   void _showColorDialog(wxCommandEvent& event);
   
   ///Show the opacity dialog
   ///\param event wxCommand event
   void _showOpacityDialog(wxCommandEvent& event);

   ///Show the face selection mode dialog
   ///\param event wxCommand event
   void _showFaceSelectDialog(wxCommandEvent& event);

   ///Show the color mode selection dialog
   ///\param event wxCommand event
   void _showColorModeSelectDialog(wxCommandEvent& event);

   ///Update the attribute dialog
   ///\param attributes The list of attributes to set in the dialog.
   void _updateAttributeList(wxArrayString attributes);

   ///Update the available attributes for this CADNode.
   void _updateAvailableAttributes();

   ///utility function to convert double to unsigned char 
   ///\param value The value to convert to double
   double _convertToDoubleColor(unsigned char value);

   ///utility function to convert unsigned char to double
   ///\param value The value to convert to double
   unsigned char _convertToUnsignedCharColor(double value);
#ifndef STAND_ALONE
   VjObs_ptr _vjObsPtr;///<The VjObj ptr.
   
   ///Send the Command back to VE-Xplorer.
   void _sendCommandsToXplorer();
#endif

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
   wxListCtrl* _attributeSelection;///<The box listing the available attributes.
   wxButton* _addAttributeButton;///<The button for adding attributes.
   wxButton* _removeAttributeButton;///<The button for adding attributes.
   wxButton* _editAttributeButton;///<The button for removing attributes.
   wxArrayString _availableShaders;///<The shader names.
   wxArrayString _availableMaterials;///<The material names.

   unsigned int _nMaterials;///<The number of materials.
   unsigned int _nShaders;///<The number of shaders.
   std::string _commandName;///<The command name.

   VE_CAD::CADNode* _cadNode;///<The current CADNode.

   std::vector<VE_XML::DataValuePair*> _instructions;///<The DataValuePair s for the current command.

   DECLARE_EVENT_TABLE()

};
}
}
#endif//CAD_PROPERTIES_DIALOG_H

