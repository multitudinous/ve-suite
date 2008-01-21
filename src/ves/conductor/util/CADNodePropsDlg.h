/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef CAD_PROPERTIES_DIALOG_H
#define CAD_PROPERTIES_DIALOG_H
/*!\file CADNodePropsDlg.h
CADNodePropsDlg API
*/
/*!\class ves::conductor::util::CADNodePropsDlg
*
*/
#ifndef STAND_ALONE
#include <ves/open/VjObsC.h>
#include <ves/VEConfig.h>
#endif
#include <vector>
#include <string>
#include <wx/window.h>
#include <wx/dialog.h>
#include <wx/spinctrl.h>
class wxBoxSizer;
class wxNotebook;
class wxButton;
class wxPanel;
class wxTextCtrl;
class wxComboBox;
class wxListBox;
class wxListEvent;
class wxListCtrl;
class wxArrayString;
class wxCheckBox;
class wxRadioBox;

namespace ves
{
namespace open
{
namespace xml
{
class DataValuePair;
namespace cad
{
class CADNode;
}
}
}
}

namespace ves
{
namespace conductor
{
namespace util
{
class wxSpinCtrlDbl;

class VE_CONDUCTOR_UTILS_EXPORTS CADNodePropertiesDlg : public wxDialog
{
public:
    enum CAD_PROPERTY_IDS
    {
        ACTIVE_ATTRIBUTE,///<The active attribute ID.
        ATTRIBUTE_PANEL_ID,///<The attribute panel ID.
        ATTRIBUTE_TYPE,///<The attribute type ID.
        TRANSFORM_PANEL_ID,///<The transform panel ID.
        PHYSICS_PANEL_ID,///<The physics panel ID.
        PHYSICS_MASS_ID,///<The physics mass property ID.
        PHYSICS_FRICTION_ID,///<The physics friction property ID.
        PHYSICS_RESTITUTION_ID,///<The physics restitution property ID.
        PHYSICS_MESH_ID,///<The physics mesh ID.
        EDIT_ATTRIBUTE,///<The edit attribute ID.
        REMOVE_ATTRIBUTE,///<The remove attribute ID.
        RESTORE_DEFAULT_ATTRIBUTE,///<The restore attribute ID.
        ADD_ATTRIBUTE,///<The add attribute button ID.
        ACTIVE_ANIMATION,///<The active attribute ID.
        ANIMATION_PANEL_ID,///<The animation panel ID.
        EDIT_ANIMATION,///<The edit attribute ID.
        REMOVE_ANIMATION,///<The remove attribute ID.
        ADD_ANIMATION,///<The add attribute button ID.
        UNIFORM_SCALE///<The scale uniformly checkbox ID.
    };
    ///Constructor
    ///\param parent The parent window.
    ///\param id The ID for the dialog.
    ///\param node The CADNode to display properties of.
    CADNodePropertiesDlg( wxWindow* parent, int id, ves::open::xml::cad::CADNode* node );

    ///Destructor
    virtual ~CADNodePropertiesDlg();

    ///Check for existing attribute name.
    ///\param attributeName The name to check for.
    bool AttributeExists( std::string attributeName );

    ///Check for existing animation name.
    ///\param name The name to check for.
    bool AnimationExists( wxString name );

    ///Clear out the current queue of instructions.
    void ClearInstructions();

    ///Return the transform panel.
    wxPanel* GetTransformPanel();

    ///Return the attribute panel.
    wxPanel* GetAttributePanel();

    ///Return the physics panel.
    wxPanel* GetPhysicsPanel();

    ///Return the Animation Panel
    wxPanel* GetAnimationPanel();
protected:
    ///Internally build the GUI.
    void _buildGUI();

    ///Build the tabs
    void _buildTabs();

    ///Build the transform panel.
    void _buildTransformPanel();

    ///Build the transform panel.
    void _buildAttributePanel();

    ///Build the physics panel.
    void _buildPhysicsPanel();

    ///Build the animation panel.
    void _buildAnimationPanel();

    ///Update the transform of a node
    ///\param event The wxCommand event.
    void _updateTransform( wxSpinEvent& event );

    ///Update the physics properties of a node
    ///\param event The wxCommand event.
    void _updatePhysicsProperties( wxSpinEvent& event );

    ///Update the physics mesh of a node
    ///\param event The wxCommand event.
    void _updatePhysicsMesh( wxCommandEvent& event );

    ///Update whether uniform scaling is used
    ///\param event The wxCommand event
    void UpdateUniformScale( wxCommandEvent& event );

    ///Update the attribute type and available attributes.
    ///\param event The wxCommand event.
    void _updateAttributeType( wxCommandEvent& event );

    ///Update the active attribute.
    ///\param event The wxListEvent event.
    void _setActiveAttribute( wxListEvent& event );

    ///Edit an attribute.
    ///\param event The wxList event.
    void _editAttribute( wxListEvent& event );

    ///Add an attribute to the node.
    ///\param event The wxCommand event.
    void _addAttribute( wxCommandEvent& event );

    ///Remove an attribute from the node.
    ///\param event The wxCommand event.
    void _removeAttribute( wxCommandEvent& event );

    ///Remove an attribute from the node.
    ///\param event The wxCommand event.
    void _restoreDefaultAttribute( wxCommandEvent& event );

    ///Show the color selector dialog
    ///\param event wxCommand event
    void _showColorDialog( wxCommandEvent& event );

    ///Show the opacity dialog
    ///\param event wxCommand event
    void _showOpacityDialog( wxCommandEvent& event );

    ///Show the face selection mode dialog
    ///\param event wxCommand event
    void _showFaceSelectDialog( wxCommandEvent& event );

    ///Show the color mode selection dialog
    ///\param event wxCommand event
    void _showColorModeSelectDialog( wxCommandEvent& event );

    ///Update the attribute dialog
    ///\param attributes The list of attributes to set in the dialog.
    void _updateAttributeList( wxArrayString attributes );

    ///Update the available attributes for this CADNode.
    void _updateAvailableAttributes();

    ///Add an animation to the node.
    ///\param event The wxCommand event.
    void _addAnimation( wxCommandEvent& event );

    ///Remove an animation from the node.
    ///\param event The wxCommand event.
    void _removeAnimation( wxCommandEvent& event );

    ///Update the active animation.
    ///\param event The wxListEvent event.
    void _setActiveAnimation( wxListEvent& event );

    ///Update the animation dialog
    ///\param animations The list of animations to set in the dialog.
    void _updateAnimationList( wxArrayString animations );

    ///Update the available animations for this CADNode.
    void _updateAvailableAnimations();

    ///utility function to convert double to unsigned char
    ///\param value The value to convert to double
    double _convertToDoubleColor( unsigned char value );

    ///utility function to convert unsigned char to double
    ///\param value The value to convert to double
    unsigned char _convertToUnsignedCharColor( double value );

    ///Send the Command back to VE-Xplorer.
    void _sendCommandsToXplorer();

    wxNotebook* _propertyTabs;///<The tabs for modifying the node properties.
    wxPanel* _transformPanel;///<The transform panel.
    wxPanel* _attributePanel;///<The attribute panel.
    wxPanel* _physicsPanel;///<The physics panel.
    wxPanel* _animationPanel;///<The animation panel.

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
    wxCheckBox* m_uniformScale;///<Uniform scaling checkbox

    ///Attribute panel controls
    wxComboBox* _attributeType;///<The attribute type selection box.
    wxListCtrl* _attributeSelection;///<The box listing the available attributes.
    wxButton* _addAttributeButton;///<The button for adding attributes.
    wxButton* _removeAttributeButton;///<The button for adding attributes.
    wxButton* _restoreDefaultAttributeButton;///The button to restore default attributes.
    wxButton* _editAttributeButton;///<The button for removing attributes.
    wxArrayString _availableShaders;///<The shader names.
    wxArrayString _availableMaterials;///<The material names.

    ///Physics panel controls
    wxSpinCtrlDbl* _physicsMassCtrl;///<Mass control
    wxSpinCtrlDbl* _physicsFrictionCtrl;///<Friction control
    wxSpinCtrlDbl* _physicsRestitutionCtrl;///<Restitution control
    wxRadioBox* meshProperties;///<Mesh type control

    ///Animation panel controls
    wxArrayString _animationFiles;///<The animation file names.

    wxListCtrl* _animationSelection;///<The available animationFiles.
    wxButton* _addAnimationButton;///<The button for adding attributes.
    wxButton* _removeAnimationButton;///<The button for adding attributes.

    unsigned int _nMaterials;///<The number of materials.
    unsigned int _nShaders;///<The number of shaders.
    std::string _commandName;///<The command name.

    double tempX;///<The x scale value.
    double tempY;///<The y scale value.
    double tempZ;///<The z scale value.

    ves::open::xml::cad::CADNode* _cadNode;///<The current CADNode.

    std::vector<ves::open::xml::DataValuePair*> _instructions;///<The DataValuePair s for the current command.

    std::string ConvertUnicode( const wxChar* data )
    {
        std::string tempStr( static_cast< const char* >( wxConvCurrent->cWX2MB( data ) ) );
        return tempStr;
    }
    DECLARE_EVENT_TABLE()
};
}
}
}
#endif//CAD_PROPERTIES_DIALOG_H

