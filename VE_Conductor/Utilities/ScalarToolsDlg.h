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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef SCALAR_TOOLS_DIALOG_H
#define SCALAR_TOOLS_DIALOG_H
/*!\file ScalarToolsDialog.h
  ScalarToolsDialog API
  */
/*!\class ScalarToolsDialog
 * GUI class to adjust Volume Visualization clip planes .
 * Bounds are handled as %'s of the bounding box.
 */

#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/Utilities/BaseDialog.h"
#include "VE_Conductor/Utilities/DualSlider.h"

class wxComboBox;
class wxCheckBox;
class wxButton;
class wxSlider;

class VE_CONDUCTOR_UTILS_EXPORTS ScalarToolsDialog : public VE_Conductor::GUI_Utilities::BaseDialog 
{
public:
   ///Constructor
   ScalarToolsDialog(wxWindow* parent, int id,std::string title);
   ///Destructor
   virtual ~ScalarToolsDialog();
   
   enum SCALAR_TOOLS_IDS
   {
      AVAILABLE_SCALARS,
	  AVAILABLE_SHADER_MANAGERS,
      ISO_ENABLE_CHECK,
      TB_ISOSURFACE_SLIDER,
      TB_SLICE_SLIDER,
      ADVANCED_TB_ISOSURFACE
   };

   ///Set the name of the command
   ///\param name The name of the command.
   void SetCommandName(std::string name);
  
   ///Send the commands to Xplorer;
   void SendCommands();

   ///Add an instruction to send. This is for access in the callbacks.
   ///\param newInstruct The instruction to add to the Command.
   void AddInstruction(VE_XML::DataValuePair* newInstruct);

   ///Set the active scalars
   ///\param The name of the scalars
   void UpdateScalarList(wxArrayString scalarNames);
protected:
   /*!\class ScalarToolsMinSliderCallback
    *Class that allows the user to do operations based on the min slider events
    */
   class ScalarToolsSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ScalarToolsSliderCallback(ScalarToolsDialog* parent)
        {
            _scalarDlg = parent;
        }
        ///Destructor
        virtual ~ScalarToolsSliderCallback(){}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         ScalarToolsDialog* _scalarDlg;
   };

   /*!\class ScalarToolsStopSliderCallback
    *Class that allows the user to do operations based on the min slider events
    */
   class ScalarToolsStopSliderCallback:
          public VE_Conductor::GUI_Utilities::DualSlider::SliderCallback
   {
      public:
        ///Constructors
        ScalarToolsStopSliderCallback(ScalarToolsDialog* parent)
        {
            _scalarDlg = parent;
        }
        ///Destructor
        virtual ~ScalarToolsStopSliderCallback(){}
        
        ///The operation to do for the slider
        virtual void SliderOperation();      
      protected:
         ScalarToolsDialog* _scalarDlg;
   };
   ///Build the DualSlider s for this dialog
   void _createDualSliders();
   ///Add the controls to the dialog
   virtual void _buildGUI();

   ///Update the active scalar
   ///\param command The wxCommandEvent
   void _updateActiveScalar(wxCommandEvent& command);
   
   ///Update the active scalar shader manager
   ///\param command The wxCommandEvent
   void _updateActiveScalarShaderManager(wxCommandEvent& command);

   ///Update the scalar to color the iso-surface by
   ///\param command The wxCommandEvent
   void _setColorByFace(wxCommandEvent& command);

   ///Update the isosurface
   ///\param command The wxScrollEvent
   void _onUpdateIsosurface(wxScrollEvent& command);

   ///Set the number of slice planes per brick
   ///\param command The wxScrollEvent
   void _onUpdateNumberOfSlicePlanes(wxScrollEvent& command);
   ///Enable/Disable isosurface visualization
   ///\param command The wxCommandEvent
   void _onEnableIsoSurface(wxCommandEvent& command);

   ///Fully update the pre-integration table
   void _onPreIntegrate(wxScrollEvent& command);

   wxString _activeScalar;///<The name of the scalar we are working on.
   wxString _colorByScalarName;///<The name of the scalar to color by.

   wxButton* _advancedButton;///<The advanced iso surface button.
   wxCheckBox* _isosurfaceCheck;///<The iso surface check box.
   wxSlider* _isoSlider;///<The isosurface slider.
   wxSlider* _numSlicesSlider;///<The number of slices slider.
   wxComboBox* _scalarSelection;///<The available scalars.
   wxComboBox* _shaderManagerSelection;///<The available shader manager.
   VE_Conductor::GUI_Utilities::DualSlider* _scalarRange;///<DualSlider for x bounds

   DECLARE_EVENT_TABLE()
};

#endif// SCALAR_TOOLS_DIALOG_H
