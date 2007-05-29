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

#ifndef TEXTURE_BASED_TOOL_BAR_H
#define TEXTURE_BASED_TOOL_BAR_H
/*!\file TextureBasedToolBar.h
TextureBasedToolBar API
*/
/*!\class TextureBasedToolBar
* 
*/
namespace VE_XML 
{
   class DataValuePair;
}

#include <wx/dialog.h>
#include <vector>
#include <string>

#include "VE_Conductor/Utilities/BaseDialog.h"
#include "VE_Installer/include/VEConfig.h"
class UI_TransientDialog;
class wxArrayString;
class wxToolBar;
class wxComboBox;
class wxCheckBox;


class ScalarToolsDialog;
class TransferFunctionDialog;
namespace VE_Conductor
{
   namespace GUI_Utilities
   {
      class ROIDialog;
   }

}
class VE_CONDUCTOR_UTILS_EXPORTS TextureBasedToolBar : public VE_Conductor::GUI_Utilities::BaseDialog
{
public:
   ///Constructor
   TextureBasedToolBar(wxWindow* parent, int id/*,
                     wxArrayString scalarNames,
                     wxArrayString vectorNames*/);

   ///Destructor
   virtual ~TextureBasedToolBar();

   enum TBTOOLBAR_IDS
   {
      ACTIVE_SOLUTION ,///<Active scalar/vector ID
      SCALAR_ID,///<Scalar ID
      VECTOR_ID,///<Vector ID
      ROI_ID,///<Region of interest ID
      TRANSFER_FUNCS_ID,///<Transfer function ID
      TB_TOOLBAR,///<Toolbar ID
      BBOX_CHECK_BOX,///<Bounding box check
      TRANSIENT_BUTTON///<Transient controls
   };

   ///Set the scalars
   void SetScalars(wxArrayString scalarNames);
   
   ///Set the vectors
   void SetVectors(wxArrayString vectorNames);

   ///Set the size for the subdialogs
   ///\param subSize wxRect size for sub dialogs.\n
   ///Should be the same as vistab
   void SetSubDialogSize(wxRect subSize);

   ///Activate the texture based visualization
   bool ActivateTextureVisualization();
   
protected:
   wxToolBar* _tbToolButtons;///<The toolbar buttons;
   wxComboBox* _solutionSelection;///<The list of active solutions.
   //wxCheckBox* _bboxCheckBox;///<The check box to activate the Bounding Box
   UI_TransientDialog* _transientControls;///<The "radio-like" controls for transient visualiation.
   VE_Conductor::GUI_Utilities::ROIDialog* _roiDlg;///<The ROI dialog;
   ScalarToolsDialog* _scalarToolsDlg;///<The scalar tools dialog;
   TransferFunctionDialog* _transferFunctionDlg;///<The scalar tools dialog;
  
  
   wxArrayString _availableScalars;///<Scalar names
   wxArrayString _availableVectors;///<Vector names

   wxRect _subDialogSize;//<The size for the subdialogs.
   ///Build the toolbar.  
   void _buildGUI();

   ///update the displayed available solutions
   ///\param activeSolutions The active solutions names
   void _updateSolutionList(wxArrayString activeSolutions);

   ///update the list of scalars and vectors
   ///\param scalarNames Scalar names
   ///\param vectorNames Vector names
   void _updateAvailableSolutions(wxArrayString scalarNames,
                               wxArrayString vectorNames);
   
   ///Handle tool button presses
   ///\param event wxCommand event
   void _handleToolButtons(wxCommandEvent& event);
   
   ///Handle bounding box check box event
   ///\param event wxCommand event
   //void _onBBoxCheck(wxCommandEvent& event);
   
   ///Launch transient controls
   ///\param event wxCommand event
   void _onTransient(wxCommandEvent& event);
   
   
   
   DECLARE_EVENT_TABLE()
};
#endif// TEXTURE_BASED_TOOL_BAR_H
