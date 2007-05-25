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
 * Date modified: $Date: 2006-12-29 23:16:22 -0600 (Fri, 29 Dec 2006) $
 * Version:       $Rev: 6416 $
 * Author:        $Author: mccdo $
 * Id:            $Id: TransferFunctionDlg.h 6416 2006-12-30 05:16:22Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TRANSFER_FUNCTION_DIALOG_H
#define TRANSFER_FUNCTION_DIALOG_H
/*!\file TransferFunctionDialog.h
  TransferFunctionDialog API
  */
/*!\class TransferFunctionDialog
 * GUI class to set the transfer function.
 */

#include <string>
#include "VE_Installer/include/VEConfig.h"
#include "VE_Conductor/GUIPlugin/BaseDialog.h"

class wxComboBox;
class wxCheckBox;

class TransferFunctionDialog : public VE_Conductor::GUI_Utilities::BaseDialog 
{
public:
   ///Constructor
   TransferFunctionDialog(wxWindow* parent, int id,std::string title);
   ///Destructor
   virtual ~TransferFunctionDialog();
   
   enum TRANSFER_FUNCTION_IDS
   {
      AVAILABLE_SHADER_MANAGERS,
      PHONG_ENABLE_CHECK
   };


protected:
   ///Add the controls to the dialog
   virtual void _buildGUI();

   ///Update the active scalar shader manager
   ///\param command The wxCommandEvent
   void _updateActiveScalarShaderManager(wxCommandEvent& command);

   ///Enable/Disable phong lighting 
   ///\param command The wxCommandEvent
   void _onEnablePhongLighting(wxCommandEvent& command);

   wxCheckBox* _phongShadingCheck;///<The phong shading check box.
   wxComboBox* _shaderManagerSelection;///<The available shader manager.
   

   DECLARE_EVENT_TABLE()
};

#endif// TRANSFER_FUNCTION_DIALOG_H
