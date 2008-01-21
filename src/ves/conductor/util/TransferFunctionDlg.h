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
#ifndef TRANSFER_FUNCTION_DIALOG_H
#define TRANSFER_FUNCTION_DIALOG_H
/*!\file TransferFunctionDialog.h
  TransferFunctionDialog API
  */
/*!\class TransferFunctionDialog
 * GUI class to set the transfer function.
 */

#include <string>
#include <ves/VEConfig.h>
#include <ves/conductor/util/BaseDialog.h>

class wxComboBox;
class wxCheckBox;

namespace ves
{
namespace conductor
{
namespace util
{
class VE_CONDUCTOR_UTILS_EXPORTS TransferFunctionDialog : public ves::conductor::util::BaseDialog
{
public:
    ///Constructor
    TransferFunctionDialog( wxWindow* parent, int id, std::string title );
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
    void _updateActiveScalarShaderManager( wxCommandEvent& command );

    ///Enable/Disable phong lighting
    ///\param command The wxCommandEvent
    void _onEnablePhongLighting( wxCommandEvent& command );

    wxCheckBox* _phongShadingCheck;///<The phong shading check box.
    wxComboBox* _shaderManagerSelection;///<The available shader manager.


    DECLARE_EVENT_TABLE()
};
}
}
}
#endif// TRANSFER_FUNCTION_DIALOG_H
