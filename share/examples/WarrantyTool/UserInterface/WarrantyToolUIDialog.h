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

#ifndef WARRANTY_TOOL_UI_DIALOG_H
#define WARRANTY_TOOL_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

#include <ves/conductor/util/DualSlider.h>

namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
class wxSpinCtrlDbl;
}
}
}

// --- wxWidgets Includes --- //
class wxRadioBox;
class wxSlider;
class wxSpinCtrl;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace warrantytool
{
class WarrantyToolUIDialog : public ves::conductor::UIDialog
{
public:
    WarrantyToolUIDialog();
    WarrantyToolUIDialog(
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    virtual ~WarrantyToolUIDialog();

protected:

private:
    void WarrantyToolUIDialog::GetTextInput( wxCommandEvent& event );

    void BuildGUI();

    void SendCommandsToXplorer();

    ves::conductor::util::CORBAServiceList* mServiceList;
    
    DECLARE_EVENT_TABLE()
};

} //end warrantytool

#endif //WARRANTY_TOOL_UI_DIALOG_H
