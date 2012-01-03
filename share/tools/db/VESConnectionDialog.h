/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef VES_CONNECTION_DIALOG_H
#define VES_CONNECTION_DIALOG_H

// --- VE-Suite Includes --- //
class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/dialog.h>

class wxDirPickerCtrl;
class wxTextCtrl;

// --- C/C++ Includes --- //
#include <string>

/*!\file VESConnectionDialog.h
 *
 */

/*!\class VESConnectionDialog
 * wx dialog for connecting to VE_Suite
 */
class VESConnectionDialog : public wxDialog
{
public:
    ///Constructor
    VESConnectionDialog( wxWindow* parent );

    ///Destructor
    virtual ~VESConnectionDialog();

protected:

private:
    ///
    void CreateGUI();

    ///
    void Connect( wxCommandEvent& event );

    ///
    void Clear( wxCommandEvent& event );

    ///
    AppFrame* m_appFrame;

    ///
    wxDirPickerCtrl* m_workingDirPickerCtrl;

    ///
	wxTextCtrl* m_ceServerHostTextCtrl;

    ///
	wxTextCtrl* m_cePortTextCtrl;

    DECLARE_EVENT_TABLE();

};

#endif //VES_CONNECTION_DIALOG_H
