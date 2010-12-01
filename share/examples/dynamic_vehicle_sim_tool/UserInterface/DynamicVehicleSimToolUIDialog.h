/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef DYNAMIC_VEHICLE_SIM_UI_DIALOG_H
#define DYNAMIC_VEHICLE_SIM_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include "DynamicVehicleSimToolBase.h"

namespace ves
{
namespace conductor
{
namespace util
{
class CORBAServiceList;
}
}
}

// --- wxWidgets Includes --- //
class wxChoice;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

namespace dynamicvehicletool
{
class DynamicVehicleSimToolUIDialog : public dvst::DynamicVehicleSimToolBase
{
public:
    DynamicVehicleSimToolUIDialog();
    DynamicVehicleSimToolUIDialog(
        wxWindow* parent,
        int id,
        ves::conductor::util::CORBAServiceList* service );

    virtual ~DynamicVehicleSimToolUIDialog();

private:
    void UpdateModelData();
    
    void PopulateDialogs();
    
    ves::conductor::util::CORBAServiceList* mServiceList;

    ///List of geom cadnode names
    std::vector< wxChoice* > m_geomChoiceList;
    
protected:
	// Handlers for DynamicVehicleSimTool events.
	void OnComputerNameEnter( wxCommandEvent& event );
	void OnPortNumberEnter( wxCommandEvent& event );
	void OnStartStopButton( wxCommandEvent& event );
	void OnResetSimulation( wxCommandEvent& event );
	void OnGeometryDataMapping( wxCommandEvent& event );
	void OnAddGeometryGroupButton( wxCommandEvent& event );
	void OnRemoveGeometryGroupButton( wxCommandEvent& event );
	void OnConstrainedGeometrySelection( wxCommandEvent& event );
	void OnApplyButton( wxCommandEvent& event );
	void OnOKButton( wxCommandEvent& event );
};

} //end warrantytool

#endif //DYNAMIC_VEHICLE_SIM_UI_DIALOG_H
