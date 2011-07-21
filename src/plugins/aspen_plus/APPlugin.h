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
#ifndef APPLUGIN_H
#define APPLUGIN_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/event.h>

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file APPlugin.h
  Aspen Plus Plugin
  */
/*!\class ves::conductor::APPlugin
 * This class is the Aspen Plus Plugin.
 */
class APPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( APPlugin )

public:
    ///Constructor
    APPlugin();

    ///Destructor
    virtual ~APPlugin();

    ///???
    wxString GetConductorName();

protected:
    ///???
    std::string m_unitName;
    ///???
    ves::open::xml::DataValuePairPtr vendorData;

protected:
    wxMenu* mAspenMenu;

    ///???
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );

    ///???
    void OnUnitName( wxCommandEvent& event );

    ///???
    void OnOpen( wxCommandEvent& event );

    ///???
    void ShowAspenSimulation( wxCommandEvent& event );

    ///???
    void HideAspenSimulation( wxCommandEvent& event );

    ///???
    void CloseAspenSimulation( void );

    ///???
    void DisconnectAspenSimulation( void );

    ///???
    void RunAspenNetwork( wxCommandEvent& event );

    ///???
    void ReinitializeAspenSimulation( wxCommandEvent& event );

    ///???
    void StepAspenNetwork( wxCommandEvent& event );

    ///???
    void SaveSimulation( wxCommandEvent& event );

    ///???
    void SaveAsSimulation( wxCommandEvent& event );

    ///???
    void OnCloseAspenSimulation( wxCommandEvent& event );

    ///???
    void OnDisconnectAspenSimulation( wxCommandEvent& event );

    ///???
    void SetUnitName( std::string name );

    ///???
    bool IsBKPOpen();

    DECLARE_EVENT_TABLE()
};
}
}

#endif
