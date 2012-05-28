/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
#ifndef ADUOPLUGIN_H
#define ADUOPLUGIN_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/event.h>
#include <wx/image.h>

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file ADUOPlugin.h
  Aspen Dynamics Unit Operations Plugin
  */
/*!\class ves::conductor::ADUOPlugin
 * This class is the Aspen Dynamics Unit Operations plugin.
 */
class ADUOPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( ADUOPlugin )

public:
    ///Constructor
    ADUOPlugin();

    ///Destructor
    wxString GetConductorName();

    ///???
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );
    virtual bool ShowAvailable();

    ///???
    void DrawPlugin( wxDC* dc );

protected:
    ///???
    std::string m_unitName;
    ///???
    ves::open::xml::DataValuePairPtr vendorData;

private:
    bool m_monValueExists;
    bool m_monitoring;
    std::string m_monValue;
    wxMenu* mAspenMenu;

    ///???
    virtual ~ADUOPlugin();

    ///???
    void OnShowAspenName( wxCommandEvent& event );

    ///???
    void OnQueryDynamics( wxCommandEvent& event );

    ///???
    void OnTimer( wxTimerEvent& event );

    ///???
    void StopTimer( wxCommandEvent& event );

    ///???
    void DrawValue( wxDC* dc );

    ///???
    void ReadValue( );

    ///???
    void OnMonitorVariable( wxCommandEvent& event );

    DECLARE_EVENT_TABLE();
};
}
}

#endif
