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
#ifndef OpcPlugin_H
#define OpcPlugin_H

#include <ves/conductor/UIPluginBase.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <wx/event.h>
#include <wx/image.h>

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file OpcPlugin.h
  OPC Unit Operations Plugin
  */
/*!\class ves::conductor::OpcPlugin
 * This class is the OPC Unit Operations plugin.
 */
class OpcPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( OpcPlugin )

public:
    ///Constructor
    OpcPlugin();
    
    ///Destructor
    virtual ~OpcPlugin();
    
    ///???
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
    std::string m_monValue;
    bool m_monValueExists;
    bool m_monitoring;
    wxMenu* mOpcMenu;

    ///???
    void DrawValue( wxDC* dc );
    
    ///???
    void ReadValue( );

    ///???
    //void OnShowValue( wxCommandEvent& event );
    
    ///???
    void OnShowAllVar( wxCommandEvent& event );
    
    ///???
    void QueryForAllVariables( wxCommandEvent& event );
    
    ///???
    void OnMonitorVariable( wxCommandEvent& event );

    ///???
    //void OnValveCAD( wxCommandEvent& event );

    ///???
    //void OnSwitchCAD( wxCommandEvent& event );
    
    ///???
    void OnConnect( wxCommandEvent& event );

    ///???
    //void OnTankCAD( wxCommandEvent& event );

    ///???
    void OnUnitName( wxCommandEvent& event );
    
    ///???
    void SetUnitName( std::string name );

    DECLARE_EVENT_TABLE()
};
}
}

#endif