/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef DSPLUGIN_H
#define DSPLUGIN_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/event.h>
#include <wx/wx.h>

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file DSPlugin.h
  DynSim Plugin
  */
/*!\class ves::conductor::DSPlugin
 * This class is DynSim Plugin.
 */
class DSPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( DSPlugin )

public:
    ///Constructor
    DSPlugin();

    ///Destructor
    virtual ~DSPlugin();

    ///???
    wxString GetConductorName();
    
    ///???
    std::vector< std::string > GetAvailableVariables();
    
    ///???
    std::vector< std::string > GetSelectVariables();
    
    ///???
    void SetSelectVariables( std::vector< std::string> selectedVariables );

protected:
    wxMenu* mDynSimMenu;
    std::vector< std::string > m_opcList;
    std::vector< std::string > m_selectedOpcList;
    wxTimer * m_timer;
    
    ///???
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );
    
    ///???
    void OnOpen( wxCommandEvent& event );
    
    ///???
    void OnCreateOPCList( wxCommandEvent& event );
    
    ///???
    void OnConnect( wxCommandEvent& event );
    
    ///???
    void OnAddVariable( wxCommandEvent& event );
    
    ///???
    void OnTimer( wxTimerEvent& event );
    
    ///???
    void QueryForAllVariables( wxCommandEvent& event );

    DECLARE_EVENT_TABLE()
};

}
}

#endif
