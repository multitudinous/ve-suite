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
#ifndef DWPLUGIN_H
#define DWPLUGIN_H

#include <ves/conductor/UIPluginBase.h>

#include <wx/event.h>

class wxMenu;

namespace ves
{
namespace conductor
{

/*!\file DWPlugin.h
  DWSIM Run Dialog
  */
/*!\class ves::conductor::DWPlugin
 * This class is the DWSIM plugin.
 */
class DWPlugin : public UIPluginBase
{
    DECLARE_DYNAMIC_CLASS( DWPlugin )

public:

    ///Constructor
    DWPlugin();

    ///Destructor
    virtual ~DWPlugin();

    ///???
    wxString GetConductorName();

private:
    wxMenu* mMenu;
    bool mIsSheetOpen;

    ///???
    void OnOpen( wxCommandEvent& event );

    ///???
    void ShowSimulation( wxCommandEvent& event );

    ///???
    void HideSimulation( wxCommandEvent& event );

    ///???
    void CloseSimulation( void );

    ///???
    void RunSimulation( wxCommandEvent& event );

    ///???
    void ReinitializeSimulation( wxCommandEvent& event );

    ///???
    void StepNetwork( wxCommandEvent& event );

    ///???
    void SaveSimulation( wxCommandEvent& event );

    ///???
    void SaveAsSimulation( wxCommandEvent& event );

    ///???
    void OnCloseSimulation( wxCommandEvent& event );

    ///???
    void GetInputs( wxCommandEvent& event );
    
    ///???
    void SetInputs( wxCommandEvent& event );
    
    ///???
    void GetOutputs( wxCommandEvent& event );
    
    ///???
    virtual wxMenu* GetPluginPopupMenu( wxMenu* baseMenu );

    typedef struct                                                     
    {
      std::string id;
      std::string name;
      double value;
      std::string unit;
    }prop;

    DECLARE_EVENT_TABLE()
};

}
}

#endif
