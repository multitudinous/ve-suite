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
 * Date modified: $Date: 2007-05-09 14:51:35 -0500 (Wed, 09 May 2007) $
 * Version:       $Rev: 7579 $
 * Author:        $Author: jbkoch $
 * Id:            $Id: MainToolBar.h 7579 2007-05-09 19:51:35Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MAIN_TOOL_BAR_H
#define MAIN_TOOL_BAR_H

/*!\file MainToolBar.h
*/

/*!\class MainToolBar
* 
*/

// --- VE-Suite Includes --- //
namespace VE_Conductor
{
    class CORBAServiceList;
}

// --- wxWidgets Includes --- //
#include <wx/toolbar.h>

class MainToolBar : public wxToolBar
{
public:
    ///Constructor
    ///\param parent The parent of the toolbar
    MainToolBar( wxWindow* parent );

    ///Destructor
    ~MainToolBar();

    ///\enum The enums for MainToolBar
    enum 
    {
        TOOLBAR_SELECTION,///<ID for cursor tool
        TOOLBAR_NAVIGATION,///<ID for navigation tool

        TOOLBAR_OBJECT_TRANSLATE,///<ID for object translation tool
        TOOLBAR_OBJECT_ROTATE,///<ID for object rotation tool
        TOOLBAR_OBJECT_SCALE,///<ID for object scale tool

        TOOLBAR_PHYSICS,///<ID for physics simulation tool
        TOOLBAR_RESET,///<ID for reset simulation tool
        TOOLBAR_PAUSE,///<ID for pause simulation tool
        TOOLBAR_PLAY,///<ID for start simulation tool
        TOOLBAR_STEP,///<ID for step simulation tool

        TOOLBAR_SUMMIT_JOB///<ID for summit job tool
    };

    ///Adds the tools to the toolbar
    void CreateMainToolBar();

private:
    ///Handles events for changing xplorer device mode
    ///\param event The wxCommand event
    void OnChangeDeviceMode( wxCommandEvent& event );

    ///Handles events for the physics simulation
    ///\param event The wxCommand event
    void OnPhysicsSimulation( wxCommandEvent& event );

    ///Handles event for summit job
    ///\param event The wxCommand event
    void OnSummitJob( wxCommandEvent& event );

    DECLARE_EVENT_TABLE()
};

#endif //MAIN_TOOL_BAR_H
