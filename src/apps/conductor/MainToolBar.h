/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef MAIN_TOOL_BAR_H
#define MAIN_TOOL_BAR_H

/*!\file MainToolBar.h
 */

/*!\class MainToolBar
 *
 */

// --- wxWidgets Includes --- //
#include <wx/toolbar.h>

// --- C/C++ Libraries --- //
#include <map>
#include <string>

class MainToolBar : public wxToolBar
{
public:
    ///Constructor
    ///\param parent The parent of the toolbar
    MainToolBar( wxWindow* parent );

    ///Destructor
    virtual ~MainToolBar();

    ///\enum The enums for MainToolBar
    enum
    {
        TOOLBAR_NEW,///<ID for new tool
        TOOLBAR_OPEN,///<ID for open tool
        TOOLBAR_SAVE,///<ID for save tool

        TOOLBAR_SELECTION,///<ID for select tool
        TOOLBAR_WORLD_NAVIGATION,///<ID for world navigation tool
        TOOLBAR_OBJECT_NAVIGATION,///<ID for object navigation tool
        TOOLBAR_UNSELECT,///<ID for unselect tool

        TOOLBAR_SMALL_CENTERPOINT_JUMP,///<ID for fixed centerpoint small jump setting
        TOOLBAR_MEDIUM_CENTERPOINT_JUMP,///<ID for fixed centerpoint medium jump setting
        TOOLBAR_LARGE_CENTERPOINT_JUMP,///<ID for fixed centerpoint large jump setting
        TOOLBAR_BB_CENTERPOINT_JUMP,///<ID for centerpoint boundingbox jump setting

        TOOLBAR_PHYSICS,///<ID for physics simulation tool
        TOOLBAR_RESET,///<ID for reset simulation tool
        TOOLBAR_PAUSE,///<ID for pause simulation tool
        TOOLBAR_PLAY,///<ID for start simulation tool
        TOOLBAR_STEP,///<ID for step simulation tool

        TOOLBAR_SUMMIT_JOB///<ID for summit job tool
    };

private:
    ///Loads and stores the xpm images into a std::map for this toolbar
    void LoadToolBarBitmaps();

    ///Adds the tools to the toolbar
    void CreateMainToolBar();

    ///Handles event for new
    ///\param event The wxCommand event
    void OnNew( wxCommandEvent& event );

    ///Handles event for open
    ///\param event The wxCommand event
    void OnOpen( wxCommandEvent& event );

    ///Handles event for save
    ///\param event The wxCommand event
    void OnSave( wxCommandEvent& event );

    ///Handles events for changing xplorer device mode
    ///\param event The wxCommand event
    void OnChangeDeviceMode( wxCommandEvent& event );

    ///Handles events for changing the centerpoint jump distance
    ///\param event The wxCommand event
    void OnChangeCenterPointJump( wxCommandEvent& event );

    ///Handles the event to unselect all objects in xplorer
    ///\param event The wxCommand event
    void OnUnselectObjects( wxCommandEvent& event );

    ///Handles event for physics state
    ///\param event The wxCommand event
    void OnPhysicsState( wxCommandEvent& event );

    ///Handles events for the physics simulation
    ///\param event The wxCommand event
    void OnPhysicsSimulation( wxCommandEvent& event );

    ///Handles event for summit job
    ///\param event The wxCommand event
    void OnSummitJob( wxCommandEvent& event );

    std::map< std::string, wxBitmap > m_toolbarBitmaps;///<A map that holds the bitmaps for this toolbar

    DECLARE_EVENT_TABLE()
};

#endif //MAIN_TOOL_BAR_H
