/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef APP_TOOL_BAR_H
#define APP_TOOL_BAR_H

// --- VE-Suite Includes --- //
#include "ConductorAppEnums.h"

class AppFrame;

// --- wxWidgets Includes --- //
#include <wx/toolbar.h>

class wxChoice;

// --- C/C++ Libraries --- //
#include <map>
#include <string>

/*!\file AppToolBar.h
 *
 */

/*!\class AppToolBar
 *
 */
class AppToolBar : public wxToolBar
{
public:
    ///Constructor
    ///\param parent The parent of the toolbar
    AppToolBar( wxWindow* parent );

    ///Destructor
    virtual ~AppToolBar();

private:
    ///Loads and stores the xpm images into a std::map for this toolbar
    void LoadToolBarBitmaps();

    ///Adds the tools to the toolbar
    void CreateAppToolBar();

    ///Handles events for changing the center point
    ///\param event The wxCommand event
    void OnCenterPointUpdate( wxCommandEvent& event );

    ///Handles events for changing xplorer device mode
    ///\param event The wxCommand event
    void OnChangeDeviceMode( wxCommandEvent& event );

    ///
    ///\param
    void OnChangeManipulatorMode( wxCommandEvent& event );

    ///
    ///\param
    void OnEraseBackGround( wxEraseEvent& event );

    ///
    ///\param
    void OnManipulatorState( wxCommandEvent& event );

    ///Handles event for new
    ///\param event The wxCommand event
    void OnNew( wxCommandEvent& event );

    ///Handles event for open
    ///\param event The wxCommand event
    void OnOpen( wxCommandEvent& event );

    ///
    ///\param
    void OnPaint( wxPaintEvent& event );

    ///Handles events for the physics simulation
    ///\param event The wxCommand event
    void OnPhysicsSimulation( wxCommandEvent& event );

    ///Handles event for physics state
    ///\param event The wxCommand event
    void OnPhysicsState( wxCommandEvent& event );

    ///Handles events for the physics simulation
    ///\param event The wxCommand event
    void OnResetCenterPoint( wxCommandEvent& event );

    ///Handles event for save
    ///\param event The wxCommand event
    void OnSave( wxCommandEvent& event );

    ///Handles event for summit job
    ///\param event The wxCommand event
    void OnSummitJob( wxCommandEvent& event );

    ///Handles the event to unselect all objects in xplorer
    ///\param event The wxCommand event
    void OnUnselectObjects( wxCommandEvent& event );

    ///
    APP_TOOL_BAR m_prevDeviceMode;

    ///
    APP_TOOL_BAR m_prevCenterPoint;

    ///
    APP_TOOL_BAR m_prevPhysicsSimulation;

    ///
    APP_TOOL_BAR m_prevManipulatorMode;

    ///A map that holds the bitmaps for this toolbar
    std::map< APP_TOOL_BAR, wxBitmap > mToolbarBitmaps;

    ///
    wxToolBarToolBase* m_manipulatorTranslateTool;

    ///
    wxToolBarToolBase* m_manipulatorRotateTool;

    ///
    wxToolBarToolBase* m_manipulatorScaleTool;

    ///
    wxToolBarToolBase* m_manipulatorComboTool;

    ///
    wxToolBarToolBase* m_characterTool;

    ///
    wxToolBarToolBase* m_resetTool;

    ///
    wxToolBarToolBase* m_pauseTool;

    ///
    wxToolBarToolBase* m_playTool;

    ///
    wxToolBarToolBase* m_stepTool;

    ///
    wxChoice* m_manipulatorChoice;

    ///
    AppFrame* m_appFrame;

    DECLARE_EVENT_TABLE()

};

#endif //APP_TOOL_BAR_H
