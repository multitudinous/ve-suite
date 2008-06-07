/*************** <auto-copyright.rb BEGIN do not edit this line> **************
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
 *************** <auto-copyright.rb END do not edit this line> ***************/
// --- VE-Suite Includes --- //
//Don't move Frame.h below MainToolBar.h
#include "AppFrame.h"
#include "MainToolBar.h"

#include <ves/conductor/xpm/ToolBar/NewDocumentButton.xpm>
#include <ves/conductor/xpm/ToolBar/OpenButton.xpm>
#include <ves/conductor/xpm/ToolBar/SaveButton.xpm>
#include <ves/conductor/xpm/ToolBar/CursorButton.xpm>
#include <ves/conductor/xpm/ToolBar/CursorButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/WorldNavigationButton.xpm>
#include <ves/conductor/xpm/ToolBar/WorldNavigationButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ObjectNavigationButton.xpm>
#include <ves/conductor/xpm/ToolBar/ObjectNavigationButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/UnselectButton.xpm>
#include <ves/conductor/xpm/ToolBar/SmallCenterPointJumpButton.xpm>
#include <ves/conductor/xpm/ToolBar/SmallCenterPointJumpButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/MediumCenterPointJumpButton.xpm>
#include <ves/conductor/xpm/ToolBar/MediumCenterPointJumpButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/LargeCenterPointJumpButton.xpm>
#include <ves/conductor/xpm/ToolBar/LargeCenterPointJumpButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ResetCenterPointButton.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButton.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/PhysicsButton.xpm>
#include <ves/conductor/xpm/ToolBar/PhysicsButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButton.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/ResetButton.xpm>
#include <ves/conductor/xpm/ToolBar/ResetButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/SendJobButton.xpm>
#include <ves/conductor/xpm/ToolBar/StepButton.xpm>
#include <ves/conductor/xpm/ToolBar/StepButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/StopButton.xpm>
#include <ves/conductor/xpm/ToolBar/StopButtonSelect.xpm>

#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

using namespace ves::open::xml;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( MainToolBar, wxToolBar )
    EVT_MENU( TOOLBAR_NEW, MainToolBar::OnNew )
    EVT_MENU( TOOLBAR_OPEN, MainToolBar::OnOpen )
    EVT_MENU( TOOLBAR_SAVE, MainToolBar::OnSave )

    EVT_MENU( TOOLBAR_SELECTION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_WORLD_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_UNSELECT, MainToolBar::OnUnselectObjects )

    EVT_MENU( TOOLBAR_SMALL_CENTER_POINT_JUMP, MainToolBar::OnCenterPointUpdate )
    EVT_MENU( TOOLBAR_MEDIUM_CENTER_POINT_JUMP, MainToolBar::OnCenterPointUpdate )
    EVT_MENU( TOOLBAR_LARGE_CENTER_POINT_JUMP, MainToolBar::OnCenterPointUpdate )
    EVT_MENU( TOOLBAR_RESET_CENTER_POINT, MainToolBar::OnCenterPointUpdate )

    EVT_MENU( TOOLBAR_PHYSICS, MainToolBar::OnPhysicsState )

    EVT_MENU( TOOLBAR_RESET, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_PAUSE, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_PLAY, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_STEP, MainToolBar::OnPhysicsSimulation )

    EVT_MENU( TOOLBAR_SUMMIT_JOB, MainToolBar::OnSummitJob )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
MainToolBar::MainToolBar( wxWindow* parent )
        :
        wxToolBar( parent, wxWindowID( -1 ), wxPoint( wxDefaultPosition ),
                   wxSize( wxDefaultSize ),
                   long( wxCLIP_CHILDREN | wxTB_HORIZONTAL | wxSUNKEN_BORDER ),
                   wxString( "toolBar", wxConvUTF8 ) )
{
    LoadToolBarBitmaps();
    CreateMainToolBar();
}
////////////////////////////////////////////////////////////////////////////////
MainToolBar::~MainToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::LoadToolBarBitmaps()
{
    mToolbarBitmaps[ std::string( "newBitmap" )] =
        wxBitmap( NewDocumentButton_xpm );
    mToolbarBitmaps[ std::string( "openBitmap" )] =
        wxBitmap( OpenButton_xpm );
    mToolbarBitmaps[ std::string( "saveBitmap" )] =
        wxBitmap( SaveButton_xpm );

    mToolbarBitmaps[ std::string( "cursorBitmap" )] =
        wxBitmap( CursorButton_xpm );
    mToolbarBitmaps[ std::string( "cursorSelectBitmap" )] =
        wxBitmap( CursorButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "worldNavigationBitmap" )] =
        wxBitmap( WorldNavigationButton_xpm );
    mToolbarBitmaps[ std::string( "worldNavigationSelectBitmap" )] =
        wxBitmap( WorldNavigationButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "objectNavigationBitmap" )] =
        wxBitmap( ObjectNavigationButton_xpm );
    mToolbarBitmaps[ std::string( "objectNavigationSelectBitmap" )] =
        wxBitmap( ObjectNavigationButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "unselectBitmap" )] =
        wxBitmap( UnselectButton_xpm );

    mToolbarBitmaps[ std::string( "smallCenterPointBitmap" )] =
        wxBitmap( SmallCenterPointJumpButton_xpm );
    mToolbarBitmaps[ std::string( "smallCenterPointSelectBitmap" )] =
        wxBitmap( SmallCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "mediumCenterPointBitmap" )] =
        wxBitmap( MediumCenterPointJumpButton_xpm );
    mToolbarBitmaps[ std::string( "mediumCenterPointSelectBitmap" )] =
        wxBitmap( MediumCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "largeCenterPointBitmap" )] =
        wxBitmap( LargeCenterPointJumpButton_xpm );
    mToolbarBitmaps[ std::string( "largeCenterPointSelectBitmap" )] =
        wxBitmap( LargeCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "resetCenterPointBitmap" )] =
        wxBitmap( ResetCenterPointButton_xpm );

    mToolbarBitmaps[ std::string( "physicsBitmap" )] =
        wxBitmap( PhysicsButton_xpm );
    mToolbarBitmaps[ std::string( "physicsSelectBitmap" )] =
        wxBitmap( PhysicsButtonSelect_xpm );

    mToolbarBitmaps[ std::string( "resetBitmap" )] =
        wxBitmap( ResetButton_xpm );
    mToolbarBitmaps[ std::string( "resetDisabledBitmap" )] =
        wxBitmap( ResetButtonDisabled_xpm );
    mToolbarBitmaps[ std::string( "pauseBitmap" )] =
        wxBitmap( PauseButton_xpm );
    mToolbarBitmaps[ std::string( "pauseSelectBitmap" )] =
        wxBitmap( PauseButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "pauseDisabledBitmap" )] =
        wxBitmap( PauseButtonDisabled_xpm );
    mToolbarBitmaps[ std::string( "playBitmap" )] =
        wxBitmap( PlayButton_xpm );
    mToolbarBitmaps[ std::string( "playSelectBitmap" )] =
        wxBitmap( PlayButtonSelect_xpm );
    mToolbarBitmaps[ std::string( "playDisabledBitmap" )] =
        wxBitmap( PlayButtonDisabled_xpm );
    mToolbarBitmaps[ std::string( "stepBitmap" )] =
        wxBitmap( StepButton_xpm );
    mToolbarBitmaps[ std::string( "stepDisabledBitmap" )] =
        wxBitmap( StepButtonDisabled_xpm );

    mToolbarBitmaps[ std::string( "sendJobBitmap" )] =
        wxBitmap( SendJobButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::CreateMainToolBar()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool( wxID_NEW, _( "" ), mToolbarBitmaps[ "newBitmap" ], _( "New" ), wxITEM_NORMAL );
    AddTool( wxID_OPEN, _( "" ), mToolbarBitmaps[ "openBitmap" ], _( "Open" ), wxITEM_NORMAL );
    AddTool( wxID_SAVE, _( "" ), mToolbarBitmaps[ "saveBitmap" ], _( "Save" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool( TOOLBAR_SELECTION, _( "" ), mToolbarBitmaps[ "cursorBitmap" ], _( "Selection" ), wxITEM_RADIO );
    AddTool( TOOLBAR_WORLD_NAVIGATION, _( "" ), mToolbarBitmaps[ "worldNavigationSelectBitmap" ], _( "World Navigation" ), wxITEM_RADIO );
    AddTool( TOOLBAR_OBJECT_NAVIGATION, _( "" ), mToolbarBitmaps[ "objectNavigationBitmap" ], _( "Object Navigation" ), wxITEM_RADIO );
    AddTool( TOOLBAR_UNSELECT, _( "" ), mToolbarBitmaps[ "unselectBitmap" ], _( "Unselect Objects" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool( TOOLBAR_SMALL_CENTER_POINT_JUMP, _( "" ), mToolbarBitmaps[ "smallCenterPointSelectBitmap" ], _( "Small Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_MEDIUM_CENTER_POINT_JUMP, _( "" ), mToolbarBitmaps[ "mediumCenterPointBitmap" ], _( "Medium Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_LARGE_CENTER_POINT_JUMP, _( "" ), mToolbarBitmaps[ "largeCenterPointBitmap" ], _( "Large Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_RESET_CENTER_POINT, _( "" ), mToolbarBitmaps[ "resetCenterPointBitmap" ], _( "Reset Centerpoint" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool( TOOLBAR_PHYSICS, _( "" ), mToolbarBitmaps[ "physicsBitmap" ], _( "Physics On/Off" ), wxITEM_CHECK );
#ifdef WIN32
    AddTool( TOOLBAR_RESET, _( "" ), mToolbarBitmaps[ "resetBitmap" ], mToolbarBitmaps[ "resetDisabledBitmap" ], wxITEM_NORMAL, _( "Reset Simulation" ) );
    AddTool( TOOLBAR_PAUSE, _( "" ), mToolbarBitmaps[ "pauseBitmap" ], mToolbarBitmaps[ "pauseDisabledBitmap" ], wxITEM_CHECK, _( "Pause Simulation" ) );
    AddTool( TOOLBAR_PLAY, _( "" ), mToolbarBitmaps[ "playBitmap" ], mToolbarBitmaps[ "playDisabledBitmap" ], wxITEM_CHECK, _( "Start Simulation" ) );
    AddTool( TOOLBAR_STEP, _( "" ), mToolbarBitmaps[ "stepBitmap" ], mToolbarBitmaps[ "stepDisabledBitmap" ], wxITEM_NORMAL, _( "Step Simulation" ) );
#else
    AddTool( TOOLBAR_RESET, _( "" ), mToolbarBitmaps[ "resetBitmap" ], _( "Reset Simulation" ), wxITEM_NORMAL );
    AddTool( TOOLBAR_PAUSE, _( "" ), mToolbarBitmaps[ "pauseBitmap" ], _( "Pause Simulation" ), wxITEM_CHECK );
    AddTool( TOOLBAR_PLAY, _( "" ), mToolbarBitmaps[ "playBitmap" ], _( "Start Simulation" ), wxITEM_CHECK );
    AddTool( TOOLBAR_STEP, _( "" ), mToolbarBitmaps[ "stepBitmap" ], _( "Step Simulation" ), wxITEM_NORMAL );
#endif
    AddSeparator();

    AddTool( TOOLBAR_SUMMIT_JOB, _( "" ), mToolbarBitmaps[ "sendJobBitmap" ], _( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( TOOLBAR_WORLD_NAVIGATION, true );
    ToggleTool( TOOLBAR_SMALL_CENTER_POINT_JUMP, true );
#ifdef WIN32
    SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseDisabledBitmap" ] );
#endif

    EnableTool( TOOLBAR_RESET, false );
    EnableTool( TOOLBAR_PAUSE, false );
    EnableTool( TOOLBAR_PLAY, false );
    EnableTool( TOOLBAR_STEP, false );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnNew( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->NewCanvas( event );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnOpen( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->Open( event );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnSave( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->Save( event );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnChangeDeviceMode( wxCommandEvent& event )
{
    DataValuePairPtr dvp( new DataValuePair() );
    CommandSharedPtr command( new ves::open::xml::Command() );

    std::string mode;

    if( event.GetId() == TOOLBAR_SELECTION )
    {
        mode = "Selection";

        SetToolNormalBitmap( TOOLBAR_SELECTION, mToolbarBitmaps[ "cursorSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, mToolbarBitmaps[ "worldNavigationBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, mToolbarBitmaps[ "objectNavigationBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_WORLD_NAVIGATION )
    {
        mode = "World Navigation";

        SetToolNormalBitmap( TOOLBAR_SELECTION, mToolbarBitmaps[ "cursorBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, mToolbarBitmaps[ "worldNavigationSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, mToolbarBitmaps[ "objectNavigationBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_OBJECT_NAVIGATION )
    {
        mode = "Object Navigation";

        SetToolNormalBitmap( TOOLBAR_SELECTION, mToolbarBitmaps[ "cursorBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, mToolbarBitmaps[ "worldNavigationBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, mToolbarBitmaps[ "objectNavigationSelectBitmap" ] );
    }

    dvp->SetData( std::string( "Mode" ), mode );

    command->SetCommandName( std::string( "CHANGE_DEVICE_MODE" ) );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnCenterPointUpdate( wxCommandEvent& event )
{
    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CENTER_POINT_UPDATE" );

    if( event.GetId() == TOOLBAR_RESET_CENTER_POINT )
    {
        DataValuePairPtr resetDVP( new DataValuePair() );
        resetDVP->SetData( "Reset", static_cast< unsigned int >( 0 ) );
        command->AddDataValuePair( resetDVP );

        CORBAServiceList::instance()->SendCommandStringToXplorer( command );

        return;
    }

    std::string mode;
    if( event.GetId() == TOOLBAR_SMALL_CENTER_POINT_JUMP )
    {
        mode = "Small";

        SetToolNormalBitmap(
            TOOLBAR_SMALL_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "smallCenterPointSelectBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_MEDIUM_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "mediumCenterPointBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_LARGE_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "largeCenterPointBitmap" ] );
    }
    else if( event.GetId() == TOOLBAR_MEDIUM_CENTER_POINT_JUMP )
    {
        mode = "Medium";

        SetToolNormalBitmap(
            TOOLBAR_SMALL_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "smallCenterPointBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_MEDIUM_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "mediumCenterPointSelectBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_LARGE_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "largeCenterPointBitmap" ] );
    }
    else if( event.GetId() == TOOLBAR_LARGE_CENTER_POINT_JUMP )
    {
        mode = "Large";

        SetToolNormalBitmap(
            TOOLBAR_SMALL_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "smallCenterPointBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_MEDIUM_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "mediumCenterPointBitmap" ] );
        SetToolNormalBitmap(
            TOOLBAR_LARGE_CENTER_POINT_JUMP,
            mToolbarBitmaps[ "largeCenterPointSelectBitmap" ] );
    }

    DataValuePairPtr jumpModeDVP( new DataValuePair() );
    jumpModeDVP->SetData( "Mode", mode );
    command->AddDataValuePair( jumpModeDVP );
    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnUnselectObjects( wxCommandEvent& event )
{
    DataValuePairPtr dvp( new DataValuePair() );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );

    SetToolNormalBitmap( TOOLBAR_SELECTION, mToolbarBitmaps[ "cursorBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, mToolbarBitmaps[ "worldNavigationSelectBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, mToolbarBitmaps[ "objectNavigationBitmap" ] );

    ToggleTool( TOOLBAR_WORLD_NAVIGATION, true );

    command->SetCommandName( std::string( "UNSELECT_OBJECTS" ) );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsState( wxCommandEvent& event )
{
    if( GetToolState( TOOLBAR_PHYSICS ) )
    {
        SetToolNormalBitmap( TOOLBAR_PHYSICS, mToolbarBitmaps[ "physicsSelectBitmap" ] );

#ifdef WIN32
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playBitmap" ] );
#endif

        EnableTool( TOOLBAR_RESET, true );
        EnableTool( TOOLBAR_PAUSE, true );
        EnableTool( TOOLBAR_PLAY, true );
        EnableTool( TOOLBAR_STEP, true );
    }
    else
    {
        ToggleTool( TOOLBAR_PAUSE, false );
        ToggleTool( TOOLBAR_PLAY, false );

        SetToolNormalBitmap( TOOLBAR_PHYSICS, mToolbarBitmaps[ "physicsBitmap" ] );
#ifdef WIN32
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseDisabledBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playDisabledBitmap" ] );
#else
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playBitmap" ] );
#endif

        EnableTool( TOOLBAR_RESET, false );
        EnableTool( TOOLBAR_PAUSE, false );
        EnableTool( TOOLBAR_PLAY, false );
        EnableTool( TOOLBAR_STEP, false );

        DataValuePairPtr dvp( new DataValuePair() );
        ves::open::xml::CommandPtr command( new ves::open::xml::Command() );

        std::string value;

        dvp->SetData( std::string( "PausePhysicsSimulation" ), value );

        command->SetCommandName( std::string( "PHYSICS_SIMULATION" ) );
        command->AddDataValuePair( dvp );

        CORBAServiceList::instance()->SendCommandStringToXplorer( command );
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsSimulation( wxCommandEvent& event )
{
    DataValuePairPtr dvp( new DataValuePair() );
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );

    std::string value;

    if( event.GetId() == TOOLBAR_RESET )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "ResetPhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_PAUSE )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "PausePhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_PLAY )
    {
        ToggleTool( TOOLBAR_PLAY, true );
        ToggleTool( TOOLBAR_PAUSE, false );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseBitmap" ] );

        dvp->SetData( std::string( "StartPhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_STEP )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, mToolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, mToolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "StepPhysicsSimulation" ), value );
    }

    command->SetCommandName( std::string( "PHYSICS_SIMULATION" ) );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnSummitJob( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
