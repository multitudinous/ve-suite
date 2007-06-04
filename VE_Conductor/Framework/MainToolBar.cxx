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
 * Id:            $Id: MainToolBar.cxx 7579 2007-05-09 19:51:35Z jbkoch $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
//Don't move Frame.h below MainToolBar.h
#include "VE_Conductor/Framework/Frame.h"
#include "VE_Conductor/Framework/MainToolBar.h"

#include "VE_Conductor/xpm/ToolBar/NewDocumentButton.xpm"
#include "VE_Conductor/xpm/ToolBar/OpenButton.xpm"
#include "VE_Conductor/xpm/ToolBar/SaveButton.xpm"
#include "VE_Conductor/xpm/ToolBar/CursorButton.xpm"
#include "VE_Conductor/xpm/ToolBar/CursorButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/WorldNavigationButton.xpm"
#include "VE_Conductor/xpm/ToolBar/WorldNavigationButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectNavigationButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectNavigationButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/UnselectButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PauseButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PauseButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PauseButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/PhysicsButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PhysicsButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PlayButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PlayButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PlayButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/ResetButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ResetButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/SendJobButton.xpm"
#include "VE_Conductor/xpm/ToolBar/StepButton.xpm"
#include "VE_Conductor/xpm/ToolBar/StepButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/StopButton.xpm"
#include "VE_Conductor/xpm/ToolBar/StopButtonSelect.xpm"

#include "VE_Conductor/Utilities/CORBAServiceList.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

BEGIN_EVENT_TABLE( MainToolBar, wxToolBar )
    EVT_MENU( TOOLBAR_NEW, MainToolBar::OnNew )
    EVT_MENU( TOOLBAR_OPEN, MainToolBar::OnOpen )
    EVT_MENU( TOOLBAR_SAVE, MainToolBar::OnSave )

    EVT_MENU( TOOLBAR_SELECTION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_WORLD_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_UNSELECT, MainToolBar::OnUnselectObjects )

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
    m_toolbarBitmaps[ std::string( "newBitmap" ) ] = 
        wxBitmap( NewDocumentButton_xpm );
    m_toolbarBitmaps[ std::string( "openBitmap" ) ] = 
        wxBitmap( OpenButton_xpm );
    m_toolbarBitmaps[ std::string( "saveBitmap" ) ] = 
        wxBitmap( SaveButton_xpm );

    m_toolbarBitmaps[ std::string( "cursorBitmap" ) ] = 
        wxBitmap( CursorButton_xpm );
    m_toolbarBitmaps[ std::string( "cursorSelectBitmap" ) ] = 
        wxBitmap( CursorButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "worldNavigationBitmap" ) ] = 
        wxBitmap( WorldNavigationButton_xpm );
    m_toolbarBitmaps[ std::string( "worldNavigationSelectBitmap" ) ] = 
        wxBitmap( WorldNavigationButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "objectNavigationBitmap" ) ] = 
        wxBitmap( ObjectNavigationButton_xpm );
    m_toolbarBitmaps[ std::string( "objectNavigationSelectBitmap" ) ] = 
        wxBitmap( ObjectNavigationButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "unselectBitmap" ) ] = 
        wxBitmap( UnselectButton_xpm );

    m_toolbarBitmaps[ std::string( "physicsBitmap" ) ] = 
        wxBitmap( PhysicsButton_xpm );
    m_toolbarBitmaps[ std::string( "physicsSelectBitmap" ) ] = 
        wxBitmap( PhysicsButtonSelect_xpm );

    m_toolbarBitmaps[ std::string( "resetBitmap" ) ] = 
        wxBitmap( ResetButton_xpm );
    m_toolbarBitmaps[ std::string( "resetDisabledBitmap" ) ] = 
        wxBitmap( ResetButtonDisabled_xpm );
    m_toolbarBitmaps[ std::string( "pauseBitmap" ) ] = 
        wxBitmap( PauseButton_xpm );
    m_toolbarBitmaps[ std::string( "pauseSelectBitmap" ) ] = 
        wxBitmap( PauseButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "pauseDisabledBitmap" ) ] = 
        wxBitmap( PauseButtonDisabled_xpm );
    m_toolbarBitmaps[ std::string( "playBitmap" ) ] = 
        wxBitmap( PlayButton_xpm );
    m_toolbarBitmaps[ std::string( "playSelectBitmap" ) ] = 
        wxBitmap( PlayButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "playDisabledBitmap" ) ] = 
        wxBitmap( PlayButtonDisabled_xpm );
    m_toolbarBitmaps[ std::string( "stepBitmap" ) ] = 
        wxBitmap( StepButton_xpm );
    m_toolbarBitmaps[ std::string( "stepDisabledBitmap" ) ] = 
        wxBitmap( StepButtonDisabled_xpm );

    m_toolbarBitmaps[ std::string( "sendJobBitmap" ) ] = 
        wxBitmap( SendJobButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::CreateMainToolBar()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool( wxID_NEW, _( "" ), m_toolbarBitmaps[ "newBitmap" ], _( "New" ), wxITEM_NORMAL );
    AddTool( wxID_OPEN, _( "" ), m_toolbarBitmaps[ "openBitmap" ], _( "Open" ), wxITEM_NORMAL );
    AddTool( wxID_SAVE, _( "" ), m_toolbarBitmaps[ "saveBitmap" ], _( "Save" ), wxITEM_NORMAL );
    AddSeparator();

    AddTool( TOOLBAR_SELECTION, _( "" ), m_toolbarBitmaps[ "cursorBitmap" ], _( "Selection" ), wxITEM_RADIO );
    AddTool( TOOLBAR_WORLD_NAVIGATION, _( "" ), m_toolbarBitmaps[ "worldNavigationSelectBitmap" ], _( "World Navigation" ), wxITEM_RADIO );
    AddTool( TOOLBAR_OBJECT_NAVIGATION, _( "" ), m_toolbarBitmaps[ "objectNavigationBitmap" ], _( "Object Navigation" ), wxITEM_RADIO );
    AddTool( TOOLBAR_UNSELECT, _( "" ), m_toolbarBitmaps[ "unselectBitmap" ], _( "Unselect Objects" ), wxITEM_NORMAL );
    AddSeparator();


    AddTool( TOOLBAR_PHYSICS, _( "" ), m_toolbarBitmaps[ "physicsBitmap" ], _( "Physics On/Off" ), wxITEM_CHECK );
#ifdef WIN32
    AddTool( TOOLBAR_RESET, _( "" ), m_toolbarBitmaps[ "resetBitmap" ], m_toolbarBitmaps[ "resetDisabledBitmap" ], wxITEM_NORMAL, _( "Reset Simulation" ) );
    AddTool( TOOLBAR_PAUSE, _( "" ), m_toolbarBitmaps[ "pauseBitmap" ], m_toolbarBitmaps[ "pauseDisabledBitmap" ], wxITEM_CHECK, _( "Pause Simulation" ) );
    AddTool( TOOLBAR_PLAY, _( "" ), m_toolbarBitmaps[ "playBitmap" ], m_toolbarBitmaps[ "playDisabledBitmap" ], wxITEM_CHECK, _( "Start Simulation" ) );
    AddTool( TOOLBAR_STEP, _( "" ), m_toolbarBitmaps[ "stepBitmap" ], m_toolbarBitmaps[ "stepDisabledBitmap" ], wxITEM_NORMAL, _( "Step Simulation" ) );
#else
    AddTool( TOOLBAR_RESET, _( "" ), m_toolbarBitmaps[ "resetBitmap" ], _( "Reset Simulation" ), wxITEM_NORMAL );
    AddTool( TOOLBAR_PAUSE, _( "" ), m_toolbarBitmaps[ "pauseBitmap" ], _( "Pause Simulation" ), wxITEM_CHECK );
    AddTool( TOOLBAR_PLAY, _( "" ), m_toolbarBitmaps[ "playBitmap" ], _( "Start Simulation" ), wxITEM_CHECK );
    AddTool( TOOLBAR_STEP, _( "" ), m_toolbarBitmaps[ "stepBitmap" ], _( "Step Simulation" ), wxITEM_NORMAL );
#endif
    AddSeparator();

    AddTool( TOOLBAR_SUMMIT_JOB, _( "" ), m_toolbarBitmaps[ "sendJobBitmap" ], _( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( TOOLBAR_WORLD_NAVIGATION, true );
#ifdef WIN32
    SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseDisabledBitmap" ] );
#endif

    EnableTool( TOOLBAR_OBJECT_NAVIGATION, false );
    EnableTool( TOOLBAR_RESET, false );
    EnableTool( TOOLBAR_PAUSE, false );
    EnableTool( TOOLBAR_PLAY, false );
    EnableTool( TOOLBAR_STEP, false );
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnNew( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->New( event );
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
    VE_XML::DataValuePair* dvp = new VE_XML::DataValuePair();
    VE_XML::Command* command = new VE_XML::Command();

    std::string mode;

    if( event.GetId() == TOOLBAR_SELECTION )
    {
        mode = "Selection";

        SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, m_toolbarBitmaps[ "worldNavigationBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, m_toolbarBitmaps[ "objectNavigationBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_WORLD_NAVIGATION )
    {
        mode = "World Navigation";

        SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, m_toolbarBitmaps[ "worldNavigationSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, m_toolbarBitmaps[ "objectNavigationBitmap" ] );  
    }

    else if( event.GetId() == TOOLBAR_OBJECT_NAVIGATION )
    {
        mode = "Object Navigation";

        SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, m_toolbarBitmaps[ "worldNavigationBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, m_toolbarBitmaps[ "objectNavigationSelectBitmap" ] );  
    }

    dvp->SetData( std::string( "Mode" ), mode );

    command->SetCommandName( std::string( "CHANGE_DEVICE_MODE" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnUnselectObjects( wxCommandEvent& event )
{
    VE_XML::Command* command = new VE_XML::Command();

    SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, m_toolbarBitmaps[ "worldNavigationSelectBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, m_toolbarBitmaps[ "objectNavigationBitmap" ] );  

    command->SetCommandName( std::string( "UNSELECT_OBJECTS" ) );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsState( wxCommandEvent& event )
{
    if( GetToolState( TOOLBAR_PHYSICS ) )
    {
        SetToolNormalBitmap( TOOLBAR_PHYSICS, m_toolbarBitmaps[ "physicsSelectBitmap" ] );

#ifdef WIN32
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );
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

        SetToolNormalBitmap( TOOLBAR_PHYSICS, m_toolbarBitmaps[ "physicsBitmap" ] );
#ifdef WIN32
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseDisabledBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playDisabledBitmap" ] );
#else
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );
#endif

        EnableTool( TOOLBAR_RESET, false );
        EnableTool( TOOLBAR_PAUSE, false );
        EnableTool( TOOLBAR_PLAY, false );
        EnableTool( TOOLBAR_STEP, false );

        VE_XML::DataValuePair* dvp = new VE_XML::DataValuePair();
        VE_XML::Command* command = new VE_XML::Command();

        std::string value;

        dvp->SetData( std::string( "PausePhysicsSimulation" ), value );

        command->SetCommandName( std::string( "PHYSICS_SIMULATION" ) );
        command->AddDataValuePair( dvp );

        VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

        delete command;
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsSimulation( wxCommandEvent& event )
{
    VE_XML::DataValuePair* dvp = new VE_XML::DataValuePair();
    VE_XML::Command* command = new VE_XML::Command();

    std::string value;

    if( event.GetId() == TOOLBAR_RESET )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "ResetPhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_PAUSE )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "PausePhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_PLAY )
    {
        ToggleTool( TOOLBAR_PLAY, true );
        ToggleTool( TOOLBAR_PAUSE, false );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseBitmap" ] );

        dvp->SetData( std::string( "StartPhysicsSimulation" ), value );
    }
    else if( event.GetId() == TOOLBAR_STEP )
    {
        ToggleTool( TOOLBAR_PAUSE, true );
        ToggleTool( TOOLBAR_PLAY, false );
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );

        dvp->SetData( std::string( "StepPhysicsSimulation" ), value );
    }

    command->SetCommandName( std::string( "PHYSICS_SIMULATION" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnSummitJob( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
