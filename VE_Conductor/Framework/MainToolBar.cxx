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
#include "VE_Conductor/xpm/ToolBar/ObjectCoButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectCoButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectRotateButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectRotateButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectRotateButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectScaleButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectScaleButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectScaleButtonDisabled.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectTranslateButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectTranslateButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectTranslateButtonDisabled.xpm"
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
#include "VE_Conductor/xpm/ToolBar/WorldCoButton.xpm"
#include "VE_Conductor/xpm/ToolBar/WorldCoButtonSelect.xpm"

#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

// --- wxWidgets Includes --- //
#include <wx/dc.h>
#include <wx/filename.h>

BEGIN_EVENT_TABLE( MainToolBar, wxToolBar )
    EVT_MENU( TOOLBAR_NEW, MainToolBar::OnNew )
    EVT_MENU( TOOLBAR_OPEN, MainToolBar::OnOpen )
    EVT_MENU( TOOLBAR_SAVE, MainToolBar::OnSave )

    EVT_MENU( TOOLBAR_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_SELECTION, MainToolBar::OnChangeDeviceMode )

    EVT_MENU( TOOLBAR_OBJECT_TRANSLATE, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_ROTATE, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_SCALE, MainToolBar::OnChangeDeviceMode )

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
           long( wxCLIP_CHILDREN | wxSUNKEN_BORDER | 
           wxTB_HORIZONTAL | wxNO_BORDER ), 
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
    m_toolbarBitmaps[ std::string( "worldCoBitmap" ) ] = 
        wxBitmap( WorldCoButton_xpm );
    m_toolbarBitmaps[ std::string( "worldCoSelectBitmap" ) ] = 
        wxBitmap( WorldCoButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "objectTranslateBitmap" ) ] = 
        wxBitmap( ObjectTranslateButton_xpm );
    m_toolbarBitmaps[ std::string( "objectTranslateSelectBitmap" ) ] = 
        wxBitmap( ObjectTranslateButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "objectTranslateDisabledBitmap" ) ] = 
        wxBitmap( ObjectTranslateButtonDisabled_xpm );
    m_toolbarBitmaps[ std::string( "objectRotateBitmap" ) ] = 
        wxBitmap( ObjectRotateButton_xpm );
    m_toolbarBitmaps[ std::string( "objectRotateSelectBitmap" ) ] = 
        wxBitmap( ObjectRotateButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "objectRotateDisabledBitmap" ) ] = 
        wxBitmap( ObjectRotateButtonDisabled_xpm );
    m_toolbarBitmaps[ std::string( "objectScaleBitmap" ) ] = 
        wxBitmap( ObjectScaleButton_xpm );
    m_toolbarBitmaps[ std::string( "objectScaleSelectBitmap" ) ] = 
        wxBitmap( ObjectScaleButtonSelect_xpm );
    m_toolbarBitmaps[ std::string( "objectScaleDisabledBitmap" ) ] = 
        wxBitmap( ObjectScaleButtonDisabled_xpm );

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
    AddTool( TOOLBAR_NAVIGATION, _( "" ), m_toolbarBitmaps[ "worldCoSelectBitmap" ], _( "Navigation" ), wxITEM_RADIO );
    AddTool( TOOLBAR_OBJECT_TRANSLATE, _( "" ), m_toolbarBitmaps[ "objectTranslateBitmap" ], m_toolbarBitmaps[ "objectTranslateDisabledBitmap" ], wxITEM_RADIO, _( "Translate Object" ) );
    AddTool( TOOLBAR_OBJECT_ROTATE, _( "" ), m_toolbarBitmaps[ "objectRotateBitmap" ], m_toolbarBitmaps[ "objectRotateDisabledBitmap" ], wxITEM_RADIO, _( "Rotate Object" ) );
    AddTool( TOOLBAR_OBJECT_SCALE, _( "" ), m_toolbarBitmaps[ "objectScaleBitmap" ], m_toolbarBitmaps[ "objectScaleDisabledBitmap" ], wxITEM_RADIO, _( "Scale Object" ) );
    AddSeparator();

    AddTool( TOOLBAR_PHYSICS, _( "" ), m_toolbarBitmaps[ "physicsBitmap" ], _( "Physics On/Off" ), wxITEM_CHECK );
    AddTool( TOOLBAR_RESET, _( "" ), m_toolbarBitmaps[ "resetBitmap" ], m_toolbarBitmaps[ "resetDisabledBitmap" ], wxITEM_NORMAL, _( "Reset Simulation" ) );
    AddTool( TOOLBAR_PAUSE, _( "" ), m_toolbarBitmaps[ "pauseBitmap" ], m_toolbarBitmaps[ "pauseDisabledBitmap" ], wxITEM_CHECK, _( "Pause Simulation" ) );
    AddTool( TOOLBAR_PLAY, _( "" ), m_toolbarBitmaps[ "playBitmap" ], m_toolbarBitmaps[ "playDisabledBitmap" ], wxITEM_CHECK, _( "Start Simulation" ) );
    AddTool( TOOLBAR_STEP, _( "" ), m_toolbarBitmaps[ "stepBitmap" ], m_toolbarBitmaps[ "stepDisabledBitmap" ], wxITEM_NORMAL, _( "Step Simulation" ) );
    AddSeparator();

    AddTool( TOOLBAR_SUMMIT_JOB, _( "" ), m_toolbarBitmaps[ "sendJobBitmap" ], _( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( TOOLBAR_NAVIGATION, true );
    SetToolNormalBitmap( TOOLBAR_NAVIGATION, m_toolbarBitmaps[ "worldCoSelectBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseDisabledBitmap" ] );

    //These disabled just for now until implemented
    //***********************************************//
    EnableTool( TOOLBAR_OBJECT_TRANSLATE, false );
    EnableTool( TOOLBAR_OBJECT_ROTATE, false );
    EnableTool( TOOLBAR_OBJECT_SCALE, false );
    //***********************************************//

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
        SetToolNormalBitmap( TOOLBAR_NAVIGATION, m_toolbarBitmaps[ "worldCoBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_TRANSLATE, m_toolbarBitmaps[ "objectTranslateBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_ROTATE, m_toolbarBitmaps[ "objectRotateBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_SCALE, m_toolbarBitmaps[ "objectScaleBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_NAVIGATION )
    {
        mode = "Navigation";

        SetToolNormalBitmap( TOOLBAR_NAVIGATION, m_toolbarBitmaps[ "worldCoSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_TRANSLATE, m_toolbarBitmaps[ "objectTranslateBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_ROTATE, m_toolbarBitmaps[ "objectRotateBitmap" ] );
        //SetToolNormalBitmap( TOOLBAR_OBJECT_SCALE, m_toolbarBitmaps[ "objectScaleBitmap" ] );
    }

    dvp->SetData( std::string( "Mode" ), mode );

    command->SetCommandName( std::string( "CHANGE_DEVICE_MODE" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsState( wxCommandEvent& event )
{
    if( GetToolState( TOOLBAR_PHYSICS ) )
    {
        SetToolNormalBitmap( TOOLBAR_PHYSICS, m_toolbarBitmaps[ "physicsSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playBitmap" ] );

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
        SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseDisabledBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_PLAY, m_toolbarBitmaps[ "playDisabledBitmap" ] );

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
