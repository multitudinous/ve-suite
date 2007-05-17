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

#include "VE_Conductor/xpm/ToolBar/CursorButton.xpm"
#include "VE_Conductor/xpm/ToolBar/CursorButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectCoButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectCoButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectRotateButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectRotateButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectScaleButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectScaleButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectTranslateButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ObjectTranslateButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PauseButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PauseButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PhysicsButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PhysicsButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/PlayButton.xpm"
#include "VE_Conductor/xpm/ToolBar/PlayButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/ResetButton.xpm"
#include "VE_Conductor/xpm/ToolBar/ResetButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/SendJobButton.xpm"
#include "VE_Conductor/xpm/ToolBar/SendJobButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/StepButton.xpm"
#include "VE_Conductor/xpm/ToolBar/StepButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/StopButton.xpm"
#include "VE_Conductor/xpm/ToolBar/StopButtonSelect.xpm"
#include "VE_Conductor/xpm/ToolBar/WorldCoButton.xpm"
#include "VE_Conductor/xpm/ToolBar/WorldCoButtonSelect.xpm"

#include "VE_Conductor/GUIPlugin/CORBAServiceList.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

BEGIN_EVENT_TABLE( MainToolBar, wxToolBar )
    EVT_MENU( TOOLBAR_NAVIGATION, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_SELECTION, MainToolBar::OnChangeDeviceMode )

    EVT_MENU( TOOLBAR_OBJECT_TRANSLATE, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_ROTATE, MainToolBar::OnChangeDeviceMode )
    EVT_MENU( TOOLBAR_OBJECT_SCALE, MainToolBar::OnChangeDeviceMode )

    EVT_MENU( TOOLBAR_PHYSICS, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_RESET, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_PAUSE, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_PLAY, MainToolBar::OnPhysicsSimulation )
    EVT_MENU( TOOLBAR_STEP, MainToolBar::OnPhysicsSimulation )

    EVT_MENU( TOOLBAR_SUMMIT_JOB, MainToolBar::OnSummitJob )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
MainToolBar::MainToolBar( wxWindow* parent )
:
wxToolBar( parent, wxWindowID( -1 ), wxPoint( wxDefaultPosition ), wxSize( wxDefaultSize ), long( wxTB_FLAT | wxTB_HORIZONTAL | wxNO_BORDER ), wxString( "toolBar", wxConvUTF8 ) )
{
    CreateMainToolBar();
}
////////////////////////////////////////////////////////////////////////////////
MainToolBar::~MainToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::CreateMainToolBar()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    wxBitmap newBitmap( CursorButton_xpm );
    wxBitmap openBitmap( CursorButton_xpm );
    wxBitmap saveBitmap( CursorButton_xpm );

    wxBitmap cursorBitmap( CursorButton_xpm );
    wxBitmap cursorSelectBitmap( CursorButtonSelect_xpm );
    wxBitmap worldCoBitmap( WorldCoButton_xpm );
    wxBitmap worldCoSelectBitmap( WorldCoButtonSelect_xpm );

    wxBitmap objectTranslateBitmap( ObjectTranslateButton_xpm );
    wxBitmap objectTranslateSelectBitmap( ObjectTranslateButtonSelect_xpm );
    wxBitmap objectRotateBitmap( ObjectRotateButton_xpm );
    wxBitmap objectRotateSelectBitmap( ObjectRotateButtonSelect_xpm );
    wxBitmap objectScaleBitmap( ObjectScaleButton_xpm );
    wxBitmap objectScaleSelectBitmap( ObjectScaleButtonSelect_xpm );

    wxBitmap physicsBitmap( PhysicsButton_xpm );
    wxBitmap physicsSelectBitmap( PhysicsButtonSelect_xpm );
    wxBitmap resetBitmap( ResetButton_xpm );
    wxBitmap resetSelectBitmap( ResetButtonSelect_xpm );
    wxBitmap pauseBitmap( PauseButton_xpm );
    wxBitmap pauseSelectBitmap( PauseButtonSelect_xpm );
    wxBitmap playBitmap( PlayButton_xpm );
    wxBitmap playSelectBitmap( PlayButtonSelect_xpm );
    wxBitmap stepBitmap( StepButton_xpm );
    wxBitmap stepSelectBitmap( StepButtonSelect_xpm );

    wxBitmap sendJobBitmap( SendJobButton_xpm );
    wxBitmap sendJobSelectBitmap( SendJobButtonSelect_xpm );

    AddTool( wxID_NEW, _( "" ), newBitmap, _( "New" ), wxITEM_NORMAL );
    AddTool( wxID_OPEN, _( "" ), openBitmap, _( "Open" ), wxITEM_NORMAL );
    AddTool( wxID_SAVE, _( "" ), saveBitmap, _( "Save" ), wxITEM_NORMAL );
    AddSeparator();
    
    AddRadioTool( TOOLBAR_SELECTION, _( "" ), cursorBitmap, cursorSelectBitmap, _( "Selection" ) );
    AddRadioTool( TOOLBAR_NAVIGATION, _( "" ), worldCoBitmap, worldCoSelectBitmap, _( "Navigation" ) );
    
    AddRadioTool( TOOLBAR_OBJECT_TRANSLATE, _( "" ), objectTranslateBitmap, objectTranslateSelectBitmap, _( "Translate Object" ) );
    AddRadioTool( TOOLBAR_OBJECT_ROTATE, _( "" ), objectRotateBitmap, objectRotateSelectBitmap, _( "Rotate Object" ) );
    AddRadioTool( TOOLBAR_OBJECT_SCALE, _( "" ), objectScaleBitmap, objectScaleSelectBitmap, _( "Scale Object" ) );
    ToggleTool( TOOLBAR_NAVIGATION, true );
    AddSeparator();

    AddCheckTool( TOOLBAR_PHYSICS, _( "" ), physicsBitmap, physicsSelectBitmap, _( "Physics On/Off" ) );
    AddCheckTool( TOOLBAR_RESET, _( "" ), resetBitmap, resetSelectBitmap, _( "Reset Simulation" ) );
    AddCheckTool( TOOLBAR_PAUSE, _( "" ), pauseBitmap, pauseSelectBitmap, _( "Pause Simulation" ) );
    AddCheckTool( TOOLBAR_PLAY, _( "" ), playBitmap, playSelectBitmap, _( "Start Simulation" ) );
    AddCheckTool( TOOLBAR_STEP, _( "" ), stepBitmap, stepSelectBitmap, _( "Step Simulation" ) );
    AddSeparator();

    AddTool( TOOLBAR_SUMMIT_JOB, _( "" ), sendJobBitmap, _( "Summit Job" ), wxITEM_NORMAL );

    Realize();
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
    }

    else if( event.GetId() == TOOLBAR_NAVIGATION )
    {
        mode = "Navigation";
    }

    dvp->SetData( std::string( "Mode" ), mode );

    command->SetCommandName( std::string( "CHANGE_DEVICE_MODE" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnPhysicsSimulation( wxCommandEvent& event )
{
    if( event.GetId() == TOOLBAR_PHYSICS )
    {
        
    }
}
////////////////////////////////////////////////////////////////////////////////
void MainToolBar::OnSummitJob( wxCommandEvent& event )
{
    static_cast< AppFrame* >( GetParent() )->SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
