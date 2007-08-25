/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// --- VE-Suite Includes --- //
//Don't move Frame.h below ExportMenu.h
#include "VE_Conductor/Framework/ExportMenu.h"

#include "VE_Conductor/Utilities/CORBAServiceList.h"

#include "VE_Open/XML/Command.h"
#include "VE_Open/XML/DataValuePair.h"

BEGIN_EVENT_TABLE( ExportMenu, wxMenu )
    EVT_MENU( EXPORT_SCREEN_SHOT, ExportMenu::OnNew )
    EVT_MENU( EXPORT_DOT_FILE, ExportMenu::OnOpen )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
ExportMenu::ExportMenu() : wxMenu()
{
    LoadToolBarBitmaps();
    CreateExportMenu();
}
////////////////////////////////////////////////////////////////////////////////
ExportMenu::~ExportMenu()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::LoadToolBarBitmaps()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::CreateExportMenu()
{
/*    SetBackgroundColour( wxColour( 255, 255, 255 ) );
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

    AddTool( TOOLBAR_SMALL_CENTERPOINT_JUMP, _( "" ), m_toolbarBitmaps[ "smallCenterPointSelectBitmap" ], _( "Small Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_MEDIUM_CENTERPOINT_JUMP, _( "" ), m_toolbarBitmaps[ "mediumCenterPointBitmap" ], _( "Medium Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_LARGE_CENTERPOINT_JUMP, _( "" ), m_toolbarBitmaps[ "largeCenterPointBitmap" ], _( "Large Centerpoint Jump" ), wxITEM_RADIO );
    AddTool( TOOLBAR_BB_CENTERPOINT_JUMP, _( "" ), m_toolbarBitmaps[ "bbCenterPointBitmap" ], _( "Bounding Box Centerpoint Jump" ), wxITEM_RADIO );
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
    ToggleTool( TOOLBAR_SMALL_CENTERPOINT_JUMP, true );
#ifdef WIN32
    SetToolNormalBitmap( TOOLBAR_PAUSE, m_toolbarBitmaps[ "pauseDisabledBitmap" ] );
#endif

    EnableTool( TOOLBAR_RESET, false );
    EnableTool( TOOLBAR_PAUSE, false );
    EnableTool( TOOLBAR_PLAY, false );
    EnableTool( TOOLBAR_STEP, false );*/
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnNew( wxCommandEvent& event )
{
    //static_cast< AppFrame* >( GetParent() )->New( event );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnOpen( wxCommandEvent& event )
{
    //static_cast< AppFrame* >( GetParent() )->Open( event );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnSave( wxCommandEvent& event )
{
    //static_cast< AppFrame* >( GetParent() )->Save( event );
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnChangeDeviceMode( wxCommandEvent& event )
{
/*    VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
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

    delete command;*/
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnChangeCenterPointJump( wxCommandEvent& event )
{
/*    VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
    VE_XML::Command* command = new VE_XML::Command();

    std::string mode;

    if( event.GetId() == TOOLBAR_SMALL_CENTERPOINT_JUMP )
    {
        mode = "Small";

        SetToolNormalBitmap( TOOLBAR_SMALL_CENTERPOINT_JUMP, m_toolbarBitmaps[ "smallCenterPointSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_MEDIUM_CENTERPOINT_JUMP, m_toolbarBitmaps[ "mediumCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_LARGE_CENTERPOINT_JUMP, m_toolbarBitmaps[ "largeCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_BB_CENTERPOINT_JUMP, m_toolbarBitmaps[ "bbCenterPointBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_MEDIUM_CENTERPOINT_JUMP )
    {
        mode = "Medium";

        SetToolNormalBitmap( TOOLBAR_SMALL_CENTERPOINT_JUMP, m_toolbarBitmaps[ "smallCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_MEDIUM_CENTERPOINT_JUMP, m_toolbarBitmaps[ "mediumCenterPointSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_LARGE_CENTERPOINT_JUMP, m_toolbarBitmaps[ "largeCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_BB_CENTERPOINT_JUMP, m_toolbarBitmaps[ "bbCenterPointBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_LARGE_CENTERPOINT_JUMP )
    {
        mode = "Large";

        SetToolNormalBitmap( TOOLBAR_SMALL_CENTERPOINT_JUMP, m_toolbarBitmaps[ "smallCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_MEDIUM_CENTERPOINT_JUMP, m_toolbarBitmaps[ "mediumCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_LARGE_CENTERPOINT_JUMP, m_toolbarBitmaps[ "largeCenterPointSelectBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_BB_CENTERPOINT_JUMP, m_toolbarBitmaps[ "bbCenterPointBitmap" ] );
    }

    else if( event.GetId() == TOOLBAR_BB_CENTERPOINT_JUMP )
    {
        mode = "Bounding Box";

        SetToolNormalBitmap( TOOLBAR_SMALL_CENTERPOINT_JUMP, m_toolbarBitmaps[ "smallCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_MEDIUM_CENTERPOINT_JUMP, m_toolbarBitmaps[ "mediumCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_LARGE_CENTERPOINT_JUMP, m_toolbarBitmaps[ "largeCenterPointBitmap" ] );
        SetToolNormalBitmap( TOOLBAR_BB_CENTERPOINT_JUMP, m_toolbarBitmaps[ "bbCenterPointSelectBitmap" ] );
    }

    dvp->SetData( std::string( "Mode" ), mode );

    command->SetCommandName( std::string( "CHANGE_CENTERPOINT_MODE" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;*/
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnUnselectObjects( wxCommandEvent& event )
{
   /* VE_XML::DataValuePairWeakPtr dvp = new VE_XML::DataValuePair();
    VE_XML::Command* command = new VE_XML::Command();

    SetToolNormalBitmap( TOOLBAR_SELECTION, m_toolbarBitmaps[ "cursorBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_WORLD_NAVIGATION, m_toolbarBitmaps[ "worldNavigationSelectBitmap" ] );
    SetToolNormalBitmap( TOOLBAR_OBJECT_NAVIGATION, m_toolbarBitmaps[ "objectNavigationBitmap" ] );

    ToggleTool( TOOLBAR_WORLD_NAVIGATION, true );

    command->SetCommandName( std::string( "UNSELECT_OBJECTS" ) );
    command->AddDataValuePair( dvp );

    VE_Conductor::CORBAServiceList::instance()->SendCommandStringToXplorer( command );

    delete command;*/
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnPhysicsState( wxCommandEvent& event )
{
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnPhysicsSimulation( wxCommandEvent& event )
{
}
////////////////////////////////////////////////////////////////////////////////
void ExportMenu::OnSummitJob( wxCommandEvent& event )
{
//    static_cast< AppFrame* >( GetParent() )->SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
