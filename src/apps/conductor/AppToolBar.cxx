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

// --- VE-Suite Includes --- //
//Don't move Frame.h below AppToolBar.h
#include "AppFrame.h"
#include "AppToolBar.h"

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
#include <ves/conductor/xpm/ToolBar/BBCenterPointJumpButton.xpm>
#include <ves/conductor/xpm/ToolBar/BBCenterPointJumpButtonSelect.xpm>

#include <ves/conductor/xpm/ToolBar/ResetCenterPointButton.xpm>

#include <ves/conductor/xpm/ToolBar/ManipulatorButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorTranslateButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorTranslateButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorRotateButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorRotateButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorScaleButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorScaleButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorComboButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorComboButtonSelect.xpm>

#include <ves/conductor/xpm/ToolBar/PhysicsButton.xpm>
#include <ves/conductor/xpm/ToolBar/PhysicsButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/CharacterButton.xpm>
#include <ves/conductor/xpm/ToolBar/CharacterButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ResetButton.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButton.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButton.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/StepButton.xpm>

#include <ves/conductor/xpm/ToolBar/SendJobButton.xpm>

#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>

// --- wxWidgets Includes --- //
#ifdef WIN32
//windows.h is included from somewhere above causing errors
//http://www.wxwidgets.org/docs/faqmsw.htm#asuffix
#include <wx/msw/winundef.h>
#endif //WIN32
#include <wx/dc.h>
#include <wx/dcbuffer.h>
#include <wx/choice.h>
#include <wx/settings.h>

using namespace ves::open::xml;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( AppToolBar, wxToolBar )

//EVT_ERASE_BACKGROUND( AppToolBar::OnEraseBackGround )
//EVT_PAINT( AppToolBar::OnPaint )

EVT_MENU( APP_TOOL_BAR_NEW, AppToolBar::OnNew )
EVT_MENU( APP_TOOL_BAR_OPEN, AppToolBar::OnOpen )
EVT_MENU( APP_TOOL_BAR_SAVE, AppToolBar::OnSave )

EVT_MENU( APP_TOOL_BAR_CURSOR, AppToolBar::OnChangeDeviceMode )
EVT_MENU( APP_TOOL_BAR_WORLD_NAVIGATION, AppToolBar::OnChangeDeviceMode )
EVT_MENU( APP_TOOL_BAR_OBJECT_NAVIGATION, AppToolBar::OnChangeDeviceMode )

EVT_MENU( APP_TOOL_BAR_UNSELECT, AppToolBar::OnUnselectObjects )

EVT_MENU( APP_TOOL_BAR_MANIPULATOR, AppToolBar::OnManipulatorState )
EVT_MENU( APP_TOOL_BAR_MANIPULATOR_TRANSLATE, AppToolBar::OnChangeManipulatorMode )
EVT_MENU( APP_TOOL_BAR_MANIPULATOR_ROTATE, AppToolBar::OnChangeManipulatorMode )
EVT_MENU( APP_TOOL_BAR_MANIPULATOR_SCALE, AppToolBar::OnChangeManipulatorMode )
EVT_MENU( APP_TOOL_BAR_MANIPULATOR_COMBO, AppToolBar::OnChangeManipulatorMode )

EVT_MENU( APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP, AppToolBar::OnCenterPointUpdate )
EVT_MENU( APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP, AppToolBar::OnCenterPointUpdate )
EVT_MENU( APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP, AppToolBar::OnCenterPointUpdate )
EVT_MENU( APP_TOOL_BAR_BB_CENTER_POINT_JUMP, AppToolBar::OnCenterPointUpdate )

EVT_MENU( APP_TOOL_BAR_RESET_CENTER_POINT, AppToolBar::OnResetCenterPoint )

EVT_MENU( APP_TOOL_BAR_PHYSICS, AppToolBar::OnPhysicsState )

EVT_MENU( APP_TOOL_BAR_PHYSICS_CHARACTER, AppToolBar::OnPhysicsSimulation )
EVT_MENU( APP_TOOL_BAR_PHYSICS_RESET, AppToolBar::OnPhysicsSimulation )
EVT_MENU( APP_TOOL_BAR_PHYSICS_PAUSE, AppToolBar::OnPhysicsSimulation )
EVT_MENU( APP_TOOL_BAR_PHYSICS_PLAY, AppToolBar::OnPhysicsSimulation )
EVT_MENU( APP_TOOL_BAR_PHYSICS_STEP, AppToolBar::OnPhysicsSimulation )

EVT_MENU( APP_TOOL_BAR_SUMMIT_JOB, AppToolBar::OnSummitJob )

END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
AppToolBar::AppToolBar( wxWindow* parent )
    :
    wxToolBar(
        parent,
        wxID_ANY,
        wxDefaultPosition,
        wxDefaultSize,
        wxTB_FLAT | wxTB_NODIVIDER | wxTB_HORIZONTAL | wxNO_BORDER,
        wxT( "ToolBar" ) ),
    m_prevDeviceMode( APP_TOOL_BAR_NULL ),
    m_prevCenterPoint( APP_TOOL_BAR_NULL ),
    m_prevPhysicsSimulation( APP_TOOL_BAR_NULL ),
    m_prevManipulatorMode( APP_TOOL_BAR_NULL ),
    m_manipulatorTranslateTool( NULL ),
    m_manipulatorRotateTool( NULL ),
    m_manipulatorScaleTool( NULL ),
    m_manipulatorComboTool( NULL ),
    m_characterTool( NULL ),
    m_resetTool( NULL ),
    m_pauseTool( NULL ),
    m_playTool( NULL ),
    m_stepTool( NULL ),
    m_manipulatorChoice( NULL ),
    m_appFrame( static_cast< AppFrame* >( parent ) )
{
    LoadToolBarBitmaps();
    CreateAppToolBar();
}
////////////////////////////////////////////////////////////////////////////////
AppToolBar::~AppToolBar()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::LoadToolBarBitmaps()
{
    mToolbarBitmaps[ APP_TOOL_BAR_NEW ] =
        wxBitmap( NewDocumentButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_OPEN ] =
        wxBitmap( OpenButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_SAVE ] =
        wxBitmap( SaveButton_xpm );

    mToolbarBitmaps[ APP_TOOL_BAR_CURSOR ] =
        wxBitmap( CursorButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_CURSOR_SELECT ] =
        wxBitmap( CursorButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION ] =
        wxBitmap( WorldNavigationButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] =
        wxBitmap( WorldNavigationButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION ] =
        wxBitmap( ObjectNavigationButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION_SELECT ] =
        wxBitmap( ObjectNavigationButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_UNSELECT ] =
        wxBitmap( UnselectButton_xpm );

    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR ] =
        wxBitmap( ManipulatorButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SELECT ] =
        wxBitmap( ManipulatorButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE ] =
        wxBitmap( ManipulatorTranslateButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_SELECT ] =
        wxBitmap( ManipulatorTranslateButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE ] =
        wxBitmap( ManipulatorRotateButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE_SELECT ] =
        wxBitmap( ManipulatorRotateButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE ] =
        wxBitmap( ManipulatorScaleButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE_SELECT ] =
        wxBitmap( ManipulatorScaleButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO ] =
        wxBitmap( ManipulatorComboButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO_SELECT ] =
        wxBitmap( ManipulatorComboButtonSelect_xpm );

    mToolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP ] =
        wxBitmap( SmallCenterPointJumpButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( SmallCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP ] =
        wxBitmap( MediumCenterPointJumpButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( MediumCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP ] =
        wxBitmap( LargeCenterPointJumpButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( LargeCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP ] =
        wxBitmap( BBCenterPointJumpButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( BBCenterPointJumpButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_RESET_CENTER_POINT ] =
        wxBitmap( ResetCenterPointButton_xpm );

    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS ] =
        wxBitmap( PhysicsButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_SELECT ] =
        wxBitmap( PhysicsButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER ] =
        wxBitmap( CharacterButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_SELECT ] =
        wxBitmap( CharacterButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET ] =
        wxBitmap( ResetButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ] =
        wxBitmap( PauseButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] =
        wxBitmap( PauseButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] =
        wxBitmap( PlayButton_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_SELECT ] =
        wxBitmap( PlayButtonSelect_xpm );
    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP ] =
        wxBitmap( StepButton_xpm );

    mToolbarBitmaps[ APP_TOOL_BAR_SUMMIT_JOB ] =
        wxBitmap( SendJobButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::CreateAppToolBar()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );
    SetToolSeparation( 10 );

    AddTool(
        APP_TOOL_BAR_NEW, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_NEW ],
        wxT( "New" ), wxITEM_NORMAL );
    AddTool(
        APP_TOOL_BAR_OPEN, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_OPEN ],
        wxT( "Open" ), wxITEM_NORMAL );
    AddTool(
        APP_TOOL_BAR_SAVE, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_SAVE ],
        wxT( "Save" ), wxITEM_NORMAL );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_CURSOR, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_CURSOR ],
        wxT( "Selection" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_WORLD_NAVIGATION, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ],
        wxT( "World Navigation" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_OBJECT_NAVIGATION, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION ],
        wxT( "Object Navigation" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_UNSELECT, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_UNSELECT ],
        wxT( "Unselect Objects" ), wxITEM_NORMAL );

    AddSeparator();

#ifdef TRANSFORM_MANIPULATOR
    AddTool(
        APP_TOOL_BAR_MANIPULATOR, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR ],
        wxT( "Transform Manipulator On/Off" ), wxITEM_CHECK );

    m_manipulatorTranslateTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_MANIPULATOR_TRANSLATE, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE ], wxNullBitmap,
        wxITEM_RADIO, NULL, wxT( "Translate Manipulator Mode" ) );
    m_manipulatorRotateTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_MANIPULATOR_ROTATE, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE ], wxNullBitmap,
        wxITEM_RADIO, NULL, wxT( "Rotate Manipulator Mode" ) );
    m_manipulatorScaleTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_MANIPULATOR_SCALE, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE ], wxNullBitmap,
        wxITEM_RADIO, NULL, wxT( "Scale Manipulator Mode" ) );
    m_manipulatorComboTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_MANIPULATOR_COMBO, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO ], wxNullBitmap,
        wxITEM_RADIO, NULL, wxT( "Combo Manipulator Mode" ) );

    wxString manipulatorChoices[] =
        { wxT( "Global" ), wxT( "Local" ), wxT( "View" ) };
	int numManipulatorChoices = sizeof( manipulatorChoices ) / sizeof( wxString );
	m_manipulatorChoice = new wxChoice(
        this, wxID_ANY,
        wxDefaultPosition, wxDefaultSize,
        numManipulatorChoices, manipulatorChoices );
	m_manipulatorChoice->SetSelection( 0 );
	m_manipulatorChoice->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	AddControl( m_manipulatorChoice );

    AddSeparator();
#endif //TRANSFORM_MANIPULATOR

    AddTool(
        APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ],
        wxT( "Small Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP ],
        wxT( "Medium Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP ],
        wxT( "Large Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_BB_CENTER_POINT_JUMP, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP ],
        wxT( "Bounding Box Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_RESET_CENTER_POINT, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_RESET_CENTER_POINT ],
        wxT( "Reset Centerpoint" ), wxITEM_NORMAL );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_PHYSICS, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS ],
        wxT( "Physics On/Off" ), wxITEM_CHECK );
#ifdef CHARACTER_CONTROLLER
    m_characterTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_PHYSICS_CHARACTER, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER ], wxNullBitmap,
        wxITEM_CHECK, NULL, wxT( "Character Controller" ) );
#endif //CHARACTER_CONTROLLER
    m_resetTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_PHYSICS_RESET, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET ], wxNullBitmap,
        wxITEM_NORMAL, NULL, wxT( "Reset Simulation" ) );
    m_pauseTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_PHYSICS_PAUSE, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ], wxNullBitmap,
        wxITEM_CHECK, NULL, wxT( "Pause Simulation" ) );
    m_playTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_PHYSICS_PLAY, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ], wxNullBitmap,
        wxITEM_CHECK, NULL, wxT( "Start Simulation" ) );
    m_stepTool = new wxToolBarToolBase(
        NULL, APP_TOOL_BAR_PHYSICS_STEP, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP ], wxNullBitmap,
        wxITEM_NORMAL, NULL, wxT( "Step Simulation" ) );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_SUMMIT_JOB, wxEmptyString,
        mToolbarBitmaps[ APP_TOOL_BAR_SUMMIT_JOB ],
        wxT( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( APP_TOOL_BAR_WORLD_NAVIGATION, true );
    ToggleTool( APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP, true );

    m_prevDeviceMode = APP_TOOL_BAR_WORLD_NAVIGATION;
    m_prevCenterPoint = APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnEraseBackGround( wxEraseEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnNew( wxCommandEvent& event )
{
    m_appFrame->NewCanvas( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnOpen( wxCommandEvent& event )
{
    m_appFrame->Open( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnPaint( wxPaintEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnResetCenterPoint( wxCommandEvent& event )
{
    DataValuePairPtr resetDVP( new DataValuePair() );
    resetDVP->SetData( "Reset", static_cast< unsigned int >( 0 ) );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CENTER_POINT_UPDATE" );
    command->AddDataValuePair( resetDVP );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnSave( wxCommandEvent& event )
{
    m_appFrame->Save( event );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnChangeManipulatorMode( wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnManipulatorState( wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnChangeDeviceMode( wxCommandEvent& event )
{
    APP_TOOL_BAR currentSelection =
        static_cast< APP_TOOL_BAR >( event.GetId() );

    if( m_prevDeviceMode == currentSelection )
    {
        return;
    }

    std::string mode;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_CURSOR:
        {
            mode = "Selection";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_CURSOR_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_WORLD_NAVIGATION:
        {
            mode = "World Navigation";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_OBJECT_NAVIGATION:
        {
            mode = "Object Navigation";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION_SELECT ] );

            break;
        }
    }

    SetToolNormalBitmap(
        m_prevDeviceMode,
        mToolbarBitmaps[ m_prevDeviceMode ] );

    m_prevDeviceMode = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "Mode", mode );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CHANGE_DEVICE_MODE" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnCenterPointUpdate( wxCommandEvent& event )
{
    APP_TOOL_BAR currentSelection =
        static_cast< APP_TOOL_BAR >( event.GetId() );

    if( m_prevCenterPoint == currentSelection )
    {
        return;
    }

    std::string mode;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP:
        {
            mode = "Small";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP:
        {
            mode = "Medium";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP:
        {
            mode = "Large";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_BB_CENTER_POINT_JUMP:
        {
            mode = "Bounding Box";

            SetToolNormalBitmap(
                currentSelection,
                mToolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
    }

    SetToolNormalBitmap(
        m_prevCenterPoint,
        mToolbarBitmaps[ m_prevCenterPoint ] );

    m_prevCenterPoint = currentSelection;

    DataValuePairPtr jumpModeDVP( new DataValuePair() );
    jumpModeDVP->SetData( "Mode", mode );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CENTER_POINT_UPDATE" );
    command->AddDataValuePair( jumpModeDVP );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnUnselectObjects( wxCommandEvent& event )
{
    SetToolNormalBitmap(
        m_prevDeviceMode,
        mToolbarBitmaps[ m_prevDeviceMode ] );

    m_prevDeviceMode = APP_TOOL_BAR_WORLD_NAVIGATION;
    SetToolNormalBitmap(
        m_prevDeviceMode,
        mToolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] );

    ToggleTool( APP_TOOL_BAR_WORLD_NAVIGATION, true );

    DataValuePairPtr dvp( new DataValuePair() );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "UNSELECT_OBJECTS" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnPhysicsState( wxCommandEvent& event )
{
    APP_TOOL_BAR currentSelection =
        static_cast< APP_TOOL_BAR >( event.GetId() );

    if( GetToolState( currentSelection ) )
    {
        int position = GetToolPos( currentSelection );
#ifdef CHARACTER_CONTROLLER
        InsertTool( ++position, m_characterTool );
#endif //CHARACTER_CONTROLLER
        InsertTool( ++position, m_resetTool );
        InsertTool( ++position, m_pauseTool );
        InsertTool( ++position, m_playTool );
        InsertTool( ++position, m_stepTool );

        SetToolNormalBitmap(
            currentSelection,
            mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_SELECT ] );
    }
    else
    {
#ifdef CHARACTER_CONTROLLER
        ToggleTool( APP_TOOL_BAR_PHYSICS_CHARACTER, false );
#endif //CHARACTER_CONTROLLER
        ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, false );
        ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, false );

        SetToolNormalBitmap(
            currentSelection,
            mToolbarBitmaps[ currentSelection ] );
#ifdef CHARACTER_CONTROLLER
        SetToolNormalBitmap(
            APP_TOOL_BAR_PHYSICS_CHARACTER,
            mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER ] );
#endif //CHARACTER_CONTROLLER
        SetToolNormalBitmap(
            APP_TOOL_BAR_PHYSICS_PAUSE,
            mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ] );
        SetToolNormalBitmap(
            APP_TOOL_BAR_PHYSICS_PLAY,
            mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] );

#ifdef CHARACTER_CONTROLLER
        RemoveTool( APP_TOOL_BAR_PHYSICS_CHARACTER );
#endif //CHARACTER_CONTROLLER
        RemoveTool( APP_TOOL_BAR_PHYSICS_RESET );
        RemoveTool( APP_TOOL_BAR_PHYSICS_PAUSE );
        RemoveTool( APP_TOOL_BAR_PHYSICS_PLAY );
        RemoveTool( APP_TOOL_BAR_PHYSICS_STEP );

        m_prevPhysicsSimulation = APP_TOOL_BAR_NULL;
        
        std::string value = "PausePhysicsSimulation";

        DataValuePairPtr dvp( new DataValuePair() );
        dvp->SetData( "value", value );

        ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
        command->SetCommandName( "PHYSICS_SIMULATION" );
        command->AddDataValuePair( dvp );

        CORBAServiceList::instance()->SendCommandStringToXplorer( command );
    }
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnPhysicsSimulation( wxCommandEvent& event )
{
    APP_TOOL_BAR currentSelection =
        static_cast< APP_TOOL_BAR >( event.GetId() );

    if( m_prevPhysicsSimulation == currentSelection &&
        currentSelection != APP_TOOL_BAR_PHYSICS_RESET &&
        currentSelection != APP_TOOL_BAR_PHYSICS_STEP )
    {
        return;
    }

    std::string value;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_PHYSICS_CHARACTER:
        {
            if( GetToolState( currentSelection ) )
            {
                value = "CharacterControllerOn";

                SetToolNormalBitmap(
                    currentSelection,
                    mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_SELECT ] );
            }
            else
            {
                value = "CharacterControllerOff";

                SetToolNormalBitmap(
                    currentSelection,
                    mToolbarBitmaps[ currentSelection ] );
            }

            break;
        }
        case APP_TOOL_BAR_PHYSICS_PAUSE:
        {
            value = "PausePhysicsSimulation";
        }
        case APP_TOOL_BAR_PHYSICS_RESET:
        {
            value = "ResetPhysicsSimulation";
        }
        case APP_TOOL_BAR_PHYSICS_STEP:
        {
            value = "StepPhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, true );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, false );

            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] );

            break;
        }
        case APP_TOOL_BAR_PHYSICS_PLAY:
        {
            value = "StartPhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, false );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, true );
            
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                mToolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ] );

            break;
        }
    }

    m_prevPhysicsSimulation = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "value", value );

    ves::open::xml::CommandPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "PHYSICS_SIMULATION" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnSummitJob( wxCommandEvent& event )
{
    m_appFrame->SubmitToServer( event );
}
////////////////////////////////////////////////////////////////////////////////
