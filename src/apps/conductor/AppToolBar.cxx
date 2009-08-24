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
#include <ves/conductor/xpm/ToolBar/ManipulatorTranslateButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorRotateButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorRotateButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorRotateButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorScaleButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorScaleButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorScaleButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorComboButton.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorComboButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/ManipulatorComboButtonDisabled.xpm>

#include <ves/conductor/xpm/ToolBar/PhysicsButton.xpm>
#include <ves/conductor/xpm/ToolBar/PhysicsButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/CharacterButton.xpm>
#include <ves/conductor/xpm/ToolBar/CharacterButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/CharacterButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/ResetButton.xpm>
#include <ves/conductor/xpm/ToolBar/ResetButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButton.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PauseButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButton.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButtonSelect.xpm>
#include <ves/conductor/xpm/ToolBar/PlayButtonDisabled.xpm>
#include <ves/conductor/xpm/ToolBar/StepButton.xpm>
#include <ves/conductor/xpm/ToolBar/StepButtonDisabled.xpm>

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
//#include <wx/dc.h>
//#include <wx/dcbuffer.h>
#include <wx/choice.h>
//#include <wx/settings.h>

// --- C/C++ Includes --- //
#include <string>

using namespace ves::open::xml;
using namespace ves::conductor::util;

BEGIN_EVENT_TABLE( AppToolBar, wxToolBar )

//EVT_ERASE_BACKGROUND( AppToolBar::OnEraseBackGround )
//EVT_PAINT( AppToolBar::OnPaint )

EVT_MENU( APP_TOOL_BAR_NEW, AppToolBar::OnNew )
EVT_MENU( APP_TOOL_BAR_OPEN, AppToolBar::OnOpen )
EVT_MENU( APP_TOOL_BAR_SAVE, AppToolBar::OnSave )

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

EVT_MENU( APP_TOOL_BAR_PHYSICS_CHARACTER, AppToolBar::OnCharacterState )
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
    m_physicsState( false ),
    m_characterState( false ),
    m_manipulatorState( false ),
    m_prevDeviceMode( APP_TOOL_BAR_NULL ),
    m_prevCenterPoint( APP_TOOL_BAR_NULL ),
    m_prevPhysicsSimulation( APP_TOOL_BAR_NULL ),
    m_prevManipulatorMode( APP_TOOL_BAR_NULL ),
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
    m_toolbarBitmaps[ APP_TOOL_BAR_NEW ] =
        wxBitmap( NewDocumentButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_OPEN ] =
        wxBitmap( OpenButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_SAVE ] =
        wxBitmap( SaveButton_xpm );

    m_toolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION ] =
        wxBitmap( WorldNavigationButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] =
        wxBitmap( WorldNavigationButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION ] =
        wxBitmap( ObjectNavigationButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION_SELECT ] =
        wxBitmap( ObjectNavigationButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_UNSELECT ] =
        wxBitmap( UnselectButton_xpm );

    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR ] =
        wxBitmap( ManipulatorButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SELECT ] =
        wxBitmap( ManipulatorButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE ] =
        wxBitmap( ManipulatorTranslateButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_SELECT ] =
        wxBitmap( ManipulatorTranslateButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_DISABLED ] =
        wxBitmap( ManipulatorTranslateButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE ] =
        wxBitmap( ManipulatorRotateButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE_SELECT ] =
        wxBitmap( ManipulatorRotateButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE_DISABLED ] =
        wxBitmap( ManipulatorRotateButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE ] =
        wxBitmap( ManipulatorScaleButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE_SELECT ] =
        wxBitmap( ManipulatorScaleButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE_DISABLED ] =
        wxBitmap( ManipulatorScaleButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO ] =
        wxBitmap( ManipulatorComboButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO_SELECT ] =
        wxBitmap( ManipulatorComboButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO_DISABLED ] =
        wxBitmap( ManipulatorComboButtonDisabled_xpm );

    m_toolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP ] =
        wxBitmap( SmallCenterPointJumpButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( SmallCenterPointJumpButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP ] =
        wxBitmap( MediumCenterPointJumpButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( MediumCenterPointJumpButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP ] =
        wxBitmap( LargeCenterPointJumpButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( LargeCenterPointJumpButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP ] =
        wxBitmap( BBCenterPointJumpButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP_SELECT ] =
        wxBitmap( BBCenterPointJumpButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_RESET_CENTER_POINT ] =
        wxBitmap( ResetCenterPointButton_xpm );

    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS ] =
        wxBitmap( PhysicsButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_SELECT ] =
        wxBitmap( PhysicsButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER ] =
        wxBitmap( CharacterButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_SELECT ] =
        wxBitmap( CharacterButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_DISABLED ] =
        wxBitmap( CharacterButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET ] =
        wxBitmap( ResetButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET_DISABLED ] =
        wxBitmap( ResetButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ] =
        wxBitmap( PauseButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] =
        wxBitmap( PauseButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_DISABLED ] =
        wxBitmap( PauseButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] =
        wxBitmap( PlayButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_SELECT ] =
        wxBitmap( PlayButtonSelect_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_DISABLED ] =
        wxBitmap( PlayButtonDisabled_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP ] =
        wxBitmap( StepButton_xpm );
    m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP_DISABLED ] =
        wxBitmap( StepButtonDisabled_xpm );

    m_toolbarBitmaps[ APP_TOOL_BAR_SUMMIT_JOB ] =
        wxBitmap( SendJobButton_xpm );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::CreateAppToolBar()
{
    SetBackgroundColour( wxColour( 255, 255, 255 ) );
    SetToolBitmapSize( wxSize( 32, 32 ) );

    AddTool(
        APP_TOOL_BAR_NEW, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_NEW ],
        wxT( "New" ), wxITEM_NORMAL );
    AddTool(
        APP_TOOL_BAR_OPEN, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_OPEN ],
        wxT( "Open" ), wxITEM_NORMAL );
    AddTool(
        APP_TOOL_BAR_SAVE, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_SAVE ],
        wxT( "Save" ), wxITEM_NORMAL );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_WORLD_NAVIGATION, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ],
        wxT( "World Navigation" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_OBJECT_NAVIGATION, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION ],
        wxT( "Object Navigation" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_UNSELECT, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_UNSELECT ],
        wxT( "Unselect Objects" ), wxITEM_NORMAL );

    AddSeparator();

#ifdef TRANSFORM_MANIPULATOR
    AddTool(
        APP_TOOL_BAR_MANIPULATOR, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR ],
        wxT( "Transform Manipulator On/Off" ), wxITEM_CHECK );
    AddTool(
        APP_TOOL_BAR_MANIPULATOR_TRANSLATE, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_SELECT ],
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_DISABLED ],
        wxITEM_RADIO, wxT( "Translate Manipulator Mode" ) );
    AddTool(
        APP_TOOL_BAR_MANIPULATOR_ROTATE, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE ],
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE_DISABLED ],
        wxITEM_RADIO, wxT( "Rotate Manipulator Mode" ) );
    AddTool(
        APP_TOOL_BAR_MANIPULATOR_SCALE, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE ],
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE_DISABLED ],
        wxITEM_RADIO, wxT( "Scale Manipulator Mode" ) );
    AddTool(
        APP_TOOL_BAR_MANIPULATOR_COMBO, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO ],
        m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO_DISABLED ],
        wxITEM_RADIO, wxT( "Combo Manipulator Mode" ) );

    wxString manipulatorChoices[] =
        { wxT( "Global" ), wxT( "Local" ), wxT( "View" ) };
	int numManipulatorChoices = sizeof( manipulatorChoices ) / sizeof( wxString );
	wxChoice* manipulatorChoice = new wxChoice(
        this, wxID_ANY,
        wxDefaultPosition, wxDefaultSize,
        numManipulatorChoices, manipulatorChoices );
	manipulatorChoice->SetSelection( 0 );
	manipulatorChoice->SetBackgroundColour( wxColour( 255, 255, 255 ) );
	
	AddControl( manipulatorChoice );

    AddSeparator();
#endif //TRANSFORM_MANIPULATOR

    AddTool(
        APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ],
        wxT( "Small Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP ],
        wxT( "Medium Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP ],
        wxT( "Large Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_BB_CENTER_POINT_JUMP, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP ],
        wxT( "Bounding Box Centerpoint Jump" ), wxITEM_RADIO );
    AddTool(
        APP_TOOL_BAR_RESET_CENTER_POINT, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_RESET_CENTER_POINT ],
        wxT( "Reset Centerpoint" ), wxITEM_NORMAL );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_PHYSICS, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS ],
        wxT( "Physics On/Off" ), wxITEM_CHECK );
#ifdef CHARACTER_CONTROLLER
    AddTool(
        APP_TOOL_BAR_PHYSICS_CHARACTER, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER ],
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_DISABLED ],
        wxITEM_CHECK, wxT( "Character Controller" ) );
#endif //CHARACTER_CONTROLLER
    AddTool(
        APP_TOOL_BAR_PHYSICS_RESET, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET ],
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_RESET_DISABLED ],
        wxITEM_NORMAL, wxT( "Reset Simulation" ) );
    AddTool(
        APP_TOOL_BAR_PHYSICS_PAUSE, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ],
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_DISABLED ],
        wxITEM_CHECK, wxT( "Pause Simulation" ) );
    AddTool(
        APP_TOOL_BAR_PHYSICS_PLAY, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ],
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_DISABLED ],
        wxITEM_CHECK, wxT( "Start Simulation" ) );
    AddTool(
        APP_TOOL_BAR_PHYSICS_STEP, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP ],
        m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_STEP_DISABLED ],
        wxITEM_NORMAL, wxT( "Step Simulation" ) );

    AddSeparator();

    AddTool(
        APP_TOOL_BAR_SUMMIT_JOB, wxEmptyString,
        m_toolbarBitmaps[ APP_TOOL_BAR_SUMMIT_JOB ],
        wxT( "Submit Job" ), wxITEM_NORMAL );

    Realize();

    ToggleTool( APP_TOOL_BAR_WORLD_NAVIGATION, true );
    ToggleTool( APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP, true );
    ToggleTool( APP_TOOL_BAR_MANIPULATOR_TRANSLATE, false );

    m_prevDeviceMode = APP_TOOL_BAR_WORLD_NAVIGATION;
    m_prevCenterPoint = APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP;
    m_prevManipulatorMode = APP_TOOL_BAR_MANIPULATOR_TRANSLATE;

    EnableTool( APP_TOOL_BAR_PHYSICS_CHARACTER, false );
    EnableTool( APP_TOOL_BAR_PHYSICS_RESET, false );
    EnableTool( APP_TOOL_BAR_PHYSICS_PAUSE, false );
    EnableTool( APP_TOOL_BAR_PHYSICS_PLAY, false );
    EnableTool( APP_TOOL_BAR_PHYSICS_STEP, false );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_TRANSLATE, false );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_ROTATE, false );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_SCALE, false );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_COMBO, false );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnCharacterState( wxCommandEvent& event )
{
    std::string value;
    int currentSelection = event.GetId();
    m_characterState = GetToolState( currentSelection );
    if( m_characterState )
    {
        value = "CharacterControllerOn";

        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_CHARACTER_SELECT ] );
    }
    else
    {
        value = "CharacterControllerOff";

        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ currentSelection ] );
    }

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "PHYSICS_SIMULATION_DVP", value );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "PHYSICS_SIMULATION" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
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
    std::string mode = "Reset";
    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "CENTER_POINT_UPDATE_DVP", mode );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CENTER_POINT_UPDATE" );
    command->AddDataValuePair( dvp );

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
    int currentSelection = event.GetId();
    if( m_prevManipulatorMode == currentSelection )
    {
        return;
    }

    std::string data;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_MANIPULATOR_TRANSLATE:
        {
            data = "TRANSLATE";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_TRANSLATE_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_MANIPULATOR_ROTATE:
        {
            data = "ROTATE";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_ROTATE_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_MANIPULATOR_SCALE:
        {
            data = "SCALE";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SCALE_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_MANIPULATOR_COMBO:
        {
            data = "COMBO";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_COMBO_SELECT ] );

            break;
        }
    }

    SetToolNormalBitmap(
        m_prevManipulatorMode,
        m_toolbarBitmaps[ m_prevManipulatorMode ] );

    m_prevManipulatorMode = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "MANIPULATOR_DVP", data );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "MANIPULATOR_COMMAND" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnManipulatorState( wxCommandEvent& event )
{
    std::string data;
    int currentSelection = event.GetId();
    m_manipulatorState = GetToolState( currentSelection );
    if( m_manipulatorState )
    {
        data = "ENABLE";

        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ APP_TOOL_BAR_MANIPULATOR_SELECT ] );
    }
    else
    {
        data = "DISABLE";

        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ currentSelection ] );
    }

    ToggleTool( m_prevManipulatorMode, m_manipulatorState );

    EnableTool( APP_TOOL_BAR_MANIPULATOR_TRANSLATE, m_manipulatorState );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_ROTATE, m_manipulatorState );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_SCALE, m_manipulatorState );
    EnableTool( APP_TOOL_BAR_MANIPULATOR_COMBO, m_manipulatorState );

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "MANIPULATOR_DVP", data );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "MANIPULATOR_COMMAND" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnChangeDeviceMode( wxCommandEvent& event )
{
    int currentSelection = event.GetId();

    if( m_prevDeviceMode == currentSelection )
    {
        return;
    }

    std::string mode;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_WORLD_NAVIGATION:
        {
            mode = "World Navigation";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_OBJECT_NAVIGATION:
        {
            mode = "Object Navigation";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_OBJECT_NAVIGATION_SELECT ] );

            break;
        }
    }

    SetToolNormalBitmap(
        m_prevDeviceMode,
        m_toolbarBitmaps[ m_prevDeviceMode ] );

    m_prevDeviceMode = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "CHANGE_DEVICE_MODE_DVP", mode );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CHANGE_DEVICE_MODE" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnCenterPointUpdate( wxCommandEvent& event )
{
    int currentSelection = event.GetId();
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
                m_toolbarBitmaps[ APP_TOOL_BAR_SMALL_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP:
        {
            mode = "Medium";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_MEDIUM_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP:
        {
            mode = "Large";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_LARGE_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
        case APP_TOOL_BAR_BB_CENTER_POINT_JUMP:
        {
            mode = "Bounding Box";

            SetToolNormalBitmap(
                currentSelection,
                m_toolbarBitmaps[ APP_TOOL_BAR_BB_CENTER_POINT_JUMP_SELECT ] );

            break;
        }
    }

    SetToolNormalBitmap(
        m_prevCenterPoint,
        m_toolbarBitmaps[ m_prevCenterPoint ] );

    m_prevCenterPoint = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "CENTER_POINT_UPDATE_DVP", mode );

    CommandSharedPtr command( new ves::open::xml::Command() );
    command->SetCommandName( "CENTER_POINT_UPDATE" );
    command->AddDataValuePair( dvp );

    CORBAServiceList::instance()->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnUnselectObjects( wxCommandEvent& event )
{
    SetToolNormalBitmap(
        m_prevDeviceMode,
        m_toolbarBitmaps[ m_prevDeviceMode ] );

    m_prevDeviceMode = APP_TOOL_BAR_WORLD_NAVIGATION;
    SetToolNormalBitmap(
        m_prevDeviceMode,
        m_toolbarBitmaps[ APP_TOOL_BAR_WORLD_NAVIGATION_SELECT ] );

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
    int currentSelection = event.GetId();
    m_physicsState = GetToolState( currentSelection );
    if( m_physicsState )
    {
        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_SELECT ] );
    }
    else
    {
        SetToolNormalBitmap(
            currentSelection,
            m_toolbarBitmaps[ currentSelection ] );

        event.SetId( APP_TOOL_BAR_PHYSICS_PAUSE );
        OnPhysicsSimulation( event );
    }

    if( m_prevPhysicsSimulation == APP_TOOL_BAR_PHYSICS_PAUSE )
    {
        ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, m_physicsState );
    }

    if( m_characterState )
    {
        ToggleTool( APP_TOOL_BAR_PHYSICS_CHARACTER, m_physicsState );
    }

    EnableTool( APP_TOOL_BAR_PHYSICS_CHARACTER, m_physicsState );
    EnableTool( APP_TOOL_BAR_PHYSICS_RESET, m_physicsState );
    EnableTool( APP_TOOL_BAR_PHYSICS_PAUSE, m_physicsState );
    EnableTool( APP_TOOL_BAR_PHYSICS_PLAY, m_physicsState );
    EnableTool( APP_TOOL_BAR_PHYSICS_STEP, m_physicsState );
}
////////////////////////////////////////////////////////////////////////////////
void AppToolBar::OnPhysicsSimulation( wxCommandEvent& event )
{
    int currentSelection = event.GetId();
    if( m_prevPhysicsSimulation == currentSelection &&
      ( currentSelection == APP_TOOL_BAR_PHYSICS_PLAY ||
        currentSelection == APP_TOOL_BAR_PHYSICS_PAUSE ) )
    {
        return;
    }

    std::string value;
    switch( currentSelection )
    {
        case APP_TOOL_BAR_PHYSICS_PAUSE:
        {
            value = "PausePhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, true );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, false );

            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] );

            break;
        }
        case APP_TOOL_BAR_PHYSICS_RESET:
        {
            value = "ResetPhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, true );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, false );

            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] );

            break;
        }
        case APP_TOOL_BAR_PHYSICS_STEP:
        {
            value = "StepPhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, true );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, false );

            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY ] );

            break;
        }
        case APP_TOOL_BAR_PHYSICS_PLAY:
        {
            value = "StartPhysicsSimulation";

            ToggleTool( APP_TOOL_BAR_PHYSICS_PAUSE, false );
            ToggleTool( APP_TOOL_BAR_PHYSICS_PLAY, true );
            
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PLAY,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PLAY_SELECT ] );
            SetToolNormalBitmap(
                APP_TOOL_BAR_PHYSICS_PAUSE,
                m_toolbarBitmaps[ APP_TOOL_BAR_PHYSICS_PAUSE ] );

            break;
        }
    }

    m_prevPhysicsSimulation = currentSelection;

    DataValuePairPtr dvp( new DataValuePair() );
    dvp->SetData( "PHYSICS_SIMULATION_DVP", value );

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
