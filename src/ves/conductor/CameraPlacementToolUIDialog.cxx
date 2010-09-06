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

// --- VES Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

#include <ves/conductor/ConductorLibEnums.h>
#include <ves/conductor/CameraPlacementToolUIDialog.h>

#include <ves/conductor/util/spinctld.h>

#include <ves/conductor/xpm/CPT/PrevCameraButton.xpm>
#include <ves/conductor/xpm/CPT/NextCameraButton.xpm>
#include <ves/conductor/xpm/CPT/PrevMarkerButton.xpm>
#include <ves/conductor/xpm/CPT/NextMarkerButton.xpm>

#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/button.h>
#include <wx/dialog.h>
#include <wx/statbox.h>
#include <wx/frame.h>
#include <wx/textctrl.h>
#include <wx/notebook.h>
#include <wx/combobox.h>
#include <wx/bmpbuttn.h>
#include <wx/tglbtn.h>
#include <wx/msgdlg.h>

using namespace ves::conductor;

BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )
    EVT_BUTTON(
        CPT_ADD_CAMERA_BUTTON,
        CameraPlacementToolUIDialog::OnAddCameraButton )
    EVT_BUTTON(
        CPT_PREV_CAMERA_BUTTON,
        CameraPlacementToolUIDialog::OnPrevCameraButton )
    EVT_COMBOBOX(
        CPT_CAMERA_COMBO_BOX,
        CameraPlacementToolUIDialog::OnCameraComboBox )
    //When return is pressed in the combobox
    EVT_TEXT_ENTER(
        CPT_CAMERA_COMBO_BOX,
        CameraPlacementToolUIDialog::OnCameraComboBoxTextEnter )
    EVT_BUTTON(
        CPT_NEXT_CAMERA_BUTTON,
        CameraPlacementToolUIDialog::OnNextCameraButton )
    EVT_BUTTON(
        CPT_DELETE_CAMERA_BUTTON,
        CameraPlacementToolUIDialog::OnDeleteCameraButton )
    EVT_BUTTON(
        CPT_REMOVE_ALL_CAMERAS_BUTTON,
        CameraPlacementToolUIDialog::OnRemoveAllCamerasButton )
    EVT_BUTTON(
        CPT_SAVE_IMAGE_BUTTON,
        CameraPlacementToolUIDialog::OnSaveImageButton )
    EVT_BUTTON(
        CPT_SAVE_ALL_IMAGES_BUTTON,
        CameraPlacementToolUIDialog::OnSaveAllImagesButton )
    EVT_DIRPICKER_CHANGED(
        CPT_IMAGE_DIR_PICKER_CTRL,
        CameraPlacementToolUIDialog::OnImageDirPickerCtrl )
    EVT_TOGGLEBUTTON(
        CPT_TOGGLE_HIGHLIGHT_TOOL_BUTTON,
        CameraPlacementToolUIDialog::OnToggleHighlightToolButton )
    EVT_BUTTON(
        CPT_PREV_MARKER_BUTTON,
        CameraPlacementToolUIDialog::OnPrevMarkerButton )
    EVT_COMBOBOX(
        CPT_MARKER_COMBO_BOX,
        CameraPlacementToolUIDialog::OnMarkerComboBox )
    //When return is pressed in the combobox
    EVT_TEXT_ENTER(
        CPT_MARKER_COMBO_BOX,
        CameraPlacementToolUIDialog::OnMarkerComboBoxTextEnter )
    EVT_BUTTON(
        CPT_NEXT_MARKER_BUTTON,
        CameraPlacementToolUIDialog::OnNextMarkerButton )
    EVT_BUTTON(
        CPT_DELETE_MARKER_BUTTON,
        CameraPlacementToolUIDialog::OnDeleteMarkerButton )
    EVT_BUTTON(
        CPT_REMOVE_ALL_MARKERS_BUTTON,
        CameraPlacementToolUIDialog::OnRemoveAllMarkersButton )
    EVT_RADIOBOX(
        CPT_DEPTH_OF_FIELD_EFFECT_ON_OFF,
        CameraPlacementToolUIDialog::OnDepthOfFieldEffectOnOffRadioBox )
    EVT_RADIOBOX(
        CPT_PROJECTION_EFFECT_ON_OFF,
        CameraPlacementToolUIDialog::OnProjectionEffectOnOffRadioBox )
    EVT_SLIDER(
        CPT_PROJECTION_EFFECT_OPACITY,
        CameraPlacementToolUIDialog::OnProjectionEffectOpacitySlider )
    EVT_RADIOBOX(
        CPT_CAMERA_WINDOW_ON_OFF,
        CameraPlacementToolUIDialog::OnCameraWindowOnOffRadioBox )
    EVT_SLIDER(
        CPT_CAMERA_WINDOW_RESOLUTION,
        CameraPlacementToolUIDialog::OnCameraWindowResolutionSlider )
    EVT_RADIOBOX(
        CPT_DEPTH_HELPER_WINDOW_ON_OFF,
        CameraPlacementToolUIDialog::OnDepthHelperWindowOnOffRadioBox )
    EVT_SLIDER(
        CPT_DEPTH_HELPER_WINDOW_RESOLUTION,
        CameraPlacementToolUIDialog::OnDepthHelperWindowResolutionSlider )
    EVT_RADIOBOX(
        CPT_CAMERA_GEOMETRY_ON_OFF,
        CameraPlacementToolUIDialog::OnCameraGeometryOnOffRadioBox )
    EVT_RADIOBOX(
        CPT_FRUSTUM_GEOMETRY_ON_OFF,
        CameraPlacementToolUIDialog::OnFrustumGeometryOnOffRadioBox )
    EVT_COMMAND_SCROLL(
        CPT_FIELD_OF_VIEW_SPINCTRL,
        CameraPlacementToolUIDialog::OnFieldOfViewSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_FIELD_OF_VIEW_SPINCTRL,
        CameraPlacementToolUIDialog::OnFieldOfViewText )
    EVT_SLIDER(
        CPT_FIELD_OF_VIEW_SLIDER,
        CameraPlacementToolUIDialog::OnFieldOfViewSlider )
    EVT_COMMAND_SCROLL(
        CPT_ASPECT_RATIO_SPINCTRL,
        CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_ASPECT_RATIO_SPINCTRL,
        CameraPlacementToolUIDialog::OnAspectRatioText )
    EVT_SLIDER(
        CPT_ASPECT_RATIO_SLIDER,
        CameraPlacementToolUIDialog::OnAspectRatioSlider )
    EVT_COMMAND_SCROLL(
        CPT_NEAR_PLANE_SPINCTRL,
        CameraPlacementToolUIDialog::OnNearPlaneSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_NEAR_PLANE_SPINCTRL,
        CameraPlacementToolUIDialog::OnNearPlaneText )
    EVT_SLIDER(
        CPT_NEAR_PLANE_SLIDER,
        CameraPlacementToolUIDialog::OnNearPlaneSlider )
    EVT_COMMAND_SCROLL(
        CPT_FAR_PLANE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFarPlaneSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_FAR_PLANE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFarPlaneText )
    EVT_SLIDER(
        CPT_FAR_PLANE_SLIDER,
        CameraPlacementToolUIDialog::OnFarPlaneSlider )
    EVT_COMMAND_SCROLL(
        CPT_FOCAL_DISTANCE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFocalDistanceSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_FOCAL_DISTANCE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFocalDistanceText )
    EVT_SLIDER(
        CPT_FOCAL_DISTANCE_SLIDER,
        CameraPlacementToolUIDialog::OnFocalDistanceSlider )
    EVT_COMMAND_SCROLL(
        CPT_FOCAL_RANGE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFocalRangeSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_FOCAL_RANGE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFocalRangeText )
    EVT_SLIDER(
        CPT_FOCAL_RANGE_SLIDER,
        CameraPlacementToolUIDialog::OnFocalRangeSlider )
    EVT_COMMAND_SCROLL(
        CPT_MAX_CIRCLE_OF_CONFUSION_SPINCTRL,
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSpinCtrl )
    EVT_TEXT_ENTER(
        CPT_MAX_CIRCLE_OF_CONFUSION_SPINCTRL,
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionText )
    EVT_SLIDER(
        CPT_MAX_CIRCLE_OF_CONFUSION_SLIDER,
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSlider )
    EVT_TIMER(
        CPT_UPDATE_TIMER_ID,
        CameraPlacementToolUIDialog::OnTimer )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::CameraPlacementToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::CameraPlacementToolUIDialog( 
    wxWindow* parent,
    int id, 
    ves::conductor::util::CORBAServiceList* service )
    :
    UIDialog( parent, id, wxT( "Camera Placement Tool" ) ),
    m_currentCameraSelection( -1 ),
    m_cameraNameNum( 0 ),
    m_currentMarkerSelection( -1 ),
    m_markerNameNum( 0 ),
    m_timer( this, CPT_UPDATE_TIMER_ID )
{
    mProjectionData[ 0 ] = 40.0;
    mProjectionData[ 1 ] = 1.0;
    mProjectionData[ 2 ] = 0.1;
    mProjectionData[ 3 ] = 5.0;

    mDepthOfFieldData[ 0 ] = 1.5;
    mDepthOfFieldData[ 1 ] = 5.0;
    mDepthOfFieldData[ 2 ] = 6.0;

    mServiceList = service;

    BuildGUI();

    //Setup the update timer
    m_timer.Start( 500 );
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::~CameraPlacementToolUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::TransferDataFromWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::Lock( bool WXUNUSED( l ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::BuildGUI()
{
    SetSizeHints( wxDefaultSize, wxDefaultSize );
    SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    wxBoxSizer* mainSizer;
    mainSizer = new wxBoxSizer( wxVERTICAL );

    wxNotebook* notebook;
    notebook = new wxNotebook(
        this, wxID_ANY, wxDefaultPosition, wxDefaultSize,
        wxNB_FIXEDWIDTH | wxNB_TOP );
    wxPanel* mainPanel;
    mainPanel = new wxPanel(
        notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL );
    mainPanel->SetBackgroundColour( wxColour( 190, 200, 210 ) );

    wxBoxSizer* mainPanelSizer;
    mainPanelSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticBoxSizer* managementSettingsSizer;
    managementSettingsSizer = new wxStaticBoxSizer( new wxStaticBox( mainPanel, wxID_ANY, wxT("Management Settings") ), wxVERTICAL );

    wxBoxSizer* cameraManagmentSizer;
    cameraManagmentSizer = new wxBoxSizer( wxHORIZONTAL );

    m_addCameraButton = new wxButton( mainPanel, CPT_ADD_CAMERA_BUTTON, wxT("Add Camera"), wxDefaultPosition, wxDefaultSize, 0 );
    cameraManagmentSizer->Add( m_addCameraButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_prevCameraButton = new wxBitmapButton(
        mainPanel, CPT_PREV_CAMERA_BUTTON, wxBitmap( PrevCameraButton_xpm ),
        wxDefaultPosition, wxDefaultSize, wxBU_AUTODRAW );
    cameraManagmentSizer->Add( m_prevCameraButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_cameraComboBox = new wxComboBox( mainPanel, CPT_CAMERA_COMBO_BOX, wxT("Select a Camera"), wxDefaultPosition, wxDefaultSize, 0, NULL, wxTE_PROCESS_ENTER ); 
    cameraManagmentSizer->Add( m_cameraComboBox, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_nextCameraButton = new wxBitmapButton(
        mainPanel, CPT_NEXT_CAMERA_BUTTON, wxBitmap( NextCameraButton_xpm ),
        wxDefaultPosition, wxDefaultSize, wxBU_AUTODRAW );
    cameraManagmentSizer->Add( m_nextCameraButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_deleteCameraButton = new wxButton( mainPanel, CPT_DELETE_CAMERA_BUTTON, wxT("Delete Camera"), wxDefaultPosition, wxDefaultSize, 0 );
    cameraManagmentSizer->Add( m_deleteCameraButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_removeAllCamerasButton = new wxButton( mainPanel, CPT_REMOVE_ALL_CAMERAS_BUTTON, wxT("Remove All"), wxDefaultPosition, wxDefaultSize, 0 );
    cameraManagmentSizer->Add( m_removeAllCamerasButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    managementSettingsSizer->Add( cameraManagmentSizer, 0, wxEXPAND, 5 );

    wxStaticLine* managementSettingsSeparator = new wxStaticLine( mainPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    managementSettingsSizer->Add( managementSettingsSeparator, 0, wxEXPAND | wxALL, 10 );

    wxBoxSizer* imageManagementSizer;
    imageManagementSizer = new wxBoxSizer( wxHORIZONTAL );

    m_saveImageButton = new wxButton( mainPanel, CPT_SAVE_IMAGE_BUTTON, wxT("Save Image"), wxDefaultPosition, wxDefaultSize, 0 );
    imageManagementSizer->Add( m_saveImageButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_saveAllImagesButton = new wxButton( mainPanel, CPT_SAVE_ALL_IMAGES_BUTTON, wxT("Save All Images"), wxDefaultPosition, wxDefaultSize, 0 );
    imageManagementSizer->Add( m_saveAllImagesButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    wxStaticText* imageDirectoryText;
    imageDirectoryText = new wxStaticText( mainPanel, wxID_ANY, wxT("Directory:"), wxDefaultPosition, wxDefaultSize, 0 );
    imageDirectoryText->Wrap( -1 );
    imageManagementSizer->Add( imageDirectoryText, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_imageDirPickerCtrl = new wxDirPickerCtrl( mainPanel, CPT_IMAGE_DIR_PICKER_CTRL, ::wxGetCwd(), wxT("Select a folder"), wxDefaultPosition, wxDefaultSize, wxDIRP_DEFAULT_STYLE );
    imageManagementSizer->Add( m_imageDirPickerCtrl, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    managementSettingsSizer->Add( imageManagementSizer, 0, wxEXPAND, 5 );

	wxStaticLine* managementSettingsSeparator1;
	managementSettingsSeparator1 = new wxStaticLine( mainPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
	managementSettingsSizer->Add( managementSettingsSeparator1, 0, wxEXPAND | wxALL, 5 );
	
	wxString m_cameraManagerButtonChoices[] = { wxT("Off"), wxT("On") };
	int m_cameraManagerButtonNChoices = sizeof( m_cameraManagerButtonChoices ) / sizeof( wxString );
	m_cameraManagerButton = new wxRadioBox( mainPanel, wxID_ANY, wxT("Camera Manager"), wxDefaultPosition, wxDefaultSize, m_cameraManagerButtonNChoices, m_cameraManagerButtonChoices, 1, wxRA_SPECIFY_ROWS );
	m_cameraManagerButton->SetSelection( 0 );
	managementSettingsSizer->Add( m_cameraManagerButton, 0, 0, 5 );
	    
    mainPanelSizer->Add( managementSettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* highlightToolSettingsSizer;
    highlightToolSettingsSizer = new wxStaticBoxSizer( new wxStaticBox( mainPanel, wxID_ANY, wxT("Highlight Tool Settings") ), wxHORIZONTAL );

    m_toggleHighlightToolButton = new wxToggleButton( mainPanel, CPT_TOGGLE_HIGHLIGHT_TOOL_BUTTON, wxT("Toggle On/Off"), wxDefaultPosition, wxDefaultSize, 0 );
    highlightToolSettingsSizer->Add( m_toggleHighlightToolButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_prevMarkerButton = new wxBitmapButton( mainPanel, CPT_PREV_MARKER_BUTTON, wxBitmap( PrevMarkerButton_xpm ), wxDefaultPosition, wxDefaultSize, wxBU_AUTODRAW );
    highlightToolSettingsSizer->Add( m_prevMarkerButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_markerComboBox = new wxComboBox( mainPanel, CPT_MARKER_COMBO_BOX, wxT("Select a Marker"), wxDefaultPosition, wxDefaultSize, 0, NULL, 0 ); 
    highlightToolSettingsSizer->Add( m_markerComboBox, 1, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_nextMarkerButton = new wxBitmapButton( mainPanel, CPT_NEXT_MARKER_BUTTON, wxBitmap( NextMarkerButton_xpm ), wxDefaultPosition, wxDefaultSize, wxBU_AUTODRAW );
    highlightToolSettingsSizer->Add( m_nextMarkerButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_deleteMarkerButton = new wxButton( mainPanel, CPT_DELETE_MARKER_BUTTON, wxT("Delete Marker"), wxDefaultPosition, wxDefaultSize, 0 );
    highlightToolSettingsSizer->Add( m_deleteMarkerButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    m_removeAllMarkersButton = new wxButton( mainPanel, CPT_REMOVE_ALL_MARKERS_BUTTON, wxT("Remove All"), wxDefaultPosition, wxDefaultSize, 0 );
    highlightToolSettingsSizer->Add( m_removeAllMarkersButton, 0, wxALIGN_CENTER_VERTICAL|wxALL, 5 );

    mainPanelSizer->Add( highlightToolSettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* displaySettingsSizer;
    displaySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        mainPanel, wxID_ANY, wxT( "Display Settings" ) ), wxVERTICAL );

    wxBoxSizer* depthOfFieldEffectOnOffSizer;
    depthOfFieldEffectOnOffSizer = new wxBoxSizer( wxVERTICAL );

    wxString mDepthOfFieldEffectOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mDepthOfFieldEffectOnOffNChoices =
        sizeof( mDepthOfFieldEffectOnOffChoices ) / sizeof( wxString );
    mDepthOfFieldEffectOnOff = new wxRadioBox(
        mainPanel, CPT_DEPTH_OF_FIELD_EFFECT_ON_OFF,
        wxT( "Depth of Field Effect" ), wxDefaultPosition, wxSize( 144, -1 ),
        mDepthOfFieldEffectOnOffNChoices, mDepthOfFieldEffectOnOffChoices, 1,
        wxRA_SPECIFY_ROWS );
    mDepthOfFieldEffectOnOff->SetSelection( 0 );
    mDepthOfFieldEffectOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    depthOfFieldEffectOnOffSizer->Add( mDepthOfFieldEffectOnOff, 0, wxALL, 5 );

    displaySettingsSizer->Add( depthOfFieldEffectOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* projectionEffectSizer;
    projectionEffectSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* projectionEffectOnOffSizer;
    projectionEffectOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mProjectionEffectOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mProjectionEffectOnOffNChoices =
        sizeof( mProjectionEffectOnOffChoices ) / sizeof( wxString );
    mProjectionEffectOnOff = new wxRadioBox(
        mainPanel, CPT_PROJECTION_EFFECT_ON_OFF, wxT( "Projection Effect" ),
        wxDefaultPosition, wxSize( 144, -1 ), mProjectionEffectOnOffNChoices,
        mProjectionEffectOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mProjectionEffectOnOff->SetSelection( 1 );
    mProjectionEffectOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    projectionEffectOnOffSizer->Add( mProjectionEffectOnOff, 0, wxALL, 5 );

    projectionEffectSizer->Add( projectionEffectOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* projectionEffectOpacitySizer;
    projectionEffectOpacitySizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* projectionEffectOpacityText;
    projectionEffectOpacityText = new wxStaticText(
        mainPanel, wxID_ANY, wxT( "Opacity" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    projectionEffectOpacityText->Wrap( -1 );
    projectionEffectOpacitySizer->Add(
        projectionEffectOpacityText, 0, wxALL, 5 );

    mProjectionEffectOpacity = new wxSlider(
        mainPanel, CPT_PROJECTION_EFFECT_OPACITY, 30, 0, 100,
        wxPoint( -1, -1 ), wxSize( -1, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL | wxSL_TOP );
    projectionEffectOpacitySizer->Add(
        mProjectionEffectOpacity, 0, wxEXPAND, 5 );

    projectionEffectSizer->Add( projectionEffectOpacitySizer, 1, wxALL, 5 );

    displaySettingsSizer->Add( projectionEffectSizer, 0, wxEXPAND, 5 );

    wxStaticLine* displaySettingsSeparator;
    displaySettingsSeparator = new wxStaticLine(
        mainPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize,
        wxLI_HORIZONTAL );
    displaySettingsSizer->Add(
        displaySettingsSeparator, 0, wxEXPAND | wxALL, 10 );

    wxBoxSizer* cameraWindowSizer;
    cameraWindowSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* cameraWindowOnOffSizer;
    cameraWindowOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraWindowOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraWindowOnOffNChoices =
        sizeof( mCameraWindowOnOffChoices ) / sizeof( wxString );
    mCameraWindowOnOff = new wxRadioBox(
        mainPanel, CPT_CAMERA_WINDOW_ON_OFF, wxT( "Camera Window" ),
        wxDefaultPosition, wxSize( 144, -1 ), mCameraWindowOnOffNChoices,
        mCameraWindowOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mCameraWindowOnOff->SetSelection( 1 );
    mCameraWindowOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    cameraWindowOnOffSizer->Add( mCameraWindowOnOff, 0, wxALL, 5 );

    cameraWindowSizer->Add( cameraWindowOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* cameraWindowResolutionSizer;
    cameraWindowResolutionSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* cameraWindowResolutionText;
    cameraWindowResolutionText = new wxStaticText(
        mainPanel, wxID_ANY, wxT( "Resolution (pixels)" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    cameraWindowResolutionText->Wrap( -1 );
    cameraWindowResolutionSizer->Add( cameraWindowResolutionText, 0, wxALL, 5 );

    mCameraWindowResolution = new wxSlider(
        mainPanel, CPT_CAMERA_WINDOW_RESOLUTION, 300, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL | wxSL_LABELS | wxSL_TOP );
    cameraWindowResolutionSizer->Add( mCameraWindowResolution, 0, wxEXPAND, 5 );

    cameraWindowSizer->Add( cameraWindowResolutionSizer, 1, wxALL, 5 );

    displaySettingsSizer->Add( cameraWindowSizer, 0, wxEXPAND, 5 );

    wxBoxSizer* depthHelperWindowSizer;
    depthHelperWindowSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* depthHelperWindowOnOffSizer;
    depthHelperWindowOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mDepthHelperWindowOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mDepthHelperWindowOnOffNChoices =
        sizeof( mDepthHelperWindowOnOffChoices ) / sizeof( wxString );
    mDepthHelperWindowOnOff = new wxRadioBox(
        mainPanel, CPT_DEPTH_HELPER_WINDOW_ON_OFF, wxT( "Depth Helper Window" ),
        wxDefaultPosition, wxSize( 144, -1 ), mDepthHelperWindowOnOffNChoices,
        mDepthHelperWindowOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mDepthHelperWindowOnOff->SetSelection( 1 );
    mDepthHelperWindowOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    depthHelperWindowOnOffSizer->Add( mDepthHelperWindowOnOff, 0, wxALL, 5 );

    depthHelperWindowSizer->Add( depthHelperWindowOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* depthHelperWindowResolutionSizer;
    depthHelperWindowResolutionSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* depthHelperWindowResolutionText;
    depthHelperWindowResolutionText = new wxStaticText(
        mainPanel, wxID_ANY, wxT( "Resolution (pixels)" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    depthHelperWindowResolutionText->Wrap( -1 );
    depthHelperWindowResolutionSizer->Add(
        depthHelperWindowResolutionText, 0, wxALL, 5 );

    mDepthHelperWindowResolution = new wxSlider(
        mainPanel, CPT_DEPTH_HELPER_WINDOW_RESOLUTION, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL | wxSL_LABELS | wxSL_TOP );
    depthHelperWindowResolutionSizer->Add(
        mDepthHelperWindowResolution, 0, wxEXPAND, 5 );

    depthHelperWindowSizer->Add(
        depthHelperWindowResolutionSizer, 1, wxALL, 5 );

    displaySettingsSizer->Add( depthHelperWindowSizer, 0, wxEXPAND, 5 );

    mainPanelSizer->Add( displaySettingsSizer, 0, wxALL|wxEXPAND, 10 );

    mainPanel->SetSizer( mainPanelSizer );
    mainPanel->Layout();
    mainPanelSizer->Fit( mainPanel );
    notebook->AddPage( mainPanel, wxT( "Main" ), true );

    wxPanel* cameraPanel;
    cameraPanel = new wxPanel(
        notebook, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxTAB_TRAVERSAL );
    cameraPanel->SetBackgroundColour( wxColour( 190, 200, 210 ) );

    wxBoxSizer* cameraPanelSizer;
    cameraPanelSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticBoxSizer* geometrySettingsSizer;
    geometrySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        cameraPanel, wxID_ANY, wxT( "Geometry Settings" ) ), wxHORIZONTAL );

    wxBoxSizer* cameraGeometryOnOffSizer;
    cameraGeometryOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraGeometryOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraGeometryOnOffNChoices =
        sizeof( mCameraGeometryOnOffChoices ) / sizeof( wxString );
    mCameraGeometryOnOff = new wxRadioBox(
        cameraPanel, CPT_CAMERA_GEOMETRY_ON_OFF, wxT( "Camera Geometry" ),
        wxDefaultPosition, wxSize( 144, -1 ), mCameraGeometryOnOffNChoices,
        mCameraGeometryOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mCameraGeometryOnOff->SetSelection( 1 );
    mCameraGeometryOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    cameraGeometryOnOffSizer->Add( mCameraGeometryOnOff, 0, wxALL, 5 );

    geometrySettingsSizer->Add( cameraGeometryOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* frustumGeometryOnOffSizer;
    frustumGeometryOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mFrustumGeometryOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mFrustumGeometryOnOffNChoices =
        sizeof( mFrustumGeometryOnOffChoices ) / sizeof( wxString );
    mFrustumGeometryOnOff = new wxRadioBox(
        cameraPanel, CPT_FRUSTUM_GEOMETRY_ON_OFF, wxT( "Frustum Geometry" ),
        wxDefaultPosition, wxSize( 144, -1 ), mFrustumGeometryOnOffNChoices,
        mFrustumGeometryOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mFrustumGeometryOnOff->SetSelection( 1 );
    mFrustumGeometryOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    frustumGeometryOnOffSizer->Add( mFrustumGeometryOnOff, 0, wxALL, 5 );

    geometrySettingsSizer->Add( frustumGeometryOnOffSizer, 0, wxALL, 5 );

    cameraPanelSizer->Add( geometrySettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* projectionSettingsSizer;
    projectionSettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        cameraPanel, wxID_ANY, wxT( "Projection Settings" ) ), wxVERTICAL );

    wxBoxSizer* fieldOfViewSizer;
    fieldOfViewSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* fieldOfViewTextSpinSizer;
    fieldOfViewTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* fieldOfViewTextSizer;
    fieldOfViewTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* fieldOfViewText;
    fieldOfViewText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Field of View (z)" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    fieldOfViewText->Wrap( -1 );
    fieldOfViewTextSizer->Add( fieldOfViewText, 0, wxALL, 5 );

    fieldOfViewTextSpinSizer->Add( fieldOfViewTextSizer, 0, 0, 5 );

    wxBoxSizer* fieldOfViewSpinSizer;
    fieldOfViewSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFieldOfViewSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_FIELD_OF_VIEW_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 180, mProjectionData[ 0 ], 1 );
    fieldOfViewSpinSizer->Add( mFieldOfViewSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    fieldOfViewTextSpinSizer->Add( fieldOfViewSpinSizer, 0, 0, 5 );

    fieldOfViewSizer->Add( fieldOfViewTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* fieldOfViewSliderSizer;
    fieldOfViewSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFieldOfViewSlider = new wxSlider(
        cameraPanel, CPT_FIELD_OF_VIEW_SLIDER,
        mProjectionData[ 0 ] * 10, 0, 1800, wxDefaultPosition,
        wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    fieldOfViewSliderSizer->Add(
        mFieldOfViewSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    fieldOfViewSizer->Add( fieldOfViewSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( fieldOfViewSizer, 0, wxALL | wxEXPAND, 5 );

    wxBoxSizer* aspectRatioSizer;
    aspectRatioSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* aspectRatioTextSpinSizer;
    aspectRatioTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* aspectRatioTextSizer;
    aspectRatioTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* aspectRatioText;
    aspectRatioText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Aspect Ratio:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    aspectRatioText->Wrap( -1 );
    aspectRatioTextSizer->Add( aspectRatioText, 0, wxALL, 5 );

    aspectRatioTextSpinSizer->Add( aspectRatioTextSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSpinSizer;
    aspectRatioSpinSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_ASPECT_RATIO_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 10, mProjectionData[ 1 ], 0.1 );
    aspectRatioSpinSizer->Add( mAspectRatioSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    aspectRatioTextSpinSizer->Add( aspectRatioSpinSizer, 0, 0, 5 );

    aspectRatioSizer->Add( aspectRatioTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSliderSizer;
    aspectRatioSliderSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSlider = new wxSlider(
        cameraPanel, CPT_ASPECT_RATIO_SLIDER, mProjectionData[ 1 ] * 10, 0, 100,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    aspectRatioSliderSizer->Add(
        mAspectRatioSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    aspectRatioSizer->Add( aspectRatioSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( aspectRatioSizer, 0, wxALL | wxEXPAND, 5 );

	wxBoxSizer* autoComputerFarPlaneSizer;
	autoComputerFarPlaneSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxString m_autoComputeFarButtonChoices[] = { wxT("Off"), wxT("On") };
	int m_autoComputeFarButtonNChoices = 
        sizeof( m_autoComputeFarButtonChoices ) / sizeof( wxString );
	m_autoComputeFarButton = 
        new wxRadioBox( cameraPanel, wxID_ANY, wxT("Auto Compute Far Plane"), 
        wxDefaultPosition, wxDefaultSize, m_autoComputeFarButtonNChoices, 
        m_autoComputeFarButtonChoices, 1, wxRA_SPECIFY_ROWS );
	m_autoComputeFarButton->SetSelection( 1 );
	autoComputerFarPlaneSizer->Add( m_autoComputeFarButton, 0, 0, 5 );
	
	projectionSettingsSizer->Add( autoComputerFarPlaneSizer, 1, wxEXPAND, 5 );
    
    wxBoxSizer* nearPlaneSizer;
    nearPlaneSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* nearPlaneTextSpinSizer;
    nearPlaneTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* nearPlaneTextSizer;
    nearPlaneTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* nearPlaneText;
    nearPlaneText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Near Plane:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    nearPlaneText->Wrap( -1 );
    nearPlaneTextSizer->Add( nearPlaneText, 0, wxALL, 5 );

    nearPlaneTextSpinSizer->Add( nearPlaneTextSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSpinSizer;
    nearPlaneSpinSizer = new wxBoxSizer( wxVERTICAL );

    mNearPlaneSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_NEAR_PLANE_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 1000, mProjectionData[ 2 ], 1 );
    nearPlaneSpinSizer->Add( mNearPlaneSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    nearPlaneTextSpinSizer->Add( nearPlaneSpinSizer, 0, 0, 5 );

    nearPlaneSizer->Add( nearPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSliderSizer;
    nearPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mNearPlaneSlider = new wxSlider(
        cameraPanel, CPT_NEAR_PLANE_SLIDER, mProjectionData[ 2 ] * 10, 0, 10000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    nearPlaneSliderSizer->Add(
        mNearPlaneSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    nearPlaneSizer->Add( nearPlaneSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( nearPlaneSizer, 0, wxALL|wxEXPAND, 5 );

    wxBoxSizer* farPlaneSizer;
    farPlaneSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* farPlaneTextSpinSizer;
    farPlaneTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* farPlaneTextSizer;
    farPlaneTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* farPlaneText;
    farPlaneText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Far Plane:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    farPlaneText->Wrap( -1 );
    farPlaneTextSizer->Add( farPlaneText, 0, wxALL, 5 );

    farPlaneTextSpinSizer->Add( farPlaneTextSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSpinSizer;
    farPlaneSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFarPlaneSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_FAR_PLANE_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 1000, mProjectionData[ 3 ], 1 );
    farPlaneSpinSizer->Add( mFarPlaneSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    farPlaneTextSpinSizer->Add( farPlaneSpinSizer, 0, 0, 5 );

    farPlaneSizer->Add( farPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSliderSizer;
    farPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFarPlaneSlider = new wxSlider(
        cameraPanel, CPT_FAR_PLANE_SLIDER, mProjectionData[ 3 ] * 10, 0, 10000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    farPlaneSliderSizer->Add( mFarPlaneSlider, 0, wxEXPAND|wxLEFT|wxRIGHT, 5 );

    farPlaneSizer->Add( farPlaneSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( farPlaneSizer, 0, wxALL|wxEXPAND, 5 );

    cameraPanelSizer->Add( projectionSettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* depthOfFieldSettingsSizer;
    depthOfFieldSettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        cameraPanel, wxID_ANY, wxT( "Depth of Field Settings" ) ), wxVERTICAL );

    wxBoxSizer* focalDistanceSizer;
    focalDistanceSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* focalDistanceTextSpinSizer;
    focalDistanceTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* focalDistanceTextSizer;
    focalDistanceTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* focalDistaceText;
    focalDistaceText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Focal Distance:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    focalDistaceText->Wrap( -1 );
    focalDistanceTextSizer->Add( focalDistaceText, 0, wxALL, 5 );

    focalDistanceTextSpinSizer->Add( focalDistanceTextSizer, 0, 0, 5 );

    wxBoxSizer* focalDistanceSpinSizer;
    focalDistanceSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFocalDistanceSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_FOCAL_DISTANCE_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 100, mDepthOfFieldData[ 0 ], 0.1 );
    focalDistanceSpinSizer->Add(
        mFocalDistanceSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    focalDistanceTextSpinSizer->Add( focalDistanceSpinSizer, 0, 0, 5 );

    focalDistanceSizer->Add( focalDistanceTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* focalDistanceSliderSizer;
    focalDistanceSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFocalDistanceSlider = new wxSlider(
        cameraPanel, CPT_FOCAL_DISTANCE_SLIDER,
        mDepthOfFieldData[ 0 ] * 10, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    focalDistanceSliderSizer->Add(
        mFocalDistanceSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    focalDistanceSizer->Add( focalDistanceSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    depthOfFieldSettingsSizer->Add(
        focalDistanceSizer, 0, wxALL | wxEXPAND, 5 );

    wxBoxSizer* focusRangeSizer;
    focusRangeSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* focusRangeTextSpinSizer;
    focusRangeTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* focusRangeTextSizer;
    focusRangeTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* focusRangeText;
    focusRangeText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Focus Range:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    focusRangeText->Wrap( -1 );
    focusRangeTextSizer->Add( focusRangeText, 0, wxALL, 5 );

    focusRangeTextSpinSizer->Add( focusRangeTextSizer, 0, 0, 5 );

    wxBoxSizer* focusRangeSpinSizer;
    focusRangeSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFocalRangeSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_FOCAL_RANGE_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 100, mDepthOfFieldData[ 1 ], 0.1 );
    focusRangeSpinSizer->Add( mFocalRangeSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    focusRangeTextSpinSizer->Add( focusRangeSpinSizer, 0, 0, 5 );

    focusRangeSizer->Add( focusRangeTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* focusRangeSliderSizer;
    focusRangeSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFocalRangeSlider = new wxSlider(
        cameraPanel, CPT_FOCAL_RANGE_SLIDER,
        mDepthOfFieldData[ 1 ] * 10, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    focusRangeSliderSizer->Add(
        mFocalRangeSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    focusRangeSizer->Add( focusRangeSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    depthOfFieldSettingsSizer->Add( focusRangeSizer, 0, wxALL|wxEXPAND, 5 );

    wxBoxSizer* maxCircleOfConfusionSizer;
    maxCircleOfConfusionSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* maxCircleOfConfusionTextSpinSizer;
    maxCircleOfConfusionTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* maxCircleOfConfusionTextSizer;
    maxCircleOfConfusionTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* maxCircleOfConfusionText;
    maxCircleOfConfusionText = new wxStaticText(
        cameraPanel, wxID_ANY, wxT( "Max Circle of Confusion:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    maxCircleOfConfusionText->Wrap( -1 );
    maxCircleOfConfusionTextSizer->Add(
        maxCircleOfConfusionText, 0, wxALL, 5 );

    maxCircleOfConfusionTextSpinSizer->Add(
        maxCircleOfConfusionTextSizer, 0, 0, 5 );

    wxBoxSizer* maxCircleOfConfusionSpinSizer;
    maxCircleOfConfusionSpinSizer = new wxBoxSizer( wxVERTICAL );

    mMaxCircleOfConfusionSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, CPT_MAX_CIRCLE_OF_CONFUSION_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS,
        0, 15, mDepthOfFieldData[ 2 ], 0.1 );
    maxCircleOfConfusionSpinSizer->Add(
        mMaxCircleOfConfusionSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    maxCircleOfConfusionTextSpinSizer->Add(
        maxCircleOfConfusionSpinSizer, 0, 0, 5 );

    maxCircleOfConfusionSizer->Add(
        maxCircleOfConfusionTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* maxCircleOfConfusionSliderSizer;
    maxCircleOfConfusionSliderSizer = new wxBoxSizer( wxVERTICAL );

    mMaxCircleOfConfusionSlider = new wxSlider(
        cameraPanel, CPT_MAX_CIRCLE_OF_CONFUSION_SLIDER,
        mDepthOfFieldData[ 2 ] * 10, 0, 150,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    maxCircleOfConfusionSliderSizer->Add(
        mMaxCircleOfConfusionSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    maxCircleOfConfusionSizer->Add(
        maxCircleOfConfusionSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    depthOfFieldSettingsSizer->Add(
        maxCircleOfConfusionSizer, 0, wxALL | wxEXPAND, 5 );

    cameraPanelSizer->Add( depthOfFieldSettingsSizer, 0, wxALL | wxEXPAND, 10 );

    cameraPanel->SetSizer( cameraPanelSizer );
    cameraPanel->Layout();
    cameraPanelSizer->Fit( cameraPanel );
    notebook->AddPage( cameraPanel, wxT( "Camera" ), false );

    mainSizer->Add( notebook, 1, wxEXPAND | wxALL, 5 );

    wxStdDialogButtonSizer* stdDialogButtonSizer;
    wxButton* stdDialogButtonSizerOK;
    wxButton* stdDialogButtonSizerCancel;
    stdDialogButtonSizer = new wxStdDialogButtonSizer();
    stdDialogButtonSizerOK = new wxButton( this, wxID_OK );
    stdDialogButtonSizer->AddButton( stdDialogButtonSizerOK );
    stdDialogButtonSizerCancel = new wxButton( this, wxID_CANCEL );
    stdDialogButtonSizer->AddButton( stdDialogButtonSizerCancel );
    stdDialogButtonSizer->Realize();
    mainSizer->Add( stdDialogButtonSizer, 0, wxALL | wxEXPAND, 5 );

    SetSizer( mainSizer );
    Layout();
    mainSizer->Fit( this );
    CenterOnParent();

    //Add HUD display to scene graph
    wxCommandEvent newEvent;
    OnCameraWindowOnOffRadioBox( newEvent );
    
    // Connect Events
	m_autoComputeFarButton->
        Connect( wxEVT_COMMAND_RADIOBOX_SELECTED, 
        wxCommandEventHandler( 
        CameraPlacementToolUIDialog::OnAutoComputerFarPlane ), NULL, this );

	m_cameraManagerButton->
        Connect( wxEVT_COMMAND_RADIOBOX_SELECTED, 
        wxCommandEventHandler( 
        CameraPlacementToolUIDialog::OnCameraManagerEvent ), NULL, this );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::ClearInstructions()
{
    mInstructions.clear();
    mCommandName.clear();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::SendCommandsToXplorer()
{
    ves::open::xml::CommandPtr command( new ves::open::xml::Command() ); 

    for( size_t i = 0; i < mInstructions.size(); ++i )
    {
        command->AddDataValuePair( mInstructions.at( i ) );
    }

    command->SetCommandName( mCommandName );

    mServiceList->SendCommandStringToXplorer( command );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraGeometryOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mCameraGeometryOnOff->GetSelection();

    mCommandName = "CAMERA_GEOMETRY_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr cameraGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    cameraGeometryOnOffDVP->SetData( "cameraGeometryOnOff", selection );
    mInstructions.push_back( cameraGeometryOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFrustumGeometryOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mFrustumGeometryOnOff->GetSelection();

    mCommandName = "FRUSTUM_GEOMETRY_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr frustumGeometryOnOffDVP(
        new ves::open::xml::DataValuePair() );
    frustumGeometryOnOffDVP->SetData( "frustumGeometryOnOff", selection );
    mInstructions.push_back( frustumGeometryOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAddCameraButton(
    wxCommandEvent& WXUNUSED( event ) )
{
    wxString cameraName(
        _("Camera") +
        wxString::Format( wxT( "%i" ), m_cameraNameNum ) );
    m_currentCameraSelection = m_cameraComboBox->Append( cameraName );
    m_cameraComboBox->SetStringSelection( cameraName );

    ++m_cameraNameNum;

    mCommandName = "ADD_CAMERA_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData( "addCameraObject", ConvertUnicode( cameraName.c_str() ) );
    mInstructions.push_back( dvp );

    ves::open::xml::DataValuePairSharedPtr dvpII(
        new ves::open::xml::DataValuePair() );
    dvpII->SetData(
        "selectCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvpII );

    unsigned int selection = m_autoComputeFarButton->GetSelection();
    ves::open::xml::DataValuePairSharedPtr autoDVP(
        new ves::open::xml::DataValuePair() );
    autoDVP->SetData( "autoComputeNearFarPlane", selection );
    mInstructions.push_back( autoDVP );
    
    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnPrevCameraButton(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int count( m_cameraComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentCameraSelection = m_cameraComboBox->GetSelection();
    if( m_currentCameraSelection == -1 || m_currentCameraSelection == 0 )
    {
        m_currentCameraSelection = count - 1;
    }
    else
    {
        --m_currentCameraSelection;
    }

    m_cameraComboBox->SetSelection( m_currentCameraSelection );

    mCommandName = "SELECT_CAMERA_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraComboBox( wxCommandEvent& event )
{
    m_currentCameraSelection = event.GetSelection();

    mCommandName = "SELECT_CAMERA_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraComboBoxTextEnter(
    wxCommandEvent& event )
{
    if( m_currentCameraSelection == -1 )
    {
        m_cameraComboBox->SetValue( wxT( "Select a Camera" ) );

        return;
    }

    wxString cameraName( event.GetString() );
    m_cameraComboBox->SetString( m_currentCameraSelection, cameraName );

    mCommandName = "CHANGE_CAMERA_OBJECT_NAME";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData( "changeCameraObjectName", ConvertUnicode( cameraName.c_str() ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNextCameraButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    unsigned int count( m_cameraComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentCameraSelection = m_cameraComboBox->GetSelection();
    if( m_currentCameraSelection == -1 ||
        m_currentCameraSelection == int( count - 1 ) )
    {
        m_currentCameraSelection = 0;
    }
    else
    {
        ++m_currentCameraSelection;
    }

    m_cameraComboBox->SetSelection( m_currentCameraSelection );

    mCommandName = "SELECT_CAMERA_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDeleteCameraButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    m_currentCameraSelection = m_cameraComboBox->GetSelection();
    if( m_currentCameraSelection == -1 )
    {
        return;
    }

    m_cameraComboBox->Delete( m_currentCameraSelection );

    mCommandName = "DELETE_CAMERA_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "deleteCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();

    m_currentCameraSelection = -1;
    m_cameraComboBox->SetSelection( m_currentCameraSelection );
    m_cameraComboBox->SetValue( wxT( "Select a Camera" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnRemoveAllCamerasButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    unsigned int count( m_cameraComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentCameraSelection = m_cameraComboBox->GetSelection();

    m_cameraComboBox->Clear();

    m_cameraNameNum = 0;

    mCommandName = "REMOVE_ALL_CAMERA_OBJECTS";
    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "deleteCameraObject",
        static_cast< unsigned int >( m_currentCameraSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();

    m_currentCameraSelection = -1;
    m_cameraComboBox->SetSelection( m_currentCameraSelection );
    m_cameraComboBox->SetValue( wxT( "Select a Camera" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnSaveImageButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    wxString saveImageDir = m_imageDirPickerCtrl->GetPath();
    if( saveImageDir == wxEmptyString )
    {
        wxMessageDialog msgDialog(
            this,
            wxT( "Please select a directory to save images to." ),
            wxT( "Error" ),
            wxOK );
        msgDialog.ShowModal();

        return;
    }

    mCommandName = "SAVE_CAMERA_IMAGE";
    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData( "saveImageDirectory", ConvertUnicode( saveImageDir.c_str() ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnSaveAllImagesButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    wxString saveImageDir = m_imageDirPickerCtrl->GetPath();
    if( saveImageDir == wxEmptyString )
    {
        wxMessageDialog msgDialog(
            this,
            wxT( "Please select a directory to save images to." ),
            wxT( "Error" ),
            wxOK );
        msgDialog.ShowModal();

        return;
    }

    mCommandName = "SAVE_ALL_CAMERA_IMAGES";
    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData( "saveImageDirectory", ConvertUnicode( saveImageDir.c_str() ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnImageDirPickerCtrl(
    wxFileDirPickerEvent& WXUNUSED( event ) )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnToggleHighlightToolButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    unsigned int toggle =
        static_cast< unsigned int >( m_toggleHighlightToolButton->GetValue() );

    mCommandName = "TOGGLE_HIGHLIGHT_TOOL";

    ves::open::xml::DataValuePairSharedPtr depthOfFieldEffectOnOffDVP(
        new ves::open::xml::DataValuePair() );
    depthOfFieldEffectOnOffDVP->SetData( "toggleHighlightTool", toggle );
    mInstructions.push_back( depthOfFieldEffectOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnPrevMarkerButton(
    wxCommandEvent& WXUNUSED( wxCommandEvent& event ) )
{
    unsigned int count( m_markerComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentMarkerSelection = m_markerComboBox->GetSelection();
    if( m_currentMarkerSelection == -1 || m_currentMarkerSelection == 0 )
    {
        m_currentMarkerSelection = count - 1;
    }
    else
    {
        --m_currentMarkerSelection;
    }

    m_markerComboBox->SetSelection( m_currentMarkerSelection );

    mCommandName = "SELECT_MARKER_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectMarkerObject",
        static_cast< unsigned int >( m_currentMarkerSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMarkerComboBox( wxCommandEvent& event )
{
    m_currentMarkerSelection = event.GetSelection();

    mCommandName = "SELECT_MARKER_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectMarkerObject",
        static_cast< unsigned int >( m_currentMarkerSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMarkerComboBoxTextEnter(
    wxCommandEvent& event )
{
    if( m_currentMarkerSelection == -1 )
    {
        m_markerComboBox->SetValue( wxT( "Select a Marker" ) );

        return;
    }

    wxString markerName( event.GetString() );
    m_markerComboBox->SetString( m_currentMarkerSelection, markerName );

    mCommandName = "CHANGE_MARKER_OBJECT_NAME";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData( "changeMarkerObjectName", ConvertUnicode( markerName.c_str() ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNextMarkerButton(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int count( m_markerComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentMarkerSelection = m_markerComboBox->GetSelection();
    if( m_currentMarkerSelection == -1 ||
        m_currentMarkerSelection == int( count - 1 ) )
    {
        m_currentMarkerSelection = 0;
    }
    else
    {
        ++m_currentMarkerSelection;
    }

    m_markerComboBox->SetSelection( m_currentMarkerSelection );

    mCommandName = "SELECT_MARKER_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "selectMarkerObject",
        static_cast< unsigned int >( m_currentMarkerSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDeleteMarkerButton(
    wxCommandEvent& WXUNUSED( event ) )
{
    m_currentMarkerSelection = m_markerComboBox->GetSelection();
    if( m_currentMarkerSelection == -1 )
    {
        return;
    }

    m_markerComboBox->Delete( m_currentMarkerSelection );

    mCommandName = "DELETE_MARKER_OBJECT";

    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "deleteMarkerObject",
        static_cast< unsigned int >( m_currentMarkerSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();

    m_currentMarkerSelection = -1;
    m_markerComboBox->SetSelection( m_currentMarkerSelection );
    m_markerComboBox->SetValue( wxT( "Select a Marker" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnRemoveAllMarkersButton(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int count( m_markerComboBox->GetCount() );
    if( count < 1 )
    {
        return;
    }

    m_currentMarkerSelection = m_markerComboBox->GetSelection();

    m_markerComboBox->Clear();

    m_markerNameNum = 0;

    mCommandName = "REMOVE_ALL_MARKER_OBJECTS";
    ves::open::xml::DataValuePairSharedPtr dvp(
        new ves::open::xml::DataValuePair() );
    dvp->SetData(
        "deleteMarkerObject",
        static_cast< unsigned int >( m_currentMarkerSelection ) );
    mInstructions.push_back( dvp );

    SendCommandsToXplorer();
    ClearInstructions();

    m_currentMarkerSelection = -1;
    m_markerComboBox->SetSelection( m_currentMarkerSelection );
    m_markerComboBox->SetValue( wxT( "Select a Marker" ) );
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthOfFieldEffectOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mDepthOfFieldEffectOnOff->GetSelection();

    mCommandName = "DEPTH_OF_FIELD_EFFECT_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr depthOfFieldEffectOnOffDVP(
        new ves::open::xml::DataValuePair() );
    depthOfFieldEffectOnOffDVP->SetData( "depthOfFieldEffectOnOff", selection );
    mInstructions.push_back( depthOfFieldEffectOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnProjectionEffectOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mProjectionEffectOnOff->GetSelection();

    mCommandName = "PROJECTION_EFFECT_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr projectionEffectOnOffDVP(
        new ves::open::xml::DataValuePair() );
    projectionEffectOnOffDVP->SetData( "projectionEffectOnOff", selection );
    mInstructions.push_back( projectionEffectOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnProjectionEffectOpacitySlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mProjectionEffectOpacity->GetValue();

    double opacity = static_cast< double >( value ) / 100.0;

    mCommandName = "PROJECTION_EFFECT_OPACITY";

    ves::open::xml::DataValuePairSharedPtr projectionEffectOpacityDVP(
        new ves::open::xml::DataValuePair() );
    projectionEffectOpacityDVP->SetData( "projectionEffectOpacity", opacity );
    mInstructions.push_back( projectionEffectOpacityDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraWindowOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mCameraWindowOnOff->GetSelection();

    mCommandName = "CAMERA_WINDOW_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr cameraWindowOnOffDVP(
        new ves::open::xml::DataValuePair() );
    cameraWindowOnOffDVP->SetData( "cameraWindowOnOff", selection );
    mInstructions.push_back( cameraWindowOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraWindowResolutionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mCameraWindowResolution->GetValue();

    mCommandName = "CAMERA_WINDOW_RESOLUTION";

    ves::open::xml::DataValuePairSharedPtr cameraWindowResolutionDVP(
        new ves::open::xml::DataValuePair() );
    cameraWindowResolutionDVP->SetData( "cameraWindowResolution", value );
    mInstructions.push_back( cameraWindowResolutionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthHelperWindowOnOffRadioBox(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = mDepthHelperWindowOnOff->GetSelection();

    mCommandName = "DEPTH_HELPER_WINDOW_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr depthHelperWindowOnOffDVP(
        new ves::open::xml::DataValuePair() );
    depthHelperWindowOnOffDVP->SetData( "depthHelperWindowOnOff", selection );
    mInstructions.push_back( depthHelperWindowOnOffDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthHelperWindowResolutionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mDepthHelperWindowResolution->GetValue();

    mCommandName = "DEPTH_HELPER_WINDOW_RESOLUTION";

    ves::open::xml::DataValuePairSharedPtr depthHelperWindowResolutionDVP(
        new ves::open::xml::DataValuePair() );
    depthHelperWindowResolutionDVP->SetData(
        "depthHelperWindowResolution", value );
    mInstructions.push_back( depthHelperWindowResolutionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFieldOfViewSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFieldOfViewControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFieldOfViewText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateFieldOfViewControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFieldOfViewSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mProjectionData[ 0 ] =
        static_cast< double >( mFieldOfViewSlider->GetValue() ) / 10.0;
    mFieldOfViewSpinCtrl->SetValue( mProjectionData[ 0 ] );
    
    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateAspectRatioControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAspectRatioText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateAspectRatioControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAspectRatioSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mProjectionData[ 1 ] =
        static_cast< double >( mAspectRatioSlider->GetValue() ) / 10.0;
    mAspectRatioSpinCtrl->SetValue( mProjectionData[ 1 ] );
    
    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNearPlaneSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateNearPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNearPlaneText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateNearPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNearPlaneSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    int nearPlaneValue = mNearPlaneSlider->GetValue();
    int farPlaneValue = mFarPlaneSlider->GetValue();
    if( nearPlaneValue < 1 )
    {
        nearPlaneValue = 1;
        mNearPlaneSlider->SetValue( nearPlaneValue );
    }
    else if( nearPlaneValue == 10000 )
    {
        nearPlaneValue = 9999;
        mNearPlaneSlider->SetValue( nearPlaneValue );
    }

    if( nearPlaneValue >= farPlaneValue )
    {
        EnsureSliders( CPT_NEAR_PLANE_SLIDER );
    }

    mNearPlaneSpinCtrl->SetValue(
        static_cast< double >( mNearPlaneSlider->GetValue() ) / 10.0 );
    mFarPlaneSpinCtrl->SetValue(
        static_cast< double >( mFarPlaneSlider->GetValue() ) / 10.0 );

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFarPlaneSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFarPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFarPlaneText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateFarPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFarPlaneSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    int nearPlaneValue = mNearPlaneSlider->GetValue();
    int farPlaneValue = mFarPlaneSlider->GetValue();
    if( farPlaneValue < 2 )
    {
        farPlaneValue = 2;
        mFarPlaneSlider->SetValue( farPlaneValue );
    }

    if( farPlaneValue <= nearPlaneValue )
    {
        EnsureSliders( CPT_FAR_PLANE_SLIDER );
    }

    mNearPlaneSpinCtrl->SetValue(
        static_cast< double >( mNearPlaneSlider->GetValue() ) / 10.0 );
    mFarPlaneSpinCtrl->SetValue(
        static_cast< double >( mFarPlaneSlider->GetValue() ) / 10.0 );

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalDistanceSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFocalDistanceControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalDistanceText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateFocalDistanceControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalDistanceSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mDepthOfFieldData[ 0 ] =
        static_cast< double >( mFocalDistanceSlider->GetValue() ) / 10.0;
    mFocalDistanceSpinCtrl->SetValue( mDepthOfFieldData[ 0 ] );
    
    FocalDistanceUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalRangeSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFocalRangeControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalRangeText( wxCommandEvent& WXUNUSED( event ) )
{
    UpdateFocalRangeControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalRangeSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mDepthOfFieldData[ 1 ] =
        static_cast< double >( mFocalRangeSlider->GetValue() ) / 10.0;
    mFocalRangeSpinCtrl->SetValue( mDepthOfFieldData[ 1 ] );
    
    FocalRangeUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateMaxCircleOfConfusionControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionText(
    wxCommandEvent& WXUNUSED( event ) )
{
    UpdateMaxCircleOfConfusionControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mDepthOfFieldData[ 2 ] =
        static_cast< double >( mMaxCircleOfConfusionSlider->GetValue() ) / 10.0;
    mMaxCircleOfConfusionSpinCtrl->SetValue( mDepthOfFieldData[ 2 ] );

    MaxCircleOfConfusionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::EnsureSliders( int activeSliderID )
{
    int mNearPlaneValue = mNearPlaneSlider->GetValue();
    int mFarPlaneValue = mFarPlaneSlider->GetValue();

    //maintain the value on the min/max sliders.
    if( mNearPlaneValue >= mFarPlaneValue )
    {
        /*
        if( mNearPlaneValue == 1000 )
        {
            mNearPlaneSlider->SetValue( 1000 - 1 );
        }
        else if( mFarPlaneValue == 0 )
        {
            mFarPlaneSlider->SetValue( 1 );
        }
        */

        if( activeSliderID == CPT_NEAR_PLANE_SLIDER )
        {
            mFarPlaneSlider->SetValue( mNearPlaneSlider->GetValue() + 1 );
            return true;
        }
        else if( activeSliderID == CPT_FAR_PLANE_SLIDER )
        {
            mNearPlaneSlider->SetValue( mFarPlaneSlider->GetValue() - 1 );
            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFieldOfViewControls()
{
    mProjectionData[ 0 ] = mFieldOfViewSpinCtrl->GetValue();
    mFieldOfViewSlider->SetValue(
        static_cast< int >( mProjectionData[ 0 ] ) * 10 );

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateAspectRatioControls()
{
    mProjectionData[ 1 ] = mAspectRatioSpinCtrl->GetValue();
    mAspectRatioSlider->SetValue(
        static_cast< int >( mProjectionData[ 1 ] ) * 10 );

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateNearPlaneControls()
{
    double nearPlaneValue = mNearPlaneSpinCtrl->GetValue();
    if( nearPlaneValue <= 0.0 )
    {
        nearPlaneValue = 0.1;
    }

    if( mFarPlaneSpinCtrl->GetValue() <= nearPlaneValue )
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue * 10 ) );
        mFarPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue * 10 + 1 ) );
        mFarPlaneSpinCtrl->SetValue( nearPlaneValue + 0.1 );
    }
    else
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue * 10 ) );
    }

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFarPlaneControls()
{
    double farPlaneValue = mFarPlaneSpinCtrl->GetValue();
    if( farPlaneValue <= 0.1 )
    {
        farPlaneValue = 0.2;
    }

    if( mNearPlaneSpinCtrl->GetValue() >= farPlaneValue )
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( farPlaneValue * 10 - 1 ) );
        mFarPlaneSlider->SetValue(
            static_cast< int >( farPlaneValue * 10 ) );
        mNearPlaneSpinCtrl->SetValue( farPlaneValue - 0.1 );

        if( farPlaneValue < 1 )
        {
            mNearPlaneSlider->SetValue( 0 );
            mFarPlaneSlider->SetValue( 1 );
        }
    }
    else
    {
        mFarPlaneSlider->SetValue(
        static_cast< int >( farPlaneValue * 10 ) );
    }

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFocalDistanceControls()
{
    mDepthOfFieldData[ 0 ] = mFocalDistanceSpinCtrl->GetValue();
    mFocalDistanceSlider->SetValue(
        static_cast< int >( mDepthOfFieldData[ 0 ] ) * 10 );

    FocalDistanceUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFocalRangeControls()
{
    mDepthOfFieldData[ 1 ] = mFocalRangeSpinCtrl->GetValue();
    mFocalRangeSlider->SetValue(
        static_cast< int >( mDepthOfFieldData[ 1 ] ) * 10 );

    FocalRangeUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateMaxCircleOfConfusionControls()
{
    mDepthOfFieldData[ 2 ] = mMaxCircleOfConfusionSpinCtrl->GetValue();
    mMaxCircleOfConfusionSlider->SetValue(
        static_cast< int >( mDepthOfFieldData[ 2 ] ) * 10 );

    MaxCircleOfConfusionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::ProjectionUpdate()
{
    mCommandName = std::string( "PROJECTION_UPDATE" );

    ves::open::xml::DataValuePairSharedPtr projectionFieldOfViewDVP(
        new ves::open::xml::DataValuePair() );
    projectionFieldOfViewDVP->SetData(
        "projectionFieldOfView", mProjectionData[ 0 ] );
    mInstructions.push_back( projectionFieldOfViewDVP );

    ves::open::xml::DataValuePairSharedPtr projectionAspectRatioDVP(
        new ves::open::xml::DataValuePair() );
    projectionAspectRatioDVP->SetData(
        "projectionAspectRatio", mProjectionData[ 1 ] );
    mInstructions.push_back( projectionAspectRatioDVP );

    ves::open::xml::DataValuePairSharedPtr projectionNearPlaneDVP(
        new ves::open::xml::DataValuePair() );
    projectionNearPlaneDVP->SetData(
        "projectionNearPlane", mProjectionData[ 2 ]  );
    mInstructions.push_back( projectionNearPlaneDVP );

    ves::open::xml::DataValuePairSharedPtr projectionFarPlaneDVP(
        new ves::open::xml::DataValuePair() );
    projectionFarPlaneDVP->SetData(
        "projectionFarPlane", mProjectionData[ 3 ] );
    mInstructions.push_back( projectionFarPlaneDVP );

    unsigned int selection = m_autoComputeFarButton->GetSelection();
    ves::open::xml::DataValuePairSharedPtr autoDVP(
        new ves::open::xml::DataValuePair() );
    autoDVP->SetData( "autoComputeNearFarPlane", selection );
    mInstructions.push_back( autoDVP );
    
    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::FocalDistanceUpdate()
{
    mCommandName = std::string( "FOCAL_DISTANCE" );

    ves::open::xml::DataValuePairSharedPtr focalDistanceDVP(
        new ves::open::xml::DataValuePair() );
    focalDistanceDVP->SetData(
        "focalDistance", mDepthOfFieldData[ 0 ] );
    mInstructions.push_back( focalDistanceDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::FocalRangeUpdate()
{
    mCommandName = std::string( "FOCAL_RANGE" );

    ves::open::xml::DataValuePairSharedPtr focalRangeDVP(
        new ves::open::xml::DataValuePair() );
    focalRangeDVP->SetData(
        "focalRange", mDepthOfFieldData[ 1 ] );
    mInstructions.push_back( focalRangeDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::MaxCircleOfConfusionUpdate()
{
    mCommandName = std::string( "MAX_CIRCLE_OF_CONFUSION" );

    ves::open::xml::DataValuePairSharedPtr maxCircleOfConfusionDVP(
        new ves::open::xml::DataValuePair() );
    maxCircleOfConfusionDVP->SetData(
        "maxCircleOfConfusion", mDepthOfFieldData[ 2 ] );
    mInstructions.push_back( maxCircleOfConfusionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnTimer( wxTimerEvent& WXUNUSED( event ) )
{
    //only update the gui when it is in focus and is being used
    //another method would be the wxTopLevelWindow::IsActive
    //or an wxIdleEvent may need to be used here
    //we will have to do testing to figure out the best methods
    //wxInternalIdle was called too often
    if( IsShown() )
    {
        UpdateFromXplorerData();
        if( wxUpdateUIEvent::CanUpdate( this ) )
        {
            UpdateWindowUI( wxUPDATE_UI_FROMIDLE );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFromXplorerData()
{
    UpdateCameraData();
    UpdateMarkerData();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateCameraData()
{
    const open::xml::CommandPtr command =
        mServiceList->GetGUIUpdateCommands( "UPDATE_ACTIVE_CAMERA_OBJECT" );

    //Hasn't updated yet
    if( command->GetCommandName() == "NULL" )
    {
        return;
    }

    unsigned int value;
    open::xml::DataValuePairPtr dvp =
        command->GetDataValuePair( "ActiveCameraObject" );
    if( dvp )
    {
        dvp->GetData( value );

        m_currentCameraSelection = value;
        if( m_currentCameraSelection == int( m_cameraComboBox->GetStrings().size() ) )
        {
            m_currentCameraSelection = -1;
            m_cameraComboBox->SetValue( wxT( "Select a Camera" ) );

            return;
        }

        m_cameraComboBox->SetSelection( m_currentCameraSelection );
    }

    dvp = command->GetDataValuePair( "AddCameraObject" );
    if( dvp )
    {
        wxCommandEvent event;
        OnAddCameraButton( event );
    }
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateMarkerData()
{
    const open::xml::CommandPtr command =
        mServiceList->GetGUIUpdateCommands( "UPDATE_NEW_MARKER_OBJECT" );

    //Hasn't updated yet
    if( command->GetCommandName() == "NULL" )
    {
        return;
    }

    unsigned int position;
    const open::xml::DataValuePairPtr dvpI =
        command->GetDataValuePair( "MarkerPosition" );
    dvpI->GetData( position );

    std::string name;
    const open::xml::DataValuePairPtr dvpII =
        command->GetDataValuePair( "MarkerName" );
    dvpII->GetData( name );

    wxString markerName(
        wxT( "Marker" ) +
        wxString::Format( wxT( "%i" ), m_markerNameNum ) );
    m_currentMarkerSelection = m_markerComboBox->Append( markerName );
    m_markerComboBox->SetStringSelection( markerName );

    ++m_markerNameNum;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAutoComputerFarPlane( wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = m_autoComputeFarButton->GetSelection();
    //0 = On
    //1 = Off
    if( selection )
    {
        mNearPlaneSpinCtrl->Disable();
        mNearPlaneSlider->Disable();
        mFarPlaneSpinCtrl->Disable();
        mFarPlaneSlider->Disable();
    }
    else
    {
        mNearPlaneSpinCtrl->Enable();
        mNearPlaneSlider->Enable();
        mFarPlaneSpinCtrl->Enable();
        mFarPlaneSlider->Enable();
    }

    mCommandName = "AUTO_COMPUTER_NEAR_FAR_PLANE";
    
    ves::open::xml::DataValuePairSharedPtr 
        tempDVP( new ves::open::xml::DataValuePair() );
    tempDVP->SetData( "autoComputeNearFarPlane", selection );
    mInstructions.push_back( tempDVP );
    
    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraManagerEvent( wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int selection = m_cameraManagerButton->GetSelection();
    
    mCommandName = "CAMERA_MANAGER_ON_OFF";
    
    ves::open::xml::DataValuePairSharedPtr 
        tempDVP( new ves::open::xml::DataValuePair() );
    tempDVP->SetData( "cameraManagerOnOff", selection );
    mInstructions.push_back( tempDVP );
    
    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
