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
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "CameraPlacementToolUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/conductor/util/spinctld.h>

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

using namespace cpt;

BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )
    EVT_RADIOBOX(
        ID_DRUM_ANIMATION_ON_OFF,
        CameraPlacementToolUIDialog::OnDrumAnimationOnOffRadioBox )
    EVT_RADIOBOX(
        ID_CAMERA_GEOMETRY_ON_OFF,
        CameraPlacementToolUIDialog::OnCameraGeometryOnOffRadioBox )
    EVT_RADIOBOX(
        ID_FRUSTUM_GEOMETRY_ON_OFF,
        CameraPlacementToolUIDialog::OnFrustumGeometryOnOffRadioBox )

    EVT_RADIOBOX(
        ID_DEPTH_OF_FIELD_EFFECT_ON_OFF,
        CameraPlacementToolUIDialog::OnDepthOfFieldEffectOnOffRadioBox )
    EVT_RADIOBOX(
        ID_PROJECTION_EFFECT_ON_OFF,
        CameraPlacementToolUIDialog::OnProjectionEffectOnOffRadioBox )
    EVT_SLIDER(
        ID_PROJECTION_EFFECT_OPACITY,
        CameraPlacementToolUIDialog::OnProjectionEffectOpacitySlider )

    EVT_RADIOBOX(
        ID_CAMERA_WINDOW_ON_OFF,
        CameraPlacementToolUIDialog::OnCameraWindowOnOffRadioBox )
    EVT_SLIDER(
        ID_CAMERA_WINDOW_RESOLUTION,
        CameraPlacementToolUIDialog::OnCameraWindowResolutionSlider )

    EVT_RADIOBOX(
        ID_DEPTH_HELPER_WINDOW_ON_OFF,
        CameraPlacementToolUIDialog::OnDepthHelperWindowOnOffRadioBox )
    EVT_SLIDER(
        ID_DEPTH_HELPER_WINDOW_RESOLUTION,
        CameraPlacementToolUIDialog::OnDepthHelperWindowResolutionSlider )

    EVT_COMMAND_SCROLL(
        ID_FIELD_OF_VIEW_SPINCTRL,
        CameraPlacementToolUIDialog::OnFieldOfViewSpinCtrl )
    EVT_TEXT_ENTER(
        ID_FIELD_OF_VIEW_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFieldOfViewText )
    EVT_SLIDER(
        ID_FIELD_OF_VIEW_SLIDER,
        CameraPlacementToolUIDialog::OnFieldOfViewSlider )
    EVT_COMMAND_SCROLL(
        ID_ASPECT_RATIO_SPINCTRL,
        CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl )
    EVT_TEXT_ENTER(
        ID_ASPECT_RATIO_SPINCTRL, 
        CameraPlacementToolUIDialog::OnAspectRatioText )
    EVT_SLIDER(
        ID_ASPECT_RATIO_SLIDER,
        CameraPlacementToolUIDialog::OnAspectRatioSlider )
    EVT_COMMAND_SCROLL(
        ID_NEAR_PLANE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnNearPlaneSpinCtrl )
    EVT_TEXT_ENTER(
        ID_NEAR_PLANE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnNearPlaneText )
    EVT_SLIDER(
        ID_NEAR_PLANE_SLIDER, 
        CameraPlacementToolUIDialog::OnNearPlaneSlider )
    EVT_COMMAND_SCROLL(
        ID_FAR_PLANE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFarPlaneSpinCtrl )
    EVT_TEXT_ENTER(
        ID_FAR_PLANE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFarPlaneText )
    EVT_SLIDER(
        ID_FAR_PLANE_SLIDER, 
        CameraPlacementToolUIDialog::OnFarPlaneSlider )

    EVT_COMMAND_SCROLL(
        ID_FOCAL_DISTANCE_SPINCTRL,
        CameraPlacementToolUIDialog::OnFocalDistanceSpinCtrl )
    EVT_TEXT_ENTER(
        ID_FOCAL_DISTANCE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFocalDistanceText )
    EVT_SLIDER(
        ID_FOCAL_DISTANCE_SLIDER,
        CameraPlacementToolUIDialog::OnFocalDistanceSlider )
    EVT_COMMAND_SCROLL(
        ID_FOCUS_RANGE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFocusRangeSpinCtrl )
    EVT_TEXT_ENTER(
        ID_FOCUS_RANGE_SPINCTRL, 
        CameraPlacementToolUIDialog::OnFocusRangeText )
    EVT_SLIDER(
        ID_FOCUS_RANGE_SLIDER, 
        CameraPlacementToolUIDialog::OnFocusRangeSlider )
    EVT_COMMAND_SCROLL(
        ID_MAX_CIRCLE_OF_CONFUSION_SPINCTRL, 
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSpinCtrl )
    EVT_TEXT_ENTER(
        ID_MAX_CIRCLE_OF_CONFUSION_SPINCTRL, 
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionText )
    EVT_SLIDER(
        ID_MAX_CIRCLE_OF_CONFUSION_SLIDER, 
        CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSlider )
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
UIDialog( ( wxWindow* )parent, id, wxT( "CameraPlacementTool" ) )
{
    mProjectionData[ 0 ] = 30.0;
    mProjectionData[ 1 ] = 1.0;
    mProjectionData[ 2 ] = 5.0;
    mProjectionData[ 3 ] = 10.0;

    mServiceList = service;

    BuildGUI();
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::~CameraPlacementToolUIDialog()
{
    mServiceList->CleanUp();
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
void CameraPlacementToolUIDialog::Lock( bool l )
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
    mainPanel->SetBackgroundColour( wxColour( 150, 150, 255 ) );

    wxBoxSizer* mainPanelSizer;
    mainPanelSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticBoxSizer* lensSettingsSizer;
    lensSettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        mainPanel, wxID_ANY, wxT( "Lens Settings" ) ), wxVERTICAL );

    mainPanelSizer->Add( lensSettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* geometrySettingsSizer;
    geometrySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        mainPanel, wxID_ANY, wxT( "Geometry Settings" ) ), wxHORIZONTAL );

    wxBoxSizer* drumAnimationOnOffSizer;
    drumAnimationOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mDrumAnimationOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mDrumAnimationOnOffNChoices =
        sizeof( mDrumAnimationOnOffChoices ) / sizeof( wxString );
    mDrumAnimationOnOff = new wxRadioBox(
        mainPanel, ID_DRUM_ANIMATION_ON_OFF, wxT( "Drum Animation" ),
        wxDefaultPosition, wxSize( 144,-1 ), mDrumAnimationOnOffNChoices,
        mDrumAnimationOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mDrumAnimationOnOff->SetSelection( 0 );
    mDrumAnimationOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    drumAnimationOnOffSizer->Add( mDrumAnimationOnOff, 0, wxALL, 5 );

    geometrySettingsSizer->Add( drumAnimationOnOffSizer, 0, wxALL, 5 );

    wxBoxSizer* cameraGeometryOnOffSizer;
    cameraGeometryOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraGeometryOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraGeometryOnOffNChoices =
        sizeof( mCameraGeometryOnOffChoices ) / sizeof( wxString );
    mCameraGeometryOnOff = new wxRadioBox(
        mainPanel, ID_CAMERA_GEOMETRY_ON_OFF, wxT( "Camera Geometry" ),
        wxDefaultPosition, wxSize( 144,-1 ), mCameraGeometryOnOffNChoices,
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
        mainPanel, ID_FRUSTUM_GEOMETRY_ON_OFF, wxT( "Frustum Geometry" ),
        wxDefaultPosition, wxSize( 144,-1 ), mFrustumGeometryOnOffNChoices,
        mFrustumGeometryOnOffChoices, 1, wxRA_SPECIFY_ROWS );
    mFrustumGeometryOnOff->SetSelection( 1 );
    mFrustumGeometryOnOff->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    frustumGeometryOnOffSizer->Add( mFrustumGeometryOnOff, 0, wxALL, 5 );

    geometrySettingsSizer->Add( frustumGeometryOnOffSizer, 0, wxALL, 5 );

    mainPanelSizer->Add( geometrySettingsSizer, 0, wxALL|wxEXPAND, 10 );

    wxStaticBoxSizer* displaySettingsSizer;
    displaySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        mainPanel, wxID_ANY, wxT( "Display Settings" ) ), wxVERTICAL );

    wxBoxSizer* depthOfFieldEffectOnOffSizer;
    depthOfFieldEffectOnOffSizer = new wxBoxSizer( wxVERTICAL );

    wxString mDepthOfFieldEffectOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mDepthOfFieldEffectOnOffNChoices =
        sizeof( mDepthOfFieldEffectOnOffChoices ) / sizeof( wxString );
    mDepthOfFieldEffectOnOff = new wxRadioBox(
        mainPanel, ID_DEPTH_OF_FIELD_EFFECT_ON_OFF,
        wxT( "Depth of Field Effect" ), wxDefaultPosition, wxSize( 144,-1 ),
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
        mainPanel, ID_PROJECTION_EFFECT_ON_OFF, wxT( "Projection Effect" ),
        wxDefaultPosition, wxSize( 144,-1 ), mProjectionEffectOnOffNChoices,
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
        mainPanel, ID_PROJECTION_EFFECT_OPACITY, 0, 0, 1,
        wxPoint( -1,-1 ), wxSize( -1,-1 ),
        wxSL_BOTH | wxSL_HORIZONTAL | wxSL_LABELS | wxSL_TOP );
    projectionEffectOpacitySizer->Add(
        mProjectionEffectOpacity, 0, wxEXPAND, 5 );

    projectionEffectSizer->Add( projectionEffectOpacitySizer, 1, wxALL, 5 );

    displaySettingsSizer->Add( projectionEffectSizer, 0, wxEXPAND, 5 );

    wxStaticLine* displaySettingsSeparator;
    displaySettingsSeparator = new wxStaticLine(
        mainPanel, wxID_ANY, wxDefaultPosition, wxDefaultSize,
        wxLI_HORIZONTAL );
    displaySettingsSizer->Add(
        displaySettingsSeparator, 0, wxEXPAND | wxALL, 5 );

    wxBoxSizer* cameraWindowSizer;
    cameraWindowSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* cameraWindowOnOffSizer;
    cameraWindowOnOffSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraWindowOnOffChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraWindowOnOffNChoices =
        sizeof( mCameraWindowOnOffChoices ) / sizeof( wxString );
    mCameraWindowOnOff = new wxRadioBox(
        mainPanel, ID_CAMERA_WINDOW_ON_OFF, wxT( "Camera Window" ),
        wxDefaultPosition, wxSize( 144,-1 ), mCameraWindowOnOffNChoices,
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
        mainPanel, ID_CAMERA_WINDOW_RESOLUTION, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1,-1 ),
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
        mainPanel, ID_DEPTH_HELPER_WINDOW_ON_OFF, wxT( "Depth Helper Window" ),
        wxDefaultPosition, wxSize( 144,-1 ), mDepthHelperWindowOnOffNChoices,
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
        mainPanel, ID_DEPTH_HELPER_WINDOW_RESOLUTION, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1,-1 ),
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
    cameraPanel->SetBackgroundColour( wxColour( 150, 150, 255 ) );

    wxBoxSizer* cameraPanelSizer;
    cameraPanelSizer = new wxBoxSizer( wxVERTICAL );

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
        *cameraPanel, ID_FIELD_OF_VIEW_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144,-1 ), wxSP_ARROW_KEYS,
        0, 180, mProjectionData[ 0 ], 0.1 );
    fieldOfViewSpinSizer->Add( mFieldOfViewSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    fieldOfViewTextSpinSizer->Add( fieldOfViewSpinSizer, 0, 0, 5 );

    fieldOfViewSizer->Add( fieldOfViewTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* fieldOfViewSliderSizer;
    fieldOfViewSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFieldOfViewSlider = new wxSlider(
        cameraPanel, ID_FIELD_OF_VIEW_SLIDER, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
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
        *cameraPanel, ID_ASPECT_RATIO_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS, 0, 1, 1 );
    aspectRatioSpinSizer->Add( mAspectRatioSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    aspectRatioTextSpinSizer->Add( aspectRatioSpinSizer, 0, 0, 5 );

    aspectRatioSizer->Add( aspectRatioTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSliderSizer;
    aspectRatioSliderSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSlider = new wxSlider(
        cameraPanel, ID_ASPECT_RATIO_SLIDER, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    aspectRatioSliderSizer->Add(
        mAspectRatioSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

    aspectRatioSizer->Add( aspectRatioSliderSizer, 1, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( aspectRatioSizer, 0, wxALL | wxEXPAND, 5 );

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
        *cameraPanel, ID_NEAR_PLANE_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxSize( 144, -1 ), wxSP_ARROW_KEYS, 0, 100, 5 );
    nearPlaneSpinSizer->Add( mNearPlaneSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    nearPlaneTextSpinSizer->Add( nearPlaneSpinSizer, 0, 0, 5 );

    nearPlaneSizer->Add( nearPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSliderSizer;
    nearPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mNearPlaneSlider = new wxSlider(
        cameraPanel, ID_NEAR_PLANE_SLIDER, 200, 0, 1000,
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
        cameraPanel, ID_FAR_PLANE_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxSize( 144,-1 ), wxSP_ARROW_KEYS, 0, 100, 10 );
    farPlaneSpinSizer->Add( mFarPlaneSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    farPlaneTextSpinSizer->Add( farPlaneSpinSizer, 0, 0, 5 );

    farPlaneSizer->Add( farPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSliderSizer;
    farPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFarPlaneSlider = new wxSlider(
        cameraPanel, ID_FAR_PLANE_SLIDER, 200, 0, 1000,
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
        *cameraPanel, ID_FOCAL_DISTANCE_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS, 0, 180, 30 );
    focalDistanceSpinSizer->Add(
        mFocalDistanceSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    focalDistanceTextSpinSizer->Add( focalDistanceSpinSizer, 0, 0, 5 );

    focalDistanceSizer->Add( focalDistanceTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* focalDistanceSliderSizer;
    focalDistanceSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFocalDistanceSlider = new wxSlider(
        cameraPanel, ID_FOCAL_DISTANCE_SLIDER, 200, 0, 1000,
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

    mFocusRangeSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *cameraPanel, ID_FOCUS_RANGE_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144, -1 ), wxSP_ARROW_KEYS, 0, 1, 1 );
    focusRangeSpinSizer->Add( mFocusRangeSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    focusRangeTextSpinSizer->Add( focusRangeSpinSizer, 0, 0, 5 );

    focusRangeSizer->Add( focusRangeTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* focusRangeSliderSizer;
    focusRangeSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFocusRangeSlider = new wxSlider(
        cameraPanel, ID_FOCUS_RANGE_SLIDER, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    focusRangeSliderSizer->Add(
        mFocusRangeSlider, 0, wxEXPAND | wxLEFT | wxRIGHT, 5 );

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
        *cameraPanel, ID_MAX_CIRCLE_OF_CONFUSION_SPINCTRL, wxEmptyString,
        wxDefaultPosition, wxSize( 144,-1 ), wxSP_ARROW_KEYS, 0, 100, 5 );
    maxCircleOfConfusionSpinSizer->Add(
        mMaxCircleOfConfusionSpinCtrl, 0, wxLEFT | wxRIGHT, 5 );

    maxCircleOfConfusionTextSpinSizer->Add(
        maxCircleOfConfusionSpinSizer, 0, 0, 5 );

    maxCircleOfConfusionSizer->Add(
        maxCircleOfConfusionTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* maxCircleOfConfusionSliderSizer;
    maxCircleOfConfusionSliderSizer = new wxBoxSizer( wxVERTICAL );

    mMaxCircleOfConfusionSlider = new wxSlider(
        cameraPanel, ID_MAX_CIRCLE_OF_CONFUSION_SLIDER, 200, 0, 1000,
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
void CameraPlacementToolUIDialog::OnDrumAnimationOnOffRadioBox(
    wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraGeometryOnOffRadioBox(
    wxCommandEvent& event )
{
    unsigned int selection = mCameraWindowOnOff->GetSelection();

    mCommandName = "CAMERA_GEOMETRY_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr toggleCameraDVP(
        new ves::open::xml::DataValuePair() );
    toggleCameraDVP->SetData( "toggleCamera", selection );
    mInstructions.push_back( toggleCameraDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFrustumGeometryOnOffRadioBox(
    wxCommandEvent& event )
{
    unsigned int selection = mFrustumGeometryOnOff->GetSelection();

    mCommandName = "FRUSTUM_GEOMETRY_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr toggleFrustumDVP(
        new ves::open::xml::DataValuePair() );
    toggleFrustumDVP->SetData( "toggleFrustum", selection );
    mInstructions.push_back( toggleFrustumDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthOfFieldEffectOnOffRadioBox(
    wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnProjectionEffectOnOffRadioBox(
    wxCommandEvent& event )
{
    unsigned int selection = mProjectionEffectOnOff->GetSelection();

    mCommandName = "PROJECTION_EFFECT_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr toggleProjectionDVP(
        new ves::open::xml::DataValuePair() );
    toggleProjectionDVP->SetData( "toggleProjection", selection );
    mInstructions.push_back( toggleProjectionDVP );

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

    ves::open::xml::DataValuePairSharedPtr opacityDVP(
        new ves::open::xml::DataValuePair() );
    opacityDVP->SetData( "opacity", opacity );
    mInstructions.push_back( opacityDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraWindowOnOffRadioBox(
    wxCommandEvent& event )
{
    unsigned int selection = mCameraWindowOnOff->GetSelection();

    mCommandName = "CAMERA_WINDOW_ON_OFF";

    ves::open::xml::DataValuePairSharedPtr viewPerspectiveDVP(
        new ves::open::xml::DataValuePair() );
    viewPerspectiveDVP->SetData( "viewPerspective", selection );
    mInstructions.push_back( viewPerspectiveDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraWindowResolutionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mCameraWindowResolution->GetValue();

    mCommandName = "CAMERA_WINDOW_RESOLUTION";

    ves::open::xml::DataValuePairSharedPtr resolutionDVP(
        new ves::open::xml::DataValuePair() );
    resolutionDVP->SetData( "resolution", value );
    mInstructions.push_back( resolutionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthHelperWindowOnOffRadioBox(
    wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnDepthHelperWindowResolutionSlider(
    wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFieldOfViewSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFoVZControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFieldOfViewText( wxCommandEvent& event )
{
    UpdateFoVZControls();
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
void CameraPlacementToolUIDialog::OnAspectRatioText( wxCommandEvent& event )
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
void CameraPlacementToolUIDialog::OnNearPlaneText( wxCommandEvent& event )
{
    UpdateNearPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnNearPlaneSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    if( mNearPlaneSlider->GetValue() >= mFarPlaneSlider->GetValue() )
    {
        EnsureSliders( ID_NEAR_PLANE_SLIDER );
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
void CameraPlacementToolUIDialog::OnFarPlaneText( wxCommandEvent& event )
{
    UpdateFarPlaneControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFarPlaneSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    if( mFarPlaneSlider->GetValue() <= mNearPlaneSlider->GetValue() )
    {
        EnsureSliders( ID_FAR_PLANE_SLIDER );
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

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalDistanceText( wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocalDistanceSlider(
    wxCommandEvent& WXUNUSED( event ) )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocusRangeSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocusRangeText( wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFocusRangeSlider(
    wxCommandEvent& WXUNUSED( event ) )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionText(
    wxCommandEvent& event )
{

}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnMaxCircleOfConfusionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{

}
////////////////////////////////////////////////////////////////////////////////
bool CameraPlacementToolUIDialog::EnsureSliders( int activeSliderID )
{
    int mNearPlaneValue = mNearPlaneSlider->GetValue();
    int mFarPlaneValue = mFarPlaneSlider->GetValue();

    //maintain the value on the min/max sliders.
    if( mNearPlaneValue >= mFarPlaneValue )
    {
        if( mNearPlaneValue == 1000 )
        {
            mNearPlaneSlider->SetValue( 1000 - 1 );
        }
        else if( mFarPlaneValue == 0 )
        {
            mFarPlaneSlider->SetValue( 1 );
        }

        if( activeSliderID == ID_NEAR_PLANE_SLIDER )
        {
            mFarPlaneSlider->SetValue( mNearPlaneSlider->GetValue() + 1 );
            return true;
        }
        else if( activeSliderID == ID_FAR_PLANE_SLIDER )
        {
            mNearPlaneSlider->SetValue( mFarPlaneSlider->GetValue() - 1 );
            return true;
        }
    }
    return false;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFoVZControls()
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

    if( mFarPlaneSpinCtrl->GetValue() <= nearPlaneValue )
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue ) * 10 );
        mFarPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue ) * 10 + 1 );
        mFarPlaneSpinCtrl->SetValue( nearPlaneValue + 0.1 );
    }
    else
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( nearPlaneValue ) * 10 );
    }

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::UpdateFarPlaneControls()
{
    double farPlaneValue = mFarPlaneSpinCtrl->GetValue();

    if( mNearPlaneSpinCtrl->GetValue() >= farPlaneValue )
    {
        mNearPlaneSlider->SetValue(
            static_cast< int >( farPlaneValue ) * 10 - 1 );
        mFarPlaneSlider->SetValue(
            static_cast< int >( farPlaneValue ) * 10 );
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
        static_cast< int >( farPlaneValue ) * 10 );
    }

    mProjectionData[ 2 ] = mNearPlaneSpinCtrl->GetValue();
    mProjectionData[ 3 ] = mFarPlaneSpinCtrl->GetValue();

    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::ProjectionUpdate()
{
    mCommandName = std::string( "PROJECTION_UPDATE" );

    ves::open::xml::DataValuePairSharedPtr projectionFoVZDVP(
        new ves::open::xml::DataValuePair() );
    projectionFoVZDVP->SetData(
        "projectionFoVZ", mProjectionData[ 0 ] );
    mInstructions.push_back( projectionFoVZDVP );

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

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
