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

using namespace cpt;

BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )
EVT_COMMAND_SCROLL( ID_FOVZ_SPINCTRL, 
                    CameraPlacementToolUIDialog::OnFoVZSpinCtrl )
EVT_TEXT_ENTER( ID_FOVZ_SPINCTRL, 
                CameraPlacementToolUIDialog::OnFoVZText )
EVT_SLIDER( ID_FOVZ_SLIDER,
            CameraPlacementToolUIDialog::OnFoVZSlider )
EVT_COMMAND_SCROLL( ID_ASPECTRATIO_SPINCTRL,
                    CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl )
EVT_TEXT_ENTER( ID_ASPECTRATIO_SPINCTRL, 
                CameraPlacementToolUIDialog::OnAspectRatioText )
EVT_SLIDER( ID_ASPECTRATIO_SLIDER,
            CameraPlacementToolUIDialog::OnAspectRatioSlider )
EVT_COMMAND_SCROLL( ID_NEARPLANE_SPINCTRL, 
                    CameraPlacementToolUIDialog::OnNearPlaneSpinCtrl )
EVT_TEXT_ENTER( ID_NEARPLANE_SPINCTRL, 
                CameraPlacementToolUIDialog::OnNearPlaneText )
EVT_SLIDER( ID_NEARPLANE_SLIDER, 
            CameraPlacementToolUIDialog::OnNearPlaneSlider )
EVT_COMMAND_SCROLL( ID_FARPLANE_SPINCTRL, 
                    CameraPlacementToolUIDialog::OnFarPlaneSpinCtrl )
EVT_TEXT_ENTER( ID_FARPLANE_SPINCTRL, 
                CameraPlacementToolUIDialog::OnFarPlaneText )
EVT_SLIDER( ID_FARPLANE_SLIDER, 
            CameraPlacementToolUIDialog::OnFarPlaneSlider )
EVT_RADIOBOX( ID_CAMERAVIEW_RADIOBOX,
              CameraPlacementToolUIDialog::OnCameraViewRadioBox )
EVT_SLIDER( ID_RESOLUTION_SLIDER,
            CameraPlacementToolUIDialog::OnResolutionSlider )
EVT_RADIOBOX( ID_PROJECTION_RADIOBOX,
              CameraPlacementToolUIDialog::OnProjectionRadioBox )
EVT_SLIDER( ID_OPACITY_SLIDER,
            CameraPlacementToolUIDialog::OnOpacitySlider )
EVT_RADIOBOX( ID_CAMERA_RADIOBOX,
              CameraPlacementToolUIDialog::OnCameraRadioBox )
EVT_RADIOBOX( ID_FRUSTUM_RADIOBOX,
              CameraPlacementToolUIDialog::OnFrustumRadioBox )
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
    SetBackgroundColour( wxColour( 255, 180, 0 ) );

    wxBoxSizer* mainSizer;
    mainSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticBoxSizer* projectionSettingsSizer;
    projectionSettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        this, wxID_ANY, wxT( "Projection Settings" ) ), wxVERTICAL );

    wxBoxSizer* fovzSizer;
    fovzSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* fovzTextSpinSizer;
    fovzTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* fovzTextSizer;
    fovzTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* fovzText;
    fovzText = new wxStaticText(
        this, wxID_ANY, wxT( "Field of View( z )" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    fovzText->Wrap( -1 );
    fovzTextSizer->Add( fovzText, 0, wxALL, 5 );

    fovzTextSpinSizer->Add( fovzTextSizer, 0, 0, 5 );

    wxBoxSizer* fovzSpinSizer;
    fovzSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFoVZSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *this, ID_FOVZ_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxDefaultSize, wxSP_ARROW_KEYS, 0, 180, mProjectionData[ 0 ], 0.1 );
    fovzSpinSizer->Add( mFoVZSpinCtrl, 0, wxLEFT, 5 );

    fovzTextSpinSizer->Add( fovzSpinSizer, 0, 0, 5 );

    fovzSizer->Add( fovzTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* fovzSliderSizer;
    fovzSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFoVZSlider = new wxSlider(
        this, ID_FOVZ_SLIDER, mProjectionData[ 0 ] * 10.0, 0, 1800,
        wxDefaultPosition, wxSize( 200, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL );
    fovzSliderSizer->Add( mFoVZSlider, 0, 0, 5 );

    fovzSizer->Add( fovzSliderSizer, 0, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( fovzSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSizer;
    aspectRatioSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* aspectRatioTextSpinSizer;
    aspectRatioTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* aspectRatioTextSizer;
    aspectRatioTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* aspectRatioText;
    aspectRatioText = new wxStaticText(
        this, wxID_ANY, wxT( "Aspect Ratio:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    aspectRatioText->Wrap( -1 );
    aspectRatioTextSizer->Add( aspectRatioText, 0, wxALL, 5 );

    aspectRatioTextSpinSizer->Add( aspectRatioTextSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSpinSizer;
    aspectRatioSpinSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *this, ID_ASPECTRATIO_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, mProjectionData[ 1 ], 0.1 );
    aspectRatioSpinSizer->Add( mAspectRatioSpinCtrl, 0, wxLEFT, 5 );

    aspectRatioTextSpinSizer->Add( aspectRatioSpinSizer, 0, 0, 5 );

    aspectRatioSizer->Add( aspectRatioTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* aspectRatioSliderSizer;
    aspectRatioSliderSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSlider = new wxSlider(
        this, ID_ASPECTRATIO_SLIDER, mProjectionData[ 1 ] * 10.0, 0, 100,
        wxDefaultPosition, wxSize( 200, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL );
    aspectRatioSliderSizer->Add( mAspectRatioSlider, 0, 0, 5 );

    aspectRatioSizer->Add( aspectRatioSliderSizer, 0, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( aspectRatioSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSizer;
    nearPlaneSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* nearPlaneTextSpinSizer;
    nearPlaneTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* nearPlaneTextSizer;
    nearPlaneTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* nearPlaneText;
    nearPlaneText = new wxStaticText(
        this, wxID_ANY, wxT( "Near Plane:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    nearPlaneText->Wrap( -1 );
    nearPlaneTextSizer->Add( nearPlaneText, 0, wxALL, 5 );

    nearPlaneTextSpinSizer->Add( nearPlaneTextSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSpinSizer;
    nearPlaneSpinSizer = new wxBoxSizer( wxVERTICAL );

    mNearPlaneSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl( 
        *this, ID_NEARPLANE_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxDefaultSize, wxSP_ARROW_KEYS, 0, 100, mProjectionData[ 2 ], 0.1 );
    nearPlaneSpinSizer->Add( mNearPlaneSpinCtrl, 0, wxLEFT, 5 );

    nearPlaneTextSpinSizer->Add( nearPlaneSpinSizer, 0, 0, 5 );

    nearPlaneSizer->Add( nearPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* nearPlaneSliderSizer;
    nearPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mNearPlaneSlider = new wxSlider(
        this, ID_NEARPLANE_SLIDER, mProjectionData[ 2 ] * 10.0, 0, 1000,
        wxDefaultPosition, wxSize( 200, -1 ), wxSL_BOTH | wxSL_HORIZONTAL );
    nearPlaneSliderSizer->Add( mNearPlaneSlider, 0, 0, 5 );

    nearPlaneSizer->Add( nearPlaneSliderSizer, 0, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( nearPlaneSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSizer;
    farPlaneSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* farPlaneTextSpinSizer;
    farPlaneTextSpinSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* farPlaneTextSizer;
    farPlaneTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* farPlaneText;
    farPlaneText = new wxStaticText(
        this, wxID_ANY, wxT( "Far Plane:" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    farPlaneText->Wrap( -1 );
    farPlaneTextSizer->Add( farPlaneText, 0, wxALL, 5 );

    farPlaneTextSpinSizer->Add( farPlaneTextSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSpinSizer;
    farPlaneSpinSizer = new wxBoxSizer( wxVERTICAL );

    mFarPlaneSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *this, ID_FARPLANE_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxDefaultSize, wxSP_ARROW_KEYS, 0.1, 100, mProjectionData[ 3 ], 0.1 );
    farPlaneSpinSizer->Add( mFarPlaneSpinCtrl, 0, wxLEFT, 5 );

    farPlaneTextSpinSizer->Add( farPlaneSpinSizer, 0, 0, 5 );

    farPlaneSizer->Add( farPlaneTextSpinSizer, 0, 0, 5 );

    wxBoxSizer* farPlaneSliderSizer;
    farPlaneSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFarPlaneSlider = new wxSlider(
        this, ID_FARPLANE_SLIDER, mProjectionData[ 3 ] * 10.0, 0, 1000,
        wxDefaultPosition, wxSize( 200, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL );
    farPlaneSliderSizer->Add( mFarPlaneSlider, 0, 0, 5 );

    farPlaneSizer->Add( farPlaneSliderSizer, 0, wxALIGN_BOTTOM, 5 );

    projectionSettingsSizer->Add( farPlaneSizer, 0, 0, 5 );

    mainSizer->Add( projectionSettingsSizer, 0, wxALL | wxEXPAND, 5 );

    wxStaticBoxSizer* displaySettingsSizer;
    displaySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        this, wxID_ANY, wxT( "Display Settings" ) ), wxVERTICAL );

    wxBoxSizer* cameraViewSizer;
    cameraViewSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* cameraViewRadioBoxSizer;
    cameraViewRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraViewRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraViewRadioBoxNChoices =
        sizeof( mCameraViewRadioBoxChoices ) / sizeof( wxString );
    mCameraViewRadioBox = new wxRadioBox(
        this, ID_CAMERAVIEW_RADIOBOX, wxT( "Camera Window" ),
        wxDefaultPosition, wxDefaultSize, mCameraViewRadioBoxNChoices,
        mCameraViewRadioBoxChoices, 1, wxRA_SPECIFY_ROWS );
    mCameraViewRadioBox->SetSelection( 1 );
    mCameraViewRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    cameraViewRadioBoxSizer->Add( mCameraViewRadioBox, 0, wxALL, 5 );

    cameraViewSizer->Add( cameraViewRadioBoxSizer, 0, wxALIGN_BOTTOM, 5 );

    wxBoxSizer* resolutionSizer;
    resolutionSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* resolutionText;
    resolutionText = new wxStaticText(
        this, wxID_ANY, wxT( "Resolution( pixels )" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    resolutionText->Wrap( -1 );
    resolutionSizer->Add( resolutionText, 0, wxBOTTOM | wxLEFT, 5 );

    mResolutionSlider = new wxSlider(
        this, ID_RESOLUTION_SLIDER, 200, 0, 1000,
        wxDefaultPosition, wxSize( -1, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL | wxSL_LABELS | wxSL_TOP );
    resolutionSizer->Add( mResolutionSlider, 0, wxEXPAND, 5 );

    cameraViewSizer->Add( resolutionSizer, 1, wxEXPAND, 5 );

    displaySettingsSizer->Add( cameraViewSizer, 0, wxEXPAND, 5 );

    wxStaticLine* staticLine;
    staticLine = new wxStaticLine(
        this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
    displaySettingsSizer->Add( staticLine, 0, wxALL | wxEXPAND, 10 );

    wxBoxSizer* projectionSizer;
    projectionSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* projectionRadioBoxSizer;
    projectionRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mProjectionRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mProjectionRadioBoxNChoices =
        sizeof( mProjectionRadioBoxChoices ) / sizeof( wxString );
    mProjectionRadioBox = new wxRadioBox(
        this, ID_PROJECTION_RADIOBOX, wxT( "Projection Effect" ),
        wxDefaultPosition, wxDefaultSize, mProjectionRadioBoxNChoices,
        mProjectionRadioBoxChoices, 1, wxRA_SPECIFY_ROWS );
    mProjectionRadioBox->SetSelection( 1 );
    mProjectionRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    projectionRadioBoxSizer->Add( mProjectionRadioBox, 0, wxALL, 5 );

    projectionSizer->Add( projectionRadioBoxSizer, 0, wxALIGN_BOTTOM, 5 );

    wxBoxSizer* opacitySizer;
    opacitySizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* opacityText;
    opacityText = new wxStaticText(
        this, wxID_ANY, wxT( "Opacity" ),
        wxDefaultPosition, wxDefaultSize, 0 );
    opacityText->Wrap( -1 );
    opacitySizer->Add( opacityText, 0, wxBOTTOM | wxLEFT, 5 );

    mOpacitySlider = new wxSlider(
        this, ID_OPACITY_SLIDER, 30, 0, 100,
        wxDefaultPosition, wxSize( -1, -1 ),
        wxSL_BOTH | wxSL_HORIZONTAL );
    opacitySizer->Add( mOpacitySlider, 0, wxEXPAND, 5 );

    projectionSizer->Add( opacitySizer, 1, wxEXPAND, 5 );

    displaySettingsSizer->Add( projectionSizer, 0, wxEXPAND, 5 );

    mainSizer->Add( displaySettingsSizer, 0, wxALL | wxEXPAND, 5 );

    wxStaticBoxSizer* geometrySettingsSizer;
    geometrySettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        this, wxID_ANY, wxT( "Geometry Settings" ) ), wxVERTICAL );

    wxBoxSizer* geometryCenterSizer;
    geometryCenterSizer = new wxBoxSizer( wxHORIZONTAL );

    wxBoxSizer* cameraRadioBoxSizer;
    cameraRadioBoxSizer = new wxBoxSizer( wxVERTICAL );

    wxString mCameraRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraRadioBoxNChoices =
        sizeof( mCameraRadioBoxChoices ) / sizeof( wxString );
    mCameraRadioBox = new wxRadioBox(
        this, ID_CAMERA_RADIOBOX, wxT( "Camera" ),
        wxDefaultPosition, wxDefaultSize, mCameraRadioBoxNChoices,
        mCameraRadioBoxChoices, 1, wxRA_SPECIFY_ROWS );
    mCameraRadioBox->SetSelection( 1 );
    mCameraRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    cameraRadioBoxSizer->Add( mCameraRadioBox, 0, wxALIGN_RIGHT | wxALL, 5 );

    geometryCenterSizer->Add( cameraRadioBoxSizer, 1, wxEXPAND, 5 );

    wxBoxSizer* frustumRadioBoxSizer;
    frustumRadioBoxSizer = new wxBoxSizer( wxVERTICAL );

    wxString mFrustumRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mFrustumRadioBoxNChoices =
        sizeof( mFrustumRadioBoxChoices ) / sizeof( wxString );
    mFrustumRadioBox = new wxRadioBox(
        this, ID_FRUSTUM_RADIOBOX, wxT( "Frustum" ),
        wxDefaultPosition, wxDefaultSize, mFrustumRadioBoxNChoices,
        mFrustumRadioBoxChoices, 1, wxRA_SPECIFY_ROWS );
    mFrustumRadioBox->SetSelection( 1 );
    mFrustumRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    frustumRadioBoxSizer->Add( mFrustumRadioBox, 0, wxALIGN_LEFT | wxALL, 5 );

    geometryCenterSizer->Add( frustumRadioBoxSizer, 1, wxEXPAND, 5 );

    geometrySettingsSizer->Add( geometryCenterSizer, 0, wxEXPAND, 5 );

    mainSizer->Add( geometrySettingsSizer, 0, wxALL | wxEXPAND, 5 );

    wxStdDialogButtonSizer* stdDialogButtonSizer;
    wxButton* stdDialogButtonSizerOK;
    wxButton* stdDialogButtonSizerCancel;
    stdDialogButtonSizer = new wxStdDialogButtonSizer();
    stdDialogButtonSizerOK = new wxButton( this, wxID_OK );
    stdDialogButtonSizer->AddButton( stdDialogButtonSizerOK );
    stdDialogButtonSizerCancel = new wxButton( this, wxID_CANCEL );
    stdDialogButtonSizer->AddButton( stdDialogButtonSizerCancel );
    stdDialogButtonSizer->Realize();
    mainSizer->Add( stdDialogButtonSizer, 0, wxALL | wxEXPAND, 10 );

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
void CameraPlacementToolUIDialog::OnFoVZSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    UpdateFoVZControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFoVZText( wxCommandEvent& event )
{
    UpdateFoVZControls();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFoVZSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mProjectionData[ 0 ] =
        static_cast< double >( mFoVZSlider->GetValue() ) / 10.0;
    mFoVZSpinCtrl->SetValue( mProjectionData[ 0 ] );
    
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
        EnsureSliders( ID_NEARPLANE_SLIDER );
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
        EnsureSliders( ID_FARPLANE_SLIDER );
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
void CameraPlacementToolUIDialog::OnCameraViewRadioBox(
    wxCommandEvent& event )
{
    unsigned int selection = mCameraViewRadioBox->GetSelection();

    mCommandName = "CAMERA_VIEW_UPDATE";

    ves::open::xml::DataValuePairSharedPtr viewPerspectiveDVP(
        new ves::open::xml::DataValuePair() );
    viewPerspectiveDVP->SetData( "viewPerspective", selection );
    mInstructions.push_back( viewPerspectiveDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnResolutionSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mResolutionSlider->GetValue();

    mCommandName = "RESOLUTION_UPDATE";

    ves::open::xml::DataValuePairSharedPtr resolutionDVP(
        new ves::open::xml::DataValuePair() );
    resolutionDVP->SetData( "resolution", value );
    mInstructions.push_back( resolutionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnProjectionRadioBox( wxCommandEvent& event )
{
    unsigned int selection = mProjectionRadioBox->GetSelection();

    mCommandName = "TOGGLE_PROJECTION_UPDATE";

    ves::open::xml::DataValuePairSharedPtr toggleProjectionDVP(
        new ves::open::xml::DataValuePair() );
    toggleProjectionDVP->SetData( "toggleProjection", selection );
    mInstructions.push_back( toggleProjectionDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnOpacitySlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    unsigned int value = mOpacitySlider->GetValue();

    double opacity = static_cast< double >( value ) / 100.0;

    mCommandName = "OPACITY_UPDATE";

    ves::open::xml::DataValuePairSharedPtr opacityDVP(
        new ves::open::xml::DataValuePair() );
    opacityDVP->SetData( "opacity", opacity );
    mInstructions.push_back( opacityDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraRadioBox( wxCommandEvent& event )
{
    unsigned int selection = mCameraRadioBox->GetSelection();

    mCommandName = "TOGGLE_CAMERA_UPDATE";

    ves::open::xml::DataValuePairSharedPtr toggleCameraDVP(
        new ves::open::xml::DataValuePair() );
    toggleCameraDVP->SetData( "toggleCamera", selection );
    mInstructions.push_back( toggleCameraDVP );

    SendCommandsToXplorer();
    ClearInstructions();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnFrustumRadioBox( wxCommandEvent& event )
{
    unsigned int selection = mFrustumRadioBox->GetSelection();

    mCommandName = "TOGGLE_FRUSTUM_UPDATE";

    ves::open::xml::DataValuePairSharedPtr toggleFrustumDVP(
        new ves::open::xml::DataValuePair() );
    toggleFrustumDVP->SetData( "toggleFrustum", selection );
    mInstructions.push_back( toggleFrustumDVP );

    SendCommandsToXplorer();
    ClearInstructions();
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

        if( activeSliderID == ID_NEARPLANE_SLIDER )
        {
            mFarPlaneSlider->SetValue( mNearPlaneSlider->GetValue() + 1 );
            return true;
        }
        else if( activeSliderID == ID_FARPLANE_SLIDER )
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
    mProjectionData[ 0 ] = mFoVZSpinCtrl->GetValue();
    mFoVZSlider->SetValue(
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
