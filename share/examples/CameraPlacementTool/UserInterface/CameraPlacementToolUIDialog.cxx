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

using namespace cpt;

/*----------------------------------------------------------------------------*/
BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )

EVT_RADIOBOX( ID_CAMERA_RADIOBOX,
              CameraPlacementToolUIDialog::OnCameraRadioBox )
EVT_RADIOBOX( ID_FRUSTUM_RADIOBOX,
              CameraPlacementToolUIDialog::OnFrustumRadioBox )
EVT_RADIOBOX( ID_PROJECTION_RADIOBOX,
              CameraPlacementToolUIDialog::OnProjectionRadioBox )
EVT_SLIDER( ID_FOVZ_SLIDER, CameraPlacementToolUIDialog::OnFoVZSlider )
EVT_COMMAND_SCROLL( ID_ASPECTRATIO_SPINCTRL,
                    CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl )

END_EVENT_TABLE()
/*----------------------------------------------------------------------------*/

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
UIDialog( ( wxWindow* )parent, id, _( "CameraPlacementTool" ) )
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

    wxStaticBoxSizer* toggleSizer;
    toggleSizer = new wxStaticBoxSizer( new wxStaticBox(
        this, wxID_ANY, wxT( "Toggle Settings" ) ), wxHORIZONTAL );

    wxBoxSizer* cameraRadioBoxSizer;
    cameraRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mCameraRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mCameraRadioBoxNChoices =
        sizeof( mCameraRadioBoxChoices ) / sizeof( wxString );
    mCameraRadioBox = new wxRadioBox( this, ID_CAMERA_RADIOBOX, wxT( "Camera" ),
        wxDefaultPosition, wxDefaultSize, mCameraRadioBoxNChoices,
        mCameraRadioBoxChoices, 1, wxRA_SPECIFY_ROWS | wxDOUBLE_BORDER );
    mCameraRadioBox->SetSelection( 1 );
    mCameraRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    cameraRadioBoxSizer->Add( mCameraRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );

    toggleSizer->Add(
        cameraRadioBoxSizer, 1, wxALIGN_CENTER | wxALL | wxEXPAND, 5 );

    wxBoxSizer* frustumRadioBoxSizer;
    frustumRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mFrustumRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mFrustumRadioBoxNChoices =
        sizeof( mFrustumRadioBoxChoices ) / sizeof( wxString );
    mFrustumRadioBox = new wxRadioBox( this, ID_FRUSTUM_RADIOBOX,
        wxT( "Frustum" ), wxDefaultPosition, wxDefaultSize,
        mFrustumRadioBoxNChoices, mFrustumRadioBoxChoices, 1,
        wxRA_SPECIFY_ROWS | wxDOUBLE_BORDER );
    mFrustumRadioBox->SetSelection( 1 );
    mFrustumRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    frustumRadioBoxSizer->Add( mFrustumRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );

    toggleSizer->Add(
        frustumRadioBoxSizer, 1, wxALIGN_CENTER | wxALL | wxEXPAND, 5 );

    wxBoxSizer* projectionRadioBoxSizer;
    projectionRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );

    wxString mProjectionRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
    int mProjectionRadioBoxNChoices =
        sizeof( mProjectionRadioBoxChoices ) / sizeof( wxString );
    mProjectionRadioBox = new wxRadioBox( this, ID_PROJECTION_RADIOBOX,
        wxT( "Projection" ), wxDefaultPosition, wxDefaultSize,
        mProjectionRadioBoxNChoices, mProjectionRadioBoxChoices, 1,
        wxRA_SPECIFY_ROWS | wxDOUBLE_BORDER );
    mProjectionRadioBox->SetSelection( 1 );
    mProjectionRadioBox->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    projectionRadioBoxSizer->Add(
        mProjectionRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );

    toggleSizer->Add(
        projectionRadioBoxSizer, 1, wxALIGN_CENTER | wxALL | wxEXPAND, 5 );

    mainSizer->Add( toggleSizer, 0, wxALL | wxEXPAND, 5 );

    wxBoxSizer* spacerSizer;
    spacerSizer = new wxBoxSizer( wxHORIZONTAL );


    spacerSizer->Add( 0, 0, 1, wxALL | wxEXPAND, 5 );

    mainSizer->Add( spacerSizer, 0, wxALL | wxEXPAND, 0 );

    wxStaticBoxSizer* projectionSettingsSizer;
    projectionSettingsSizer = new wxStaticBoxSizer( new wxStaticBox(
        this, wxID_ANY, wxT( "Projection Settings" ) ), wxVERTICAL );

    wxBoxSizer* fovzSizer;
    fovzSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* fovzTextSizer;
    fovzTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* fovzText;
    fovzText = new wxStaticText(
        this, wxID_ANY, wxT( "FoVZ:" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    fovzText->Wrap( -1 );
    fovzText->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    fovzTextSizer->Add( fovzText, 0, wxALL | wxEXPAND, 5 );

    fovzSizer->Add( fovzTextSizer, 0, wxEXPAND, 5 );

    wxBoxSizer* fovzSliderSizer;
    fovzSliderSizer = new wxBoxSizer( wxVERTICAL );

    mFoVZSlider = new wxSlider(
        this, ID_FOVZ_SLIDER, mProjectionData[ 0 ], 0, 180, wxDefaultPosition,
        wxDefaultSize, wxSL_HORIZONTAL | wxSL_LABELS );
    fovzSliderSizer->Add( mFoVZSlider, 0, wxALL | wxEXPAND, 5 );

    fovzSizer->Add( fovzSliderSizer, 1, wxEXPAND, 5 );

    projectionSettingsSizer->Add( fovzSizer, 1, wxEXPAND, 5 );

    wxBoxSizer* aspectRatioSizer;
    aspectRatioSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* aspectRatioTextSizer;
    aspectRatioTextSizer = new wxBoxSizer( wxVERTICAL );

    wxStaticText* aspectRatioText;
    aspectRatioText = new wxStaticText(
        this, wxID_ANY, wxT( "Aspect Ratio:" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
    aspectRatioText->Wrap( -1 );
    aspectRatioText->SetFont( wxFont(
        wxNORMAL_FONT->GetPointSize(), 70, 90, 90, false, wxEmptyString ) );

    aspectRatioTextSizer->Add( aspectRatioText, 0, wxALL | wxEXPAND, 5 );

    aspectRatioSizer->Add( aspectRatioTextSizer, 0, wxEXPAND, 5 );

    wxBoxSizer* aspectRatioSpinCtrlSizer;
    aspectRatioSpinCtrlSizer = new wxBoxSizer( wxVERTICAL );

    mAspectRatioSpinCtrl = new ves::conductor::util::wxSpinCtrlDbl(
        *this, ID_ASPECTRATIO_SPINCTRL, wxEmptyString, wxDefaultPosition,
        wxDefaultSize, wxSP_ARROW_KEYS, 0, 10, mProjectionData[ 1 ], 0.1 );
    aspectRatioSpinCtrlSizer->Add( mAspectRatioSpinCtrl, 0, wxALL, 5 );

    aspectRatioSizer->Add( aspectRatioSpinCtrlSizer, 1, wxEXPAND, 5 );

    projectionSettingsSizer->Add( aspectRatioSizer, 1, wxEXPAND, 5 );

    wxBoxSizer* nearFarPlaneSizer;
    nearFarPlaneSizer = new wxBoxSizer( wxVERTICAL );

    wxBoxSizer* nearFarPlaneDualSliderSizer;
    nearFarPlaneDualSliderSizer = new wxBoxSizer( wxVERTICAL );

    mNearFarPlaneDualSlider = new ves::conductor::util::DualSlider(
        this, wxID_ANY, 1, 0, 100, 0, 100, wxDefaultPosition,
        wxDefaultSize, wxSL_HORIZONTAL | wxSL_LABELS,
        wxString( wxT(  "Near / Far Plane" ) ) );
    cpt::CameraPlacementToolUIDialog::
        NearPlaneSliderCallback* nearPlaneSliderCallback =
            new cpt::CameraPlacementToolUIDialog::NearPlaneSliderCallback(
                this );
    cpt::CameraPlacementToolUIDialog::
        NearFarPlaneSliderCallback* nearFarPlaneSliderCallback =
            new cpt::CameraPlacementToolUIDialog::NearFarPlaneSliderCallback(
                this );
    cpt::CameraPlacementToolUIDialog::
        FarPlaneSliderCallback* farPlaneSliderCallback =
            new cpt::CameraPlacementToolUIDialog::FarPlaneSliderCallback(
                this );

    mNearFarPlaneDualSlider->SetMinSliderCallback(
        nearPlaneSliderCallback );
    mNearFarPlaneDualSlider->SetBothSliderUpdateCallback(
        nearFarPlaneSliderCallback );
    mNearFarPlaneDualSlider->SetMaxSliderCallback(
        farPlaneSliderCallback );

    nearFarPlaneDualSliderSizer->Add(
        mNearFarPlaneDualSlider, 0, wxALL | wxEXPAND, 5 );

    nearFarPlaneSizer->Add( nearFarPlaneDualSliderSizer, 1, wxEXPAND, 5 );

    projectionSettingsSizer->Add( nearFarPlaneSizer, 0, wxEXPAND, 5 );

    mainSizer->Add( projectionSettingsSizer, 0, wxEXPAND, 5 );

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
void CameraPlacementToolUIDialog::OnFoVZSlider(
    wxCommandEvent& WXUNUSED( event ) )
{
    mProjectionData[ 0 ] = mFoVZSlider->GetValue();
    ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnAspectRatioSpinCtrl(
    wxScrollEvent& WXUNUSED( event ) )
{
    mProjectionData[ 1 ] = mAspectRatioSpinCtrl->GetValue();
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
CameraPlacementToolUIDialog::NearPlaneSliderCallback::
    NearPlaneSliderCallback( CameraPlacementToolUIDialog* dialog )
{
    mDialog = dialog;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::NearFarPlaneSliderCallback::
    NearFarPlaneSliderCallback( CameraPlacementToolUIDialog* dialog )
{
    mDialog = dialog;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::FarPlaneSliderCallback::
    FarPlaneSliderCallback( CameraPlacementToolUIDialog* dialog )
{
    mDialog = dialog;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::
    NearPlaneSliderCallback::~NearPlaneSliderCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::
    NearFarPlaneSliderCallback::~NearFarPlaneSliderCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
CameraPlacementToolUIDialog::
    FarPlaneSliderCallback::~FarPlaneSliderCallback()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::NearPlaneSliderCallback::SliderOperation()
{
    mDialog->mProjectionData[ 2 ] = _dualSlider->GetMinSliderValue();

    mDialog->ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::NearFarPlaneSliderCallback::SliderOperation()
{
    mDialog->mProjectionData[ 2 ] = _dualSlider->GetMinSliderValue();
    mDialog->mProjectionData[ 3 ] = _dualSlider->GetMaxSliderValue();

    mDialog->ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::FarPlaneSliderCallback::SliderOperation()
{
    mDialog->mProjectionData[ 3 ] = _dualSlider->GetMaxSliderValue();

    mDialog->ProjectionUpdate();
}
////////////////////////////////////////////////////////////////////////////////
