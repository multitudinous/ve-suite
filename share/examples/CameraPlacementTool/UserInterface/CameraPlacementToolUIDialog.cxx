// --- VE-Suite Includes --- //
#include <ves/conductor/util/CORBAServiceList.h>

// --- My Includes --- //
#include "CameraPlacementToolUIDialog.h"

// --- VE-Suite Includes --- //
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/Command.h>

// --- wxWidgets Includes --- //
#include <wx/statline.h>
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/stattext.h>
#include <wx/slider.h>
#include <wx/spinctrl.h>
#include <wx/dialog.h>

using namespace cpt;

/*----------------------------------------------------------------------------*/
BEGIN_EVENT_TABLE( CameraPlacementToolUIDialog, wxDialog )

EVT_RADIOBOX( ID_CAMERA_RADIOBOX,
              CameraPlacementToolUIDialog::OnCameraRadioBox )
EVT_RADIOBOX( ID_FRUSTUM_RADIOBOX,
              CameraPlacementToolUIDialog::OnFrustumRadioBox )
EVT_RADIOBOX( ID_PROJECTION_RADIOBOX,
              CameraPlacementToolUIDialog::OnProjectionRadioBox )

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
	SetFont( wxFont( 12, 70, 90, 90, false, wxEmptyString ) );
	
	wxBoxSizer* mainSizer;
	mainSizer = new wxBoxSizer( wxVERTICAL );
	
	wxBoxSizer* topStaticLineSizer;
	topStaticLineSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticLine* topStaticLine;
	topStaticLine = new wxStaticLine(
        this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
	topStaticLineSizer->Add( topStaticLine, 0, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( topStaticLineSizer, 1, wxEXPAND, 5 );
	
	wxBoxSizer* toggleSizer;
	toggleSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxBoxSizer* cameraRadioBoxSizer;
	cameraRadioBoxSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxString mCameraRadioBoxChoices[] = { wxT( "Off" ), wxT( "On" ) };
	int mCameraRadioBoxNChoices =
        sizeof( mCameraRadioBoxChoices ) / sizeof( wxString );
	mCameraRadioBox = new wxRadioBox( this, ID_CAMERA_RADIOBOX,
        wxT( "Camera" ), wxDefaultPosition, wxDefaultSize,
        mCameraRadioBoxNChoices, mCameraRadioBoxChoices, 1,
        wxRA_SPECIFY_ROWS | wxDOUBLE_BORDER );
	mCameraRadioBox->SetSelection( 1 );
	mCameraRadioBox->SetFont( wxFont( 10, 70, 90, 90, false, wxEmptyString ) );
	
	cameraRadioBoxSizer->Add( mCameraRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );
	
	toggleSizer->Add( cameraRadioBoxSizer, 1, wxALIGN_CENTER | wxEXPAND, 5 );
	
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
	mFrustumRadioBox->SetFont( wxFont( 10, 70, 90, 90, false, wxEmptyString ) );
	
	frustumRadioBoxSizer->Add( mFrustumRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );
	
	toggleSizer->Add( frustumRadioBoxSizer, 1, wxALIGN_CENTER | wxEXPAND, 5 );
	
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
	mProjectionRadioBox->SetFont(
        wxFont( 10, 70, 90, 90, false, wxEmptyString ) );
	
	projectionRadioBoxSizer->Add(
        mProjectionRadioBox, 0, wxALIGN_CENTER | wxALL, 5 );
	
	toggleSizer->Add(
        projectionRadioBoxSizer, 1, wxALIGN_CENTER | wxEXPAND, 5 );
	
	mainSizer->Add( toggleSizer, 5, wxALIGN_CENTER | wxEXPAND, 5 );
	
	wxBoxSizer* bottomStaticLineSizer;
	bottomStaticLineSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticLine* bottomStaticLine;
	bottomStaticLine = new wxStaticLine(
        this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxLI_HORIZONTAL );
	bottomStaticLineSizer->Add( bottomStaticLine, 0, wxALL | wxEXPAND, 5 );
	
	mainSizer->Add( bottomStaticLineSizer, 1, wxALIGN_CENTER | wxEXPAND, 5 );
	
	wxBoxSizer* projectionSettingsSizer;
	projectionSettingsSizer = new wxBoxSizer( wxVERTICAL );
	
	wxBoxSizer* projectionSettingsTextSizer;
	projectionSettingsTextSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticText* projectionSettingsText;
	projectionSettingsText = new wxStaticText(
        this, wxID_ANY, wxT( "Projection Settings" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_LEFT );
	projectionSettingsText->Wrap( -1 );
	projectionSettingsText->SetFont(
        wxFont( 11, 70, 90, 90, true, wxEmptyString ) );
	
	projectionSettingsTextSizer->Add(
        projectionSettingsText, 0, wxALL | wxEXPAND, 5 );
	
	projectionSettingsSizer->Add( projectionSettingsTextSizer, 1, wxEXPAND, 5 );
	
	wxBoxSizer* fovzSizer;
	fovzSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxBoxSizer* fovzTextSizer;
	fovzTextSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticText* fovzText;
	fovzText = new wxStaticText( this, wxID_ANY, wxT( "FoVZ:" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	fovzText->Wrap( -1 );
	fovzText->SetFont( wxFont( 10, 70, 90, 90, false, wxEmptyString ) );
	
	fovzTextSizer->Add( fovzText, 0, wxALL | wxEXPAND, 5 );
	
	fovzSizer->Add( fovzTextSizer, 1, wxEXPAND, 5 );
	
	wxBoxSizer* fovzSliderSizer;
	fovzSliderSizer = new wxBoxSizer( wxVERTICAL );
	
	mFoVZSlider = new wxSlider( this, wxID_ANY, 50, 0, 180,
        wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL | wxSL_LABELS );
	fovzSliderSizer->Add( mFoVZSlider, 0, wxALL | wxEXPAND, 5 );
	
	fovzSizer->Add( fovzSliderSizer, 1, wxEXPAND, 5 );
	
	projectionSettingsSizer->Add( fovzSizer, 1, wxEXPAND, 5 );
	
	wxBoxSizer* aspectRatioSizer;
	aspectRatioSizer = new wxBoxSizer( wxHORIZONTAL );
	
	wxBoxSizer* aspectRatioTextSizer;
	aspectRatioTextSizer = new wxBoxSizer( wxVERTICAL );
	
	wxStaticText* aspectRatioText;
	aspectRatioText = new wxStaticText( this, wxID_ANY, wxT( "Aspect Ratio:" ),
        wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT );
	aspectRatioText->Wrap( -1 );
	aspectRatioText->SetFont( wxFont( 10, 70, 90, 90, false, wxEmptyString ) );
	
	aspectRatioTextSizer->Add( aspectRatioText, 0, wxALL | wxEXPAND, 5 );
	
	aspectRatioSizer->Add( aspectRatioTextSizer, 1, wxEXPAND, 5 );
	
	wxBoxSizer* aspectRatioSpinCtrlSizer;
	aspectRatioSpinCtrlSizer = new wxBoxSizer( wxVERTICAL );
	
	mAspectRatioSpinCtrl = new wxSpinCtrl( this, wxID_ANY, wxEmptyString,
        wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, 0, 2, 1 );
	aspectRatioSpinCtrlSizer->Add( mAspectRatioSpinCtrl, 0, wxALL, 5 );
	
	aspectRatioSizer->Add( aspectRatioSpinCtrlSizer, 1, wxEXPAND, 5 );
	
	projectionSettingsSizer->Add( aspectRatioSizer, 1, wxEXPAND, 5 );
	
	mainSizer->Add( projectionSettingsSizer, 25, wxEXPAND, 5 );
	
	SetSizer( mainSizer );
	Layout();
	mainSizer->Fit( this );
    CenterOnParent();
}
////////////////////////////////////////////////////////////////////////////////
void CameraPlacementToolUIDialog::OnCameraRadioBox( wxCommandEvent& event )
{
    unsigned int selection = mCameraRadioBox->GetSelection();

    //Build the command
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

    //Build the command
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

    //Build the command
    mCommandName = "TOGGLE_PROJECTION_UPDATE";

    ves::open::xml::DataValuePairSharedPtr toggleProjectionDVP(
        new ves::open::xml::DataValuePair() );
    toggleProjectionDVP->SetData( "toggleProjection", selection );
    mInstructions.push_back( toggleProjectionDVP );

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
