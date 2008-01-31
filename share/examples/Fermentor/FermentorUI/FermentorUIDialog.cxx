// --- My Includes --- //
#include "FermentorUIDialog.h"

// --- wxWidgets Includes --- //
#include <wx/sizer.h>
#include <wx/radiobox.h>
#include <wx/slider.h>
#include <wx/checkbox.h>
#include <wx/notebook.h>
#include <wx/button.h>
#include <wx/statbox.h>
#include <wx/textctrl.h>
#include <wx/stattext.h>
#include <wx/statbmp.h>
#include <wx/panel.h>
#include <wx/dialog.h>

BEGIN_EVENT_TABLE( FermentorUIDialog, wxDialog )
EVT_RADIOBOX( CYCLE_ID_RADIOBOX, FermentorUIDialog::_onCycle )
EVT_RADIOBOX( ROTATION_ID_RADIOBOX, FermentorUIDialog::_onRotation )
EVT_RADIOBOX( XRAY_ID_RADIOBOX, FermentorUIDialog::_onXRay )
EVT_RADIOBOX( LOOP_ID_RADIOBOX, FermentorUIDialog::_onLoop )
EVT_SLIDER( ROTATION_SLIDER, FermentorUIDialog::_onSlider )
EVT_SLIDER( SIMULATION_SLIDER,FermentorUIDialog::_onSimSlider )
EVT_BUTTON( UPDATE_BUTTON, FermentorUIDialog::_onUpdate )
EVT_BUTTON( INITIATE_BUTTON, FermentorUIDialog::_onInitiate )
END_EVENT_TABLE()

////////////////////////////////////////////////////////////////////////////////
FermentorUIDialog::FermentorUIDialog( wxWindow* parent, int id, double* agitation,
                                                                double* air_conc,
                                                                double* ini_ph,
                                                                double* nitrate_conc,
                                                                double* temperature,
                                                                double* hours,
                                                                long* cycle_ID,
                                                                long* rotation_ID,
                                                                long* xray_ID,
                                                                long* loop_ID,
                                                                double* rot_speed,
                                                                double* sim_speed )

:
    UIDialog( parent, id, _("Fermentor") ), 
    p_agitation( agitation ),
    p_air_conc( air_conc ),
    p_ini_ph( ini_ph ),
    p_nitrate_conc( nitrate_conc ),
    p_temperature( temperature ),
    p_hours( hours ),
    p_cycle_ID( cycle_ID ),
    p_rotation_ID( rotation_ID ),
    p_xray_ID( xray_ID ),
    p_loop_ID( loop_ID ),
    p_rot_speed( rot_speed ),
    p_sim_speed( sim_speed )
{
    *p_agitation = 0;
    *p_air_conc = 0;
    *p_ini_ph = 0;
    *p_nitrate_conc = 0;
    *p_temperature = 0;
    *p_hours = 0;
    *p_cycle_ID = 0;
    *p_rotation_ID = 0;
    *p_xray_ID = 0;
    *p_loop_ID = 0;
    *p_rot_speed = 0;
    *p_sim_speed = 0;

    _buildPage();
}
////////////////////////////////////////////////////////////////////////////////
FermentorUIDialog::~FermentorUIDialog()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_buildPage()
{
    wxStaticBox* AGI_Group = 0;
    wxStaticBox* AIR_Group = 0;
    wxStaticBox* PH_Group = 0;
    wxStaticBox* NIT_Group = 0;
    wxStaticBox* TEMP_Group = 0;
    wxStaticBox* HRS_Group = 0;

    wxBoxSizer* FermentorModelGroup = new wxBoxSizer( wxVERTICAL );

    _agitation = new wxTextCtrl( this,-1, _("200"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );
    _air = new wxTextCtrl( this, -1, _("1.25"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );
    _ph = new wxTextCtrl( this, -1, _("6"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );
    _nitrate = new wxTextCtrl( this, -1, _("0.1"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );
    _temperature = new wxTextCtrl( this, -1, _("37"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );
    _hours = new wxTextCtrl( this, -1, _("240"), wxDefaultPosition, wxDefaultSize, wxTE_PROCESS_ENTER, wxDefaultValidator );

    AGI_Group = new wxStaticBox( this, -1, _("Agitation(rpm)") );
    AIR_Group = new wxStaticBox( this, -1, _("Air Concentation(vvm)") );
    PH_Group = new wxStaticBox( this, -1, _("Initial pH") );
    NIT_Group = new wxStaticBox( this, -1, _("Nitrate Concentration(g/L)") );
    TEMP_Group = new wxStaticBox( this, -1, _("Temperature(C)") );
    HRS_Group = new wxStaticBox( this, -1, _("Simulation Time(Hours)") );

    wxStaticBoxSizer* LOC_AGI_Group = new wxStaticBoxSizer( AGI_Group, wxVERTICAL );
    LOC_AGI_Group->Add( _agitation, 6, wxALIGN_CENTRE );
    wxStaticBoxSizer* LOC_AIR_Group = new wxStaticBoxSizer( AIR_Group, wxVERTICAL );
    LOC_AIR_Group->Add( _air, 6, wxALIGN_CENTRE );
    wxStaticBoxSizer* LOC_PH_Group = new wxStaticBoxSizer( PH_Group, wxVERTICAL );
    LOC_PH_Group->Add( _ph, 6, wxALIGN_CENTRE );
    wxStaticBoxSizer* LOC_NIT_Group = new wxStaticBoxSizer( NIT_Group, wxVERTICAL );
    LOC_NIT_Group->Add( _nitrate, 6, wxALIGN_CENTRE );
    wxStaticBoxSizer* LOC_TEMP_Group = new wxStaticBoxSizer( TEMP_Group, wxVERTICAL );
    LOC_TEMP_Group->Add( _temperature, 6, wxALIGN_CENTRE );
    wxStaticBoxSizer* LOC_HRS_Group = new wxStaticBoxSizer( HRS_Group, wxVERTICAL );
    LOC_HRS_Group->Add( _hours, 6, wxALIGN_CENTRE );

    _agitation->Raise();
    _air->Raise();
    _ph->Raise();
    _nitrate->Raise();
    _temperature->Raise();
    _hours->Raise();
    
    wxString cycles[] = { _("Stop"), _("Start") };
    wxString rotmode[] = { _("Off"), _("On") };
    wxString xraymode[] = { _("Off"), _("On") };
    wxString loopmode[] = { _("Single"), _("Cycle") };

    _CycleRadioBox = new wxRadioBox( this, CYCLE_ID_RADIOBOX, _("Simulation Options"), wxDefaultPosition, wxDefaultSize, 2, cycles, 1, wxRA_SPECIFY_COLS );
    _RotationRadioBox = new wxRadioBox( this, ROTATION_ID_RADIOBOX, _("Auto-Rotate"), wxDefaultPosition, wxDefaultSize, 2, rotmode, 1, wxRA_SPECIFY_COLS );
    _XRayRadioBox = new wxRadioBox( this, XRAY_ID_RADIOBOX, _("X-Ray"), wxDefaultPosition, wxDefaultSize, 2, xraymode, 1, wxRA_SPECIFY_COLS );
    _LoopRadioBox = new wxRadioBox( this, LOOP_ID_RADIOBOX, _("Mode"), wxDefaultPosition, wxDefaultSize, 2, loopmode, 1, wxRA_SPECIFY_COLS );

    _RotationSlider = new wxSlider( this, ROTATION_SLIDER, 0, 0, 5, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL );
    _SimulationSlider = new wxSlider( this, SIMULATION_SLIDER, 0, 0, 10, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL );

    _Update = new wxButton( this, wxID_OK, _("UPDATE PARAMETERS AND RUN SIMULATION") );

    wxBoxSizer* Row1 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row2 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row3 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row4 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row5 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row6 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row7 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row8 = new wxBoxSizer( wxHORIZONTAL );
    wxBoxSizer* Row9 = new wxBoxSizer( wxHORIZONTAL );

    Row1->Add( LOC_AGI_Group, 1, wxALIGN_LEFT ); 
    Row1->Add( LOC_AIR_Group, 1, wxALIGN_LEFT ); 
    Row2->Add( LOC_PH_Group, 1, wxALIGN_LEFT ); 
    Row2->Add( LOC_NIT_Group, 1, wxALIGN_LEFT ); 
    Row3->Add( LOC_TEMP_Group, 1, wxALIGN_LEFT ); 
    Row3->Add( LOC_HRS_Group, 1, wxALIGN_LEFT ); 
    Row4->Add( _CycleRadioBox, 1, wxALIGN_CENTER );
    Row4->Add( _LoopRadioBox, 1, wxALIGN_CENTER );
    Row5->Add( _SimulationSlider, 1, wxALIGN_CENTER );
    Row6->Add( _XRayRadioBox, 1, wxALIGN_CENTER );
    Row7->Add( _RotationRadioBox, 1, wxALIGN_CENTER );
    Row8->Add( _RotationSlider, 1, wxALIGN_CENTER );
    Row9->Add( _Update, 1, wxALIGN_CENTER_VERTICAL );

    FermentorModelGroup->Add( Row1, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row2, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row3, 1, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row4, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row5, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row6, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row7, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row8, 0, wxALIGN_LEFT | wxEXPAND );
    FermentorModelGroup->Add( Row9, 1, wxALIGN_LEFT | wxEXPAND );

    SetAutoLayout( true );

    SetSizer( FermentorModelGroup );
    FermentorModelGroup->Fit( this );
}
////////////////////////////////////////////////////////////////////////////////
bool FermentorUIDialog::TransferDataFromWindow()
{
    long selection;
    selection = _CycleRadioBox->GetSelection();
    *p_cycle_ID = selection;

    long selection2;
    selection2 = _RotationRadioBox->GetSelection();
    *p_rotation_ID = selection2;

    long selection3;
    selection3 = _XRayRadioBox->GetSelection();
    *p_xray_ID = selection3;

    long selection4;
    selection4 = _LoopRadioBox->GetSelection();
    *p_loop_ID = selection4;

    *p_rot_speed = _RotationSlider->GetValue();

    *p_sim_speed = _SimulationSlider->GetValue();

    wxString txt;
    txt = _agitation->GetValue();
    txt.ToDouble( &(*p_agitation) );

    txt=_air->GetValue();
    txt.ToDouble( &(*p_air_conc) );

    txt=_ph->GetValue();
    txt.ToDouble( &(*p_ini_ph) );

    txt=_nitrate->GetValue();
    txt.ToDouble( &(*p_nitrate_conc) );

    txt=_temperature->GetValue();
    txt.ToDouble( &(*p_temperature) );

    txt=_hours->GetValue();
    txt.ToDouble( &(*p_hours) );

    return true;
}
////////////////////////////////////////////////////////////////////////////////
bool FermentorUIDialog::TransferDataToWindow()
{
    return true;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::Lock( bool l )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onCycle( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onRotation( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onLoop( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onXRay( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onSlider( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onSimSlider( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onUpdate( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void FermentorUIDialog::_onInitiate( wxCommandEvent& event )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
