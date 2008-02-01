#ifndef FERMENTOR_UI_DIALOG_H
#define FERMENTOR_UI_DIALOG_H

// --- VE-Suite Includes --- //
#include <ves/conductor/UIDialog.h>

// --- wxWidgets Includes --- //
class wxTextCtrl;
class wxRadioBox;
class wxSlider;
class wxButton;

// --- C/C++ Libraries --- //
#include <vector>
#include <string>

enum FERMENTOR_TAB_IDS
{
    CYCLE_ID_RADIOBOX,
    ROTATION_ID_RADIOBOX,
    XRAY_ID_RADIOBOX,
    LOOP_ID_RADIOBOX,
    ROTATION_SLIDER,
    SIMULATION_SLIDER,
    UPDATE_BUTTON,
    INITIATE_BUTTON
};

class FermentorUIDialog : public ves::conductor::UIDialog
{
public:
    FermentorUIDialog( wxWindow* parent, int id, double* agitation,
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
                                                 double* sim_speed );

    FermentorUIDialog(){;}

    virtual ~FermentorUIDialog();

    virtual bool TransferDataFromWindow();
    virtual bool TransferDataToWindow();
    virtual void Lock( bool l );

    double* p_agitation;
    double* p_air_conc;
    double* p_ini_ph;
    double* p_nitrate_conc;
    double* p_temperature;
    double* p_hours;
    long* p_cycle_ID;
    long* p_rotation_ID;
    long* p_xray_ID;
    long* p_loop_ID;
    double* p_rot_speed;
    double* p_sim_speed;

protected:
    //UI widgets variables
    wxTextCtrl* _agitation;
    wxTextCtrl* _air;
    wxTextCtrl* _ph;
    wxTextCtrl* _nitrate;
    wxTextCtrl* _temperature;
    wxTextCtrl* _hours;

    wxRadioBox* _CycleRadioBox;
    wxRadioBox* _RotationRadioBox;
    wxRadioBox* _XRayRadioBox;
    wxRadioBox* _LoopRadioBox;
    wxSlider* _RotationSlider;
    wxSlider* _SimulationSlider;
    wxButton* _Update;
    wxButton* _Initiate;

    //GUI Variables
    void _buildPage();
    void _onCycle( wxCommandEvent& event );
    void _onRotation( wxCommandEvent& event );
    void _onXRay( wxCommandEvent& event );
    void _onLoop( wxCommandEvent& event );
    void _onSlider( wxCommandEvent& event );
    void _onSimSlider( wxCommandEvent& event );
    void _onUpdate( wxCommandEvent& event );   
    void _onInitiate( wxCommandEvent& event );

    DECLARE_EVENT_TABLE() 
};

#endif //FERMENTOR_UI_DIALOG_H
