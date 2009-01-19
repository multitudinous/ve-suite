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
