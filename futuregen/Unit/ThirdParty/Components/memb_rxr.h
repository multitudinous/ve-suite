/********************************************************************************************
REI MARCH-APRIL 2003 DOL
VISION 21 MEMBRANE SHIFT REACTOR
WATER GAS SHIFT MEMBRANE REACTOR TO CONVERT A HIGH CO STREAM TO A H2 AND CO2 VIA THE REACTION
CO + H2O = CO2 + H2, PRIOR TO A FUEL CELL OR A GAS TURBINE, ETC.  
MODEL IS 1-DIMENSIONAL IN SPACE WITH COCURRENT FLOW OF GAS ON THE FEED SIDE AND SWEEP (H2 PRODUCT)
ON THE PERMEATE SIDE.  MEMBRANE IS PALLADIUM WITH A SQRT(P_H2) DRIVING FORCE.  ADIABATIC WGS
EQUILIBRIUM IS ASSUMED.  A UNIFORM FRACTIONAL HEAT LOSS IS ALLOWED FROM THE FEED SIDE TO THE 
ENVIRONMENT.  FRACTIONAL HEAT LOSS IS FRACTION OF FEED ENTHALPY BETWEEN MR INLET TEMP AND 298 K.
NO HEAT TRANSFER ACROSS THE MEMBRANE.  FEED SIDE IS ASSUMED IN THE ANNULAR REGION,
PERMEATE FLOWS IN THE CENTRAL CORE.  FEED SIDE IS PACKED WITH CATALYST. 
PROGRAM ALLOWS AN ADIABATIC PRE-MR EQUILIBRIUM STAGE FOLLOWED BY HEAT TRANSFER TO A DESIRED MR 
INLET TEMP.

USAGE:
	INPUTS
		Pd_thickness, fheat_loss, case_type, memb_diameter, shell_diameter, f_pre_mr
		IF f_H2O_CO: specify H2O_CO ratio in inlet
		Inlet gas (t, p, composition, flow rate)
		Sweep gas inlet(t, p, composition)
		IF case_type is 0, specify Sweep gas flow rate, L_rxr.
		IF case_type is 1, specify desired CO conversion.
		IF f_pre_mr is true specify mr_inlet_temp
		
	OUTPUTS
		INPUTS SUMMARY plus
		space_velocity, CO_conv, H2_recovery, heat_loss, footprint
		gasout(t, p, composition, flow), sweepout(t, p, comp, flow)
		IF case 1: L_rxr
		IF f_pre_mr: pre_mr_heatex, pre_mr_conv, 
		IF f_H2O_CO:  steam_injection_flow;
		
**********************************************************************************************/

#ifndef MEMB_RXR_H
#define MEMB_RXR_H

#include "rxrgas.h"
#include <Packages/REI/Core/ThirdParty/Therm/thermo.h>
#include <utility>

using namespace std;
using namespace REI;

class memb_rxr {

private:
	double psi;                                // extent of reaction mol/s for CO + H2O = H2 + CO2 (units of moles/s)
	double H;                                  // enthalpy variable, adjust T to achieve (adiabatic reactor)
	double ndp;                                // moles hydrogen permeate (cumulative)
	double PH2_perm;                           // permeate pressure (Pa)
	bool   f_error;

public:
	double CO_conv;                            // output overall CO conversion
	double Pd_thickness;                       // Palladium membrane (m)
	double L_rxr;                              // reactor length
	double fheat_loss;                         // fractional heat loss (T->298) inlet gas
	int    case_type;                          // 0 for spec Length, PH2 perm, 1 for spec CO conv
	double CO_conv_want;                       // input for case 1
	double H2_recovery;                        // ouput fraction of possible H2 recovered
	double memb_diameter;                
	int    n_modules;                          // number of parallel membrane modules
	double shell_diameter;                     
	double space_velocity;                     // ouput mr catalyst space velocity  (superficial) 1/h
	bool   f_pre_mr;                           // true to do an adiabatic pre-mr step with Heat Trnsfr to mr_inlet_temp
	double pre_mr_heatex;                      // output pre-mr heat transfer to mr_inlet_temp
	double mr_inlet_temp;                      // for f_pre_mr true
	double pre_mr_conv;                        // ouput pre-mr step conversion
	double footprint;                          // m2
	double heat_loss;                          // working is J/s, output is kW
	double H2O_CO;                             // input steam to CO inlet ratio
	bool   f_H2O_CO;                           // flag to specify steam to CO ratio in inlet
	double steam_injection_flow;               // kg/s of steam injected to match H2O_CO for f_H2O_CO

private:
	double Get_H2_flux(double temp, double pres);           // moles/m2/s
	double fK(rxrgas &gasin, double x_psi, double x_T);     // equilib solver 2 nonlinear equations, fK, fH
	double fH(rxrgas &gasin, double x_psi, double x_T);
	bool adiabatic_equilibrium_solver(rxrgas &gasin, rxrgas &gasw, bool f_reaction, bool f_isoT);

public:
	memb_rxr();
	pair<vector<int>, vector<string> > calculate(Gas& v21gasin, Gas& v21gasw, Gas& sweepin, Gas& sweepw);

};

#endif
