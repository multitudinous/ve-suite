#include <iostream>
#include <cmath>
#include <cstdlib>
#include "memb_rxr.h"

using namespace std;
using namespace REI;

//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

memb_rxr::memb_rxr() 
{
	H    = 0.0;
	ndp  = 0.0;
	psi  = 1.0E-5 + 1.0E-19;
	PH2_perm = 0.0;
	f_error = false;

	CO_conv              = 0.0;
	Pd_thickness         = 10.0E-6;           // meters         
	L_rxr                = 2.0;               // m
	fheat_loss           = 0.0;               // % of inlet sensible heat relative to 298 lost to envrnmt
	case_type            = 0;
	CO_conv_want         = 0.9;
	H2_recovery          = 0.0;
	memb_diameter        = 0.1;               // m
	n_modules            = 1800;
	shell_diameter       = 0.15;              // m
	space_velocity       = 0.0;
	f_pre_mr             = false;
	pre_mr_heatex        = 0.0;
	mr_inlet_temp        = 600.0;
	pre_mr_conv          = 0.0;
	footprint            = 0.0;               // m2
	heat_loss            = 0.0;
	H2O_CO               = 1.0;
	f_H2O_CO             = false;
}

//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

//bool memb_rxr::calculate(rxrgas &gasin, rxrgas &gasw, rxrgas &sweepin, rxrgas &sweepw) {

pair<vector<int>,vector<string> > memb_rxr::calculate(Gas &v21gasin, Gas &v21gasw, 
													  Gas &v21sweepin, Gas &v21sweepw) {
	// GASIN IS UNIT INLET GAS
	// GASW IS WORKING GAS (INTEGRATED) AND IS OUTLET GAS
	// SWEEPIN AND SWEEPW LIKEWISE FOR THE SWEEPGAS
	// THESE INLETS ARE VISION 21 GAS CLASS PASSED IN, BUT CONVERTED TO RXR GAS FOR OPERATION
    // FOR PREMATURE EXIT, VISION 21 GASES ARE NOT CHANGED, OTHER THAN STEAM INJECTION

	// CONVERT VISION 21 GAS TO MEMB_RXR GAS

	pair<vector<int>, vector<string> > exit_condition;   // IS RETURNED: GOOD IF EMPTY, WARN INT 1, ERR INT -1

	rxrgas gasin(v21gasin);
	rxrgas gasw(v21gasin);
	rxrgas sweepin(v21sweepin);
	rxrgas sweepw(v21sweepin);
	rxrgas mrgasin(v21gasin);

	cout << "\n\n******* WATER GAS SHIFT MEMBRANE REACTOR ******** \n\n";

	// ADJUST STEAM IN GAS INLET BASED ON SPECIFIED H2O:CO RATIO, IF OPTION IS SET
	
	if(f_H2O_CO) {
		steam_injection_flow = (H2O_CO*gasin.moles("CO") - gasin.moles("H2O")) * 0.0180152;  // kg/s
		gasin.moles(H2O_CO*gasin.moles("CO") - gasin.moles("H2O"), "H2O");    // ADD H2O TO MATCH H2O:CO RATIO SPECIFIED
  		gasw.copy(gasin);
		if(steam_injection_flow < 0.0) {
			exit_condition.first.push_back(1);
			exit_condition.second.push_back("H2O:CO ratio specified is less than inlet gas");
		}
		cout << "\nSteam Required (kg/s) for H2O:CO ratio of " << H2O_CO << " is " << steam_injection_flow;
	}

	// TEST FOR POSSIBLE CONVERSION FOR CASE_TYPE 1
	
	if(case_type == 1)
		if(CO_conv_want > gasin.moles("H2O")/gasin.moles("CO")) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Specified Conversion must be < H2O/CO in feed");
			return exit_condition;
		}

	// TEST FOR POSITIVE CO IN GAS (NOT ZERO)

	if(gasin.moles("CO") <= 0.0) {
		exit_condition.first.push_back(1);
		exit_condition.second.push_back(string("Zero incoming CO. ") +
			string("Added 1E-6 moles/s CO. CO conversion based on this value"));
		gasin.moles(1.0E-6, "CO");
	}

	// TEST FOR WHETHER OR NOT REACTION OCCURS

	bool f_reaction = true;                                     // INTERNAL FLAG TO COMPUTE EQUILIBRIUM OR NOT
	double tiny     = 1.0E-10;
	if( (gasin.comp[gasin.names["CO"]]  > tiny && gasin.comp[gasin.names["H2O"]] > tiny  ) ||
		(gasin.comp[gasin.names["CO2"]] > tiny && gasin.comp[gasin.names["H2"]]  > tiny) ) 
	   f_reaction = true;
	else {
	   f_reaction = false;
	   exit_condition.first.push_back(1); 
	   exit_condition.second.push_back("NO REACTION OCCUR, H2 may still permeate");
	}
	if(!f_reaction && gasin.comp[gasin.names["H2"]] == 0.0) {
		exit_condition.first.push_back(-1);
		exit_condition.second.push_back(string("No H2 present and no reaction. ") +
			string("Check inlet gas composition for H2, H2O, CO, CO2"));
		cout << "\nNo H2 present, and no reaction.  "
			 << "Exit conditions = inlet conditions, no H2 product";
		return exit_condition;
	}

	if(!f_reaction && case_type == 1) {
		exit_condition.first.push_back(-1);
		exit_condition.second.push_back(string("No WGS reactions for inlet gas ") +
                                        string("specified: Cannot specify CO conversion"));
		return exit_condition;
	}

	// TEST FOR MEMBRANE DIAMETER > SHELL DIAMETER

	if(memb_diameter >= shell_diameter) {
		exit_condition.first.push_back(1);
		exit_condition.second.push_back("Membrane Diameter > Shell Diameter; Catalyst assumed on shell (annulus) side. \n\tComputed Space Velocity is wrong.");
	}

	// PRE MEMBRANE REACTOR SHIFT (CATALYTIC, NO MEMBRANE)

	if(f_pre_mr) {
		cout << "\n\nPre-shift reactor option used\n";
		H = gasw.thermo_data.enthalpy_mix(gasw.comp, gasw.T) * gasw.Moles / 1000;  
		f_error = adiabatic_equilibrium_solver(gasin, gasw, f_reaction, false);
		if(f_error) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Equilibrium Solver did Not Converge");
			return exit_condition;
		}
		cout << "\nPre-Shift Reactor Exit Temperature (K): " << gasw.T;
		pre_mr_heatex = ( gasw.thermo_data.enthalpy_mix(gasw.comp, gasw.T) - 
			gasw.thermo_data.enthalpy_mix(gasw.comp, mr_inlet_temp) ) * 
			gasw.Moles / 1000.0 / 1000.0;                        // kW
		gasw.T = mr_inlet_temp;

		if(f_reaction && gasin.moles("CO") > 0.0)
			pre_mr_conv = ( gasin.moles("CO") - gasw.moles("CO") ) / gasin.moles("CO"); 
		cout << "\nPre-Shift CO conversion:      " << pre_mr_conv;

		if(case_type == 1 && pre_mr_conv >= CO_conv_want) { 
			cout << "\nNO MR NEEDED FOR DESIRED CONVERSION";
			exit_condition.first.push_back(1);
			exit_condition.second.push_back("NO MR NEEDED FOR DESIRED CONVERSION");
			gasw.rxrgas_2_v21gas(v21gasw);
			return exit_condition;
		}
		cout << "\nPre-Shift heat transfer (kW): " << pre_mr_heatex;
		mrgasin.copy(gasw);   // FOR CASE 1 NEED MR INLET GAS FOR PASS 2
	}
	
	// M.R. CALCULATIONS

	cout << "\n\nMEMBRANE REACTOR INFORMATION\n";
	cout << "\nNumber of Membrane Modules: " << n_modules;
	footprint = shell_diameter * shell_diameter * n_modules;
	cout << "\nFootprint (m2):               " << footprint;

	const int ndx   = 2000;                                      // MOLES/S H2 PERMEATE
	double dx = L_rxr / ndx;                               
	double dndp = 0.0, dndp2 = 0.0;                              // DELTA H2 PERMEATE, PREDICTOR, CORRECTOR
	double dH   = 0.0, dH2 = 0.0;                                // DELTA ENTHALPY PREDICTOR, CORRECTOR
	double memb_area_per_length = 3.1415926*memb_diameter*n_modules;
	double L_print = 0.05;                                       // OUTPUT AT INTERVALS 

	map<string,int>::iterator i_H2;
	i_H2 = gasw.names.find("H2");
	CO_conv = 0.0;

	// COMPUTE ENTHALPY LOSS TERM

	double dheat_loss = ( gasw.thermo_data.enthalpy_mix(gasw.comp, gasw.T) - 
					   gasw.thermo_data.enthalpy_mix(gasw.comp, 298.15) ) * 
					   gasw.Moles / 1000 * fheat_loss / ndx;
	H = gasw.thermo_data.enthalpy_mix(gasw.comp, gasw.T) * gasw.Moles / 1000;  
	heat_loss = 0.0;                                            // CUMULATIVE HEAT LOSS

	double Ho = H;                                              // INITIAL FEED GAS ENTHALPY (MR)
	cout << "\nInitial H " << H;
	cout << "\nM.R. INLET GAS: yCO H2O CO2 H2 N2 Ntot T P\n"
		 << gasw.comp[gasw.names.find("CO")->second] << endl
		 << gasw.comp[gasw.names.find("H2O")->second] << endl
		 << gasw.comp[gasw.names.find("CO2")->second] << endl
		 << gasw.comp[gasw.names.find("H2")->second] << endl
		 << gasw.comp[gasw.names.find("N2")->second] << endl
		 << gasw.Moles << endl
		 << gasw.T << endl
	     << gasw.P;
    	
	double x = 0.0;     
	double dummy = 0.0;
	int pass_1or2 = 0;
	//bool f_step_size_error = false;

	cout << "\n\nIntegration Along Membrane";
	cout << "\n\nPosition, Moles_CO, H2O, CO2, H2, "
		 << "Ntot, T, psi, H, H2sweep, Nsweep, CO_conv";

		if(case_type == 1) {                      // SPECIFY CONVERSION, COMPUTE LENGTH & SWEEP FLOW, 2 PASSES
		L_rxr = 30.0;                         // LONG REACTOR, ZERO H2 BACK PRESSURE.
		sweepw.Moles = 0.0;
		pass_1or2 = 1;  }
	
 integrate_again:                             // FOR GOTO STATEMENT (CASE TYPE 1, PASS 2 OF 2)

	// MEMBRANE REACTOR INTEGRATION

	for(x = 0; x < L_rxr; x+=dx) {           
		// PREDICTOR
		PH2_perm = (pass_1or2 == 1) ? 0.0 : sweepw.comp[i_H2->second] * sweepw.P;
		dndp = Get_H2_flux(gasw.T, gasw.P*gasw.comp[i_H2->second]) * 
			memb_area_per_length * dx;
		if(f_error) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Negative H2 permeation: H2 pressure is greater in sweep than feed side");
			return exit_condition;
		}
		if(dndp > gasw.moles("H2")) { 
			dndp = gasw.moles("H2")-0.001*gasw.moles("H2");  // to treat as warning, transfer only till Pdrive = 0
			//cout << "\nERROR step size too large"; 
			//if(!f_step_size_error) {
			//	f_step_size_error = true;
				exit_condition.first.push_back(-1);
				string msg = "Step size too large for H2 flux.";//: transfering 99.9% of H2 in step."; 
				msg += "\n\tDecrease number modules or length, or increase membrane thickness";
				exit_condition.second.push_back( msg );
				return exit_condition;
			//}
		}
		dH   = -1.0 * gasw.thermo_data.enthalpy_is(i_H2->second,gasw.T)/1000.0 * dndp - dheat_loss;
		gasw.moles(-dndp, "H2");
		ndp += dndp;
		H   += dH;
		f_error = adiabatic_equilibrium_solver(gasin, gasw, f_reaction, false);
		if(f_error) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Equilibrium solver did not converge");
			return exit_condition;
		}

		sweepw.moles(dndp,"H2");

		// CORRECTOR
		PH2_perm = (pass_1or2 == 1) ? 0.0 : sweepw.comp[sweepw.names["H2"]] * sweepw.P;
		dndp2 = Get_H2_flux(gasw.T, gasw.P*gasw.comp[gasw.names["H2"]]) * 
			memb_area_per_length * dx;
		if(f_error) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Negative H2 permeation: H2 pressure is greater in sweep than feed side");
			return exit_condition;
		}
		if(dndp2 > gasw.moles("H2")) dndp2 = gasw.moles("H2");
		dH2  = -1.0 * gasw.thermo_data.enthalpy_is(i_H2->second,gasw.T)/1000.0 * dndp - dheat_loss;
		H += 0.5*(dH2 - dH);
		ndp += 0.5*(dndp2 - dndp);
		gasw.moles((dndp-dndp2)/2.0, "H2");
		f_error = adiabatic_equilibrium_solver(gasin, gasw, f_reaction, false);
		if(f_error) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Equilibrium solver did not converge");
			return exit_condition;
		}
		sweepw.moles((dndp2-dndp)/2.0, "H2");
		heat_loss += dheat_loss;
	
		if(f_reaction && gasin.moles("CO") > 0.0)
			CO_conv = ( gasin.moles("CO") - gasw.moles("CO") ) / gasin.moles("CO"); 

		H2_recovery = ndp / (gasin.moles("CO") + gasin.moles("H2") );

		// OUTPUT DATA
		if(/*pass_1or2 != 1 &&*/ x >= L_print) 
		{	L_print += 0.05;
			cout << endl << x+dx << ' '
			         << gasw.Moles*gasw.comp[gasw.names["CO"]]  << ' ' 
			         << gasw.Moles*gasw.comp[gasw.names["H2O"]] << ' ' 
					 << gasw.Moles*gasw.comp[gasw.names["CO2"]] << ' ' 
					 << gasw.Moles*gasw.comp[gasw.names["H2"]]  << ' '
					 << gasw.Moles << ' ' 
					 << gasw.T     << ' ' 
					 << psi        << ' ' 
					 << H          << ' ' 
					 << sweepw.moles("H2") << ' ' 
					 << sweepw.Moles << ' ' 
					 << CO_conv;
			cout << " " << pass_1or2;
		}											// END OUTPUT
	
		// EXIT CONDITIONS FOR CONVERSION SPECIFICATION
		if(case_type == 1 && (CO_conv_want - CO_conv) < 0.0001) 
			break;  
		if(case_type == 1 && x == L_rxr - dx) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Desired CO conversion not in 30 meters");
			return exit_condition;  
		}
	}   // END INTEGRATION

	if(case_type == 1 && pass_1or2 == 1)  {         // E.G. CASE 1 1ST PASS TO FIND P_PERM (H2) TO USE
		double P_permeate = 0.81 * gasw.comp[gasw.names.find("H2")->second] * gasw.P;
		sweepin.Moles = ndp * (1.0 - P_permeate/sweepin.P) / 
			(P_permeate/sweepin.P - sweepin.comp[i_H2->second]);	
		if(sweepin.Moles < 0.0) sweepin.Moles = 0.0;  // LOW SPECIFIED CONVERSIONS CAN GIVE REQUIRED P > SWEEP P
	    sweepw.copy(sweepin);
		gasw.copy(mrgasin);                         // RESET PARAMETERS FOR NEXT PASS
		H = Ho;
		heat_loss = 0.0;
		ndp = 0.0;  
		pass_1or2 = 2;
		goto integrate_again; 
		}                                            // CASE 1 DOES TEST RUN TO GET SWEEP REQUIRED, THEN 2ND RUN

	L_rxr = x;

	if(L_rxr == 0.0) {
		exit_condition.first.push_back(1);
		exit_condition.second.push_back("M.R. Length is 0; Adiabaitic WGS Equilibrium Provides Desired CO Conv. at Inlet");
	}

	if(gasw.moles("CO") > gasin.moles("CO")) {
		exit_condition.first.push_back(1);
		exit_condition.second.push_back("Reverse WGS reaction occured; Negative CO conversion");
	}

	// OUTPUT REACTOR EFFLUENT		
		cout << endl << L_rxr << ' '
		     << gasw.Moles*gasw.comp[gasw.names.find("CO")->second]  << ' ' 
		     << gasw.Moles*gasw.comp[gasw.names.find("H2O")->second] << ' ' 
		     << gasw.Moles*gasw.comp[gasw.names.find("CO2")->second] << ' ' 
			 << gasw.Moles*gasw.comp[gasw.names.find("H2")->second]  << ' '
			 << gasw.Moles << ' ' 
			 << gasw.T     << ' ' 
			 << psi        << ' ' 
			 << H          << ' ' 
			 << sweepw.Moles*sweepw.comp[sweepw.names.find("H2")->second] << ' ' 
			 << sweepw.Moles << ' ' 
			 << CO_conv;

	// GET SWEEP EXIT TEMPERATURE

	sweepw.thermo_data.find_temperature(sweepw.T, dummy, 
		(sweepin.thermo_data.enthalpy_mix(sweepin.comp,sweepin.T)*sweepin.Moles/1000.0 + Ho-H-heat_loss) / 
		sweepw.Moles * 1000.0, sweepw.comp);
	if(sweepw.T < 300.0 || sweepw.T > 2000.0) {
		exit_condition.first.push_back(-1);
		exit_condition.second.push_back("Unrealistic Sweep Temperature (T < 300 or T > 2000)");
		return exit_condition;
	}
	map<string,int>::iterator iii;
	for(iii=sweepw.names.begin(); iii != sweepw.names.end(); iii++)
		if(sweepw.comp[iii->second] < 0.0 || sweepw.comp[iii->second] > 1.0) {
			exit_condition.first.push_back(-1);
			exit_condition.second.push_back("Sweep species " + iii->first + " mole fraction out of 0-1 range"); 
			cout << "\nout of range = " << sweepw.comp[iii->second] << endl;
			return exit_condition; 
		}

	// OUPUT MORE RESULTS

	cout << "\nSweep Exit Temperature (K) " << sweepw.T; 
	cout << "\nSweep Exit Pressure (Pa)   " << sweepw.P;
	cout << "\nSweep Exit Moles (moles/s) " << sweepw.Moles;
	cout << "\nSweep Exit % H2            " << sweepw.comp[i_H2->second] * 100;

	space_velocity = n_modules *3.1459/4.0*
		(shell_diameter*shell_diameter-memb_diameter*memb_diameter) *
		L_rxr / (8.314/gasin.P*(0.5*gasin.T + 0.5*gasw.T)*(0.5*gasin.Moles+0.5*gasw.Moles))*3600.0;
	cout << "\nCatalyst superficial space velocity (1/h): " << space_velocity;
	cout << "\nH2 recovery = " << H2_recovery;

	heat_loss /= 1000;                             // kW
	

	// UPDATE VISION 21 GAS

	gasw.rxrgas_2_v21gas(v21gasw);
	sweepw.rxrgas_2_v21gas(v21sweepw);
	sweepin.rxrgas_2_v21gas(v21sweepin);
	
	return exit_condition;
}

//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

double memb_rxr::Get_H2_flux(double temp, double pres)
{
	// CRISCUOLI:  mol / m2 / s 
	if(pres * PH2_perm < 0.0) {
		cout << "\nNegative H2 pressure square root error for H2 permeation\n";
		f_error = true;
	}
	double flux = 2.95E-4 * exp(-5833.5/temp) * 
			(sqrt(pres)-sqrt(PH2_perm)) / Pd_thickness;
	if(flux < 0.0)  {
	  /*		cout << "\nERROR: Negative H2 permeation (sweep to feed not allowed) \n\t"
			 << "Decrease Sweep H2 pressure to be less than Feed H2 pressure\n";
			 f_error = true;*/
	  flux = 0.0;
	}
	return flux;
}

//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

bool memb_rxr::adiabatic_equilibrium_solver(rxrgas & gasin, rxrgas &gasw, bool f_reaction, bool f_isoT)
{
	if(f_reaction) {

	// NEWTONS METHOD BASED ON APPENDIX E OF TURNS
	// ADIABATIC SOLVER FOR WGS REACTION EQUILIBRIM ONLY (FASTER) ( CO + H2O = CO2 + H2 )
	// USE EQUILIBRIUM CONSTANT VERSUS TEMPERATURE FIT, AND EXTENT OF REACTION VARIABLE PSI
        if(f_isoT)  cout << "\n\nISOTHERMAL OPTION NOT AVAILBLE; ADIABATIC ASSUMPTION USED\n\n";
		double dfK_dx_psi, dfH_dx_psi, dfK_dx_T, dfH_dx_T, Dx_psi, Dx_T;
		double x_psi = psi, x_T = gasw.T;
		double e_psi, e_T;
		double d1, d2;               // DUMMY VARIABLES
		double norm1, norm2;
		bool psi_conv = false;
		const int maxit = 500;
		int it;
		e_T   = 1.0E-5 * gasw.T;
		e_psi = (fabs(x_psi)>=1.0) ? 1.0E-5*fabs(x_psi) : 1.0E-5;
	
		for(it = 1; it <= maxit; it++) {
			d1 = fK(gasin, x_psi, x_T);  
			d2 = fH(gasin, x_psi, x_T);
			norm1 = fabs(d1) + fabs(d2);
			dfK_dx_psi = ( fK(gasin, x_psi+e_psi, x_T) - d1 ) / e_psi;
			dfK_dx_T   = ( fK(gasin, x_psi, x_T + e_T) - d1 ) / e_T;
			dfH_dx_psi = ( fH(gasin, x_psi+e_psi, x_T) - d2 ) / e_psi;
			dfH_dx_T   = ( fH(gasin, x_psi, x_T + e_T) - d2 ) / e_T;
			Dx_psi = (-dfH_dx_T*d1 + dfK_dx_T*d2) / (dfK_dx_psi*dfH_dx_T - dfK_dx_T*dfH_dx_psi);   // CRAMERS RULE
			Dx_T   = (-dfK_dx_psi*d2 + dfH_dx_psi*d1) / (dfK_dx_psi*dfH_dx_T - dfK_dx_T*dfH_dx_psi);
			if(fabs(x_psi) <= 1.0E-7 && fabs(Dx_psi) <= 1.0E-7) 
				psi_conv = true;                         
			else if (fabs(x_psi) > 1.0E-7 && fabs(Dx_psi/x_psi) <= 1.0E-7)
				psi_conv = true;
			if((fabs(Dx_T/x_T) <= 1.0E-7) && psi_conv)    
				break;
			norm2 = fabs(fK(gasin, x_psi+Dx_psi, x_T+Dx_T)) + fabs(fH(gasin, x_psi+Dx_psi, x_T+Dx_T));
			if(norm2 > norm1) {
				Dx_psi /= 5.0;
				Dx_T   /= 5.0;
			}
			x_psi += Dx_psi;
			x_T   += Dx_T;
			if(it==maxit){
				cout << "\n\nERROR EQUILIBRIUM NOT CONVERGED" << endl << endl;
				return true;     // ERROR FLAG TRUE
			}
		}
		// UPDATE EQUILIBRIUM STATE	
		psi = x_psi;
		gasw.T = x_T;
		gasw.moles(gasin.Moles*gasin.comp[gasin.names.find("CO")->second]-psi-gasw.Moles*gasw.comp[gasw.names.find("CO")->second], "CO");
		gasw.moles(gasin.Moles*gasin.comp[gasin.names.find("H2O")->second]-psi-gasw.Moles*gasw.comp[gasw.names.find("H2O")->second], "H2O");
		gasw.moles(gasin.Moles*gasin.comp[gasin.names.find("CO2")->second]+psi-gasw.Moles*gasw.comp[gasw.names.find("CO2")->second], "CO2");
		gasw.moles(gasin.Moles*gasin.comp[gasin.names.find("H2")->second]+psi-ndp-gasw.Moles*gasw.comp[gasw.names.find("H2")->second], "H2");

		// CHECK RESULTS
		if(gasw.T < 300.0 || gasw.T > 2000.0) {
			cout << "\n\nERROR Gas Temp unrealistic at equilibrium: " << gasw.T << endl << endl;
			return true;      // ERROR FLAG TRUE
		}
		map<string,int>::iterator iii;
		for(iii=gasw.names.begin(); iii != gasw.names.end(); iii++)
			if(gasw.comp[iii->second] < 0.0 || gasw.comp[iii->second] > 1.0) {
				cout << "\nERROR Gas has species mole fraction out of range in equilibrium solver: \n\t" 
					 << iii->first << ' ' << gasw.comp[iii->second] << endl << endl;
				return true;  // ERROR FLAG TRUE
			}

	}
	return false;             // ERROR FLAG TRUE
}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

double memb_rxr::fK(rxrgas &gasin, double x_psi, double x_T)
{   // NEWTON'S METHOD FUNCTION
	int i = 0;
	double K   = pow(10.0, (91607.0/x_T/x_T + 1680.0/x_T - 1.6097));
	double dummy = gasin.moles("CO");
	double specMoles[4] = {gasin.moles("CO2"), gasin.moles("H2"), 
		gasin.moles("CO"), gasin.moles("H2O")};
	for(i = 0; i<4; i++)
		if(specMoles[i] <= 0.0) specMoles[i] = 0.000001;                       // doesn't converge for initial speces
	double rhs = (specMoles[0]+x_psi) * (specMoles[1]+x_psi-ndp) /             // 0.0 but will for almost any other value (0-1)
			     (specMoles[2]-x_psi) / (specMoles[3]-x_psi);
	//double rhs = (gasin.moles("CO2")+x_psi) * (gasin.moles("H2")+x_psi-ndp) /
	//			 (gasin.moles("CO")-x_psi)  / (gasin.moles("H2O")-x_psi);
	return rhs - K;

}
//<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>

double memb_rxr::fH(rxrgas &gasin, double x_psi, double x_T)
{   // NEWTON'S METHOD FUNCTION     	
	double rhs = 0, molesi;
	map<string, int>::iterator iter;

	for(iter=gasin.names.begin(); iter != gasin.names.end(); iter++) {
		molesi = gasin.comp[gasin.names[iter->first]] * gasin.Moles;
		if(iter->first == "CO" || iter->first == "H2O")
			molesi -= x_psi;
		if(iter->first == "CO2" || iter->first == "H2")
			molesi += x_psi;
		if(iter->first == "H2")
			molesi -= ndp;
		rhs += gasin.thermo_data.enthalpy_is(iter->second,x_T) * molesi / 1000.0;
	}
	return rhs - H;
}














