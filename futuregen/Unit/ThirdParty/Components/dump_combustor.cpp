#pragma warning(disable: 4786)

#include "dump_combustor.h"

using namespace std;

namespace Vision21 {

////////////////////////////////////////////////////////////////////////////////////////////////

dump_combustor::dump_combustor() {

		 volume                 = 8.55;
		 desiredConversion      = 99.0; 
		 exitTemp               = 1200.0;
		 caseType               = 1;
		 fHeatLoss              = 0.0;
		 heatLoss               = 0.0;
		 deltaP                 = 0.0;         // psi (+)
		 conversion             = 0.0;
		 tauAvg                 = 0.0;
}

////////////////////////////////////////////////////////////////////////////////////////////////

pair<vector<int>, vector<string> > dump_combustor::calculate(Gas& gasin, Gas& gasw) {
		
	double d1;                    // dummy

	cout << endl
		 << "**************************************************"
		 << "\n*************  Dump Combustor  *******************"
		 << "\n**************************************************";
	cout << "\nInlet Gas Properties:";
	gasin.gas_composite.Property_Output();

	gasw.copy(gasin);

	pair<vector<int>, vector<string> > exit_cond;
	
	// Check the concentration of fuel in the inlet

	double sum = 0;
	if(gasin.specie.find("H2") != gasin.specie.end()) 
		sum+=gasin.gas_composite.moles("H2");
	if(gasin.specie.find("CO") != gasin.specie.end())
		sum+=gasin.gas_composite.moles("CO");	
	if(gasin.specie.find("CH4")!= gasin.specie.end())
		sum+=gasin.gas_composite.moles("CH4");
	if(sum <= 1.0E-10) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("No fuel present in inlet gas: CO + H2 + CH4");
		return exit_cond;
	}

	// For caseType = 1, check conversion level

	if(caseType == 1 && (desiredConversion >= 100.0 || desiredConversion <= 0.0)) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Desired conversion must be greater than 0 and less than 100.0");
		return exit_cond;
	}

	// Check and Assign Pressure Drop

	if(deltaP * 6894.76 >= gasin.gas_composite.P || deltaP < 0) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("\nPressure Drop should be positive and greater than inlet gas pressure");
		return exit_cond;
	}
	gasw.gas_composite.P -= deltaP * 6894.76;

	// Check and Assign Heat Loss, modify gasw temp

	if(fHeatLoss < 0 || fHeatLoss > 1) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Fractional heat loss must be between 0 and 1");
		return exit_cond;
	}

	heatLoss = ( gasw.gas_composite.enthalpy() - gasw.gas_composite.enthalpy(298.15) ) *
                 gasw.gas_composite.moles()/1000.0 * fHeatLoss / 1000.0;         // kW
		
	REAL e_temp = gasw.gas_composite.enthalpy() - 
	  heatLoss / gasw.gas_composite.moles() * 1000.0 * 1000.0;
	
	gasw.gas_composite.find_temperature(gasw.gas_composite.T, d1, e_temp);
	
	cout << "\ngasw T " << gasw.gas_composite.T;

	// For caseType = 1 (design mode, spec conv), check equilibrium conversion

	if(caseType == 1) {
		gasw.gas_composite.equilb();                             // This modifies gasw
		conversion = computeConversion(gasin, gasw);
		if(conversion <= desiredConversion) {
			exit_cond.first.push_back(-1);
			exit_cond.second.push_back("Desired conversion is greater than equilibrium conversion");
			return exit_cond;
		}
		gasw.copy(gasin);                        // Get gasw back
		
	       if(gasw.gas_composite.getFrac("NO") == -1) {
		  gasw.addSpecie("NO");
   		 } 		  
		
		
		gasw.gas_composite.P -= deltaP * 6894.76;
		e_temp = gasw.gas_composite.enthalpy() - heatLoss / gasw.gas_composite.moles() * 1000.0 * 1000.0;
		gasw.gas_composite.find_temperature(gasw.gas_composite.T, d1, e_temp);
	}
	
	// Initialize dimensions for integration

	double diameter, length, area, delta_L;
	if(caseType == 0) {                                      // evaluation case: detailed spec
		diameter = pow(volume*4.0/3.14/10.0, 0.33333);       // m  (temporary variable)
		length   = diameter * 10.0;                          // m
		area     = 3.14/4.0 * diameter * diameter;           // m2
		delta_L  = length / 50;
	}
	else {                                                   // design case: spec conversion
		length  = 1.0;                                       // get area for 1.0 m length, 1.0 sec tau
		delta_L = 10000.0;                                      // suppress output in f_conp_dist
		area    = gasw.gas_composite.M /                        // note step sz hrdwrd 0.001 m CONSP_DIST_CONV
				 gasw.gas_composite.density() * 1.0 / length;   // m2: mdot / rho * tau / length
	}
 
	string griMechFname = "grimech30.dat";
	string griMechThermoFname  = "thermo30.dat";

	vector<string> inp_files;
	inp_files.push_back(griMechFname);
	inp_files.push_back(griMechThermoFname);

	reks_container reks(inp_files);
    if(caseType == 0)  reks.set_case_flag("CONSP_DIST");
	else               reks.set_case_flag("CONSP_DIST_CONV");
    reks.set_atol(1.0e-14);
    reks.set_rtol(1.0e-8);
	reks.set_length(length*100.0);                            // cm  (ignored for case 1)
    reks.set_delt(delta_L*100.0);                             // cm  
    reks.set_area(area*100.0*100.0);                          // m2  (arbitrary)
    reks.set_mass_flow(gasw.gas_composite.M*1000.0);          // g/s
    reks.set_state(gasw.gas_composite.T,gasw.gas_composite.P*10);    // dynes/cm2
    reks.zero_mol_frac();
	setREKSgasComp(reks, gasw);
	reks.norm_molf();

	FILE* out_file;
	if( (out_file = fopen("REKS.out", "w")) == NULL ) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("REKS.out output file not opened");
		return exit_cond;
	}

	reks.read_kinetics(inp_files);
	reks_solve REKSsolve;

	conversion = desiredConversion;
	reks.set_fuelConvWant(desiredConversion);
	if( !REKSsolve.solve(reks, out_file) ) { //, conversion, length) ) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Kinetic solver failed");
		return exit_cond;
	}
        if(caseType == 1) length = reks.get_rxr_length();

	if(caseType == 1) 
		length /= 100.0;                           // convert length parameter cm to m
	setGasComp(reks, gasw);
	if(caseType == 0)
		conversion = computeConversion(gasin, gasw);
	if(conversion < 0.0 || conversion > 1.0) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Fuel conversion out of range 0 to 1");
		return exit_cond;
	}
	
	tauAvg = area * length / gasin.gas_composite.M * 0.5 * 
		(gasin.gas_composite.density() + gasw.gas_composite.density());
	exitTemp = gasw.gas_composite.T;

	cout << "\n\n********** RESULTS ************\n";
	if(caseType == 0) cout << "\nEvaluation Case: Specified Volume, Compute Fuel Conversion";
	else              cout << "\nDesign Case: Specify Desired Conversion, Compute Volume";
	cout << "\nAverage residence time = " << tauAvg;
	if(caseType == 1) 
	cout << "\nSpecified Conversion   = " << desiredConversion *100 << "%";
	cout << "\nFuel conversion        = " << conversion*100 << "%";
	cout << "\nCombustion Volume (m3) = " << area * length;
	cout << "\nHeat Loss (kW)         = " << heatLoss;
	cout << "\nPressure Drop (psi)    = " << deltaP;
	cout << "\nExit Gas Propterties: ";
	gasw.gas_composite.Property_Output();

	volume = area*length;
	

return exit_cond;

}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void dump_combustor::setREKSgasComp(reks_container& reks, Gas& gasw) {
	// note, should have all the species in kinetic mech and kinetic thermo
	// as in the gas thermo, or at least as in the current gas list
	reks.zero_mol_frac();
	// loop through all species in REKS gas, if species is in gasw, set the species in reks gas
	int i;
	string name;
	map<string, int>::iterator it;
	for(i=0; i < reks.get_num_specs(); i++) {
		name = reks.get_specie_name(i);
		it = gasw.specie.find(name);
		if(it != gasw.specie.end())
			reks.set_mol_frac(name, gasw.gas_composite.comp_specie[it->second]);
		//cout << "\nSpecies: " << name << "\t" << REKSgas.specs[i].mole_fraction;
	}
	//reks.norm_molf();
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void dump_combustor::setGasComp(reks_container& reks, Gas& gasw) {
	// note, should have all the species in kinetic mech and kinetic thermo
	// as in the gas thermo, or at least as in the current gas list
		
	gasw.gas_composite.T = reks.get_temperature();
	gasw.gas_composite.P = reks.get_pressure()/10.0;          // dyne/cm2 to Pa

	// loop through all the species in gas, if species is in REKS gas set the gas species
	int i;
	map<string, int>::const_iterator it, it1;
	string name; double sum = 0.0;
        for(i=0; i<reks.get_num_specs(); i++) {
	  name = reks.get_specie_name(i);
	  it = gasw.specie.find(name);
	  if(it!=gasw.specie.end()){
	    gasw.gas_composite.comp_specie[it->second] = reks.get_specie_mole_fraction(i);
            sum += reks.get_specie_mole_fraction(i);
	  } else {
            it1 = gasw.thermo_database->get_nam_spec().find(name);
	    if(it1!=gasw.thermo_database->get_nam_spec().end()){
	      gasw.addSpecie(name);
	      gasw.gas_composite.comp_specie[gasw.gas_composite.comp_specie.size()-1] = 
		reks.get_specie_mole_fraction(i);
	      sum += reks.get_specie_mole_fraction(i);
	    }
	  }
	}
	cout << " SUM " << sum << endl;

	//gasw.gas_composite.normalize_specie();
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

double dump_combustor::computeConversion(Gas& gasin, Gas& gasw) {
	double currentFuel = 0;
	double inletFuel = 1.0E-30;
	if(gasin.specie.find("H2") != gasin.specie.end()) {  
		inletFuel += gasin.gas_composite.moles("H2");
		currentFuel += gasw.gas_composite.moles("H2");
	}
	if(gasin.specie.find("CO") != gasin.specie.end()) {
		inletFuel += gasin.gas_composite.moles("CO"); 
		currentFuel += gasw.gas_composite.moles("CO"); 
	}
	if(gasin.specie.find("CH4")!= gasin.specie.end()) {
		inletFuel += gasin.gas_composite.moles("CH4");
		currentFuel += gasw.gas_composite.moles("CH4"); 
	}

	return ( (inletFuel-currentFuel) / inletFuel );
}

} //# End namespace Vision21














