#pragma warning(disable: 4786)
#include "cat_comb.h"

namespace Vision21 {

//using namespace std;
//using namespace REI;

///////////////////////////////////////////////////////////////////////////////

cat_comb::cat_comb(string surf_mech_name) : surf(surf_mech_name)
{
	fError = false;
	if(surf.fSurfChemError) {
		cerr << "\nError initializing surface mechanism";
		fError = true;
		return;
	}

	length = 0.08;                    // meters
	inlet_velocity = 10.;
	desiredConversion = 0.99;
	pressure_drop = 0.0;
	conversion = 0.0;
	Tig        = 1000.;
	Acs_monolith = 1.0;
	mode       = "eval";
	tauAvg = -1.;
	
}

///////////////////////////////////////////////////////////////////////////////

pair<vector<int>, vector<string> > cat_comb::calculate(Gas& gasin, Gas& gasw) {
	
	pair<vector<int>, vector<string> > exit_cond;
	int i;
	
	cout << endl
		 << "**************************************************"
		 << "\n*************  Catalytic Combustor  ************"
		 << "\n**************************************************";
	cout << "\nInlet Gas Properties:";
	gasin.gas_composite.Property_Output();

	gasw.copy(gasin);
	
	surf.Acs         = surf.Dh*surf.Dh*3.141592654/4.0;	// m2
	surf.position    = 1.E-4;               // m (for Nu, Sh)					
	surf.mdot        = gasin.gas_composite.density() * surf.Acs * inlet_velocity;
	Acs_monolith     = gasin.gas_composite.M/surf.mdot*surf.Acs; // m2 open area (/ by ~ 0.7 for total area)
	//surf.mdot        = 9.631e-6; //1.386E-5;            // kg/s mass per channel.

	//-------------------------------------------------------------------------
	// Check the concentration of fuel in the inlet
	//-------------------------------------------------------------------------

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

	//-------------------------------------------------------------------------
	// For mode = design, check conversion level
	//-------------------------------------------------------------------------

	if(mode == "design" && (desiredConversion >= 1.0 || desiredConversion <= 0.0)) {
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Desired conversion must be greater than 0 and less than 100%");
		return exit_cond;
	}

	//-------------------------------------------------------------------------	
	// For mode = "design" (design mode, spec conv), check equilibrium conversion
	//-------------------------------------------------------------------------

	if(mode == "design") {
		gasw.gas_composite.equilb();                             // This modifies gasw
		conversion = computeConversion(gasin, gasw);
		if(conversion <= desiredConversion) {
			exit_cond.first.push_back(-1);
			exit_cond.second.push_back("Desired conversion is greater than equilibrium conversion");
			return exit_cond;
		}
		gasw.copy(gasin);                        // Get gasw back
	}

	//-------------------------------------------------------------------------
	// Setup REKS
	//-------------------------------------------------------------------------

	string griMechFname = string(getenv("SCI_WORK"))+
	  "/Packages/Vision21/Core/ThirdParty/Components/grimech30.dat";
	string griMechThermoFname  = string(getenv("SCI_WORK"))+
	  "/Packages/Vision21/Core/ThirdParty/Components/thermo30.dat";

	vector<string> inp_files;
	inp_files.push_back(griMechFname);
	inp_files.push_back(griMechThermoFname);
	
	reks_container reks(inp_files);
    
	if(mode == "eval"){
		reks.set_case_flag("CONSP_DIST_CAT_COMB"); //("CONSP_DIST"); 
		reks.set_time(length*100.0);
	}else
		reks.set_case_flag("CONSP_DIST_CONV_CAT_COMB"); //("CONSP_DIST"); 
	reks.set_atol(1.0e-20);
    reks.set_rtol(1.0e-8);
	reks.set_length(length*100.0);                    // cm  (ignored for case 1)
    reks.set_delt(length*100.0/500.0);                 // cm  
    reks.set_area(surf.Acs*10000.);                   // c2  
    reks.set_mass_flow(surf.mdot*1000.0);             // g/s
    reks.set_state(gasin.gas_composite.T, gasw.gas_composite.P*10); // dynes/cm2
	reks.zero_mol_frac();
	setREKSgasComp(reks, gasin);
	reks.norm_molf();

	FILE* out_file;
	if( (out_file = fopen("REKS.out", "w")) == NULL ) {
		exit_cond.first.push_back(1);
		exit_cond.second.push_back("REKS.out output file not opened");
		return exit_cond;
	}

	reks.read_kinetics(inp_files);
	reks_solve REKSsolve;

	conversion = desiredConversion;
	reks.set_fuelConvWant(desiredConversion);
	
	//----------------------------------------------------------------------------
	// Set the surface species map to reks species
	// REKS species aren't in the same order as they are read in in the
	// mechanism file
	// Be sure the ordering in the surface mech file for gas species is the same
	// as for the gas mechansim
	//-------------------------------------------------------------------------

	if(surf.nGasSp != reks.get_num_specs()) {
		exit_cond.first.push_back(1);
		exit_cond.second.push_back("REKS species list not equal to surf species list");
		return exit_cond;
	}

	std::map<std::string,int>::const_iterator ittt;
	for(ittt=reks.get_name_spec().begin(); ittt!=reks.get_name_spec().end(); ittt++)
		cout << ittt->first << endl;
	for(i=0; i<surf.nGasSp; i++) 
		surf.surfSp2reksSp[i] = 
			reks.get_name_spec().find(surf.spNames[i])->second;
	

	//-------------------------------------------------------------------------
	// Integrate Homogeneous / Heterogeneous kinetics
	//   Heterogeneous chemistry interacts with bulk gas (homogeneous)
	//   through source terms on energy and species equations
	// REKS currently solves the 1-D homogeneous surf/bulk case
	//-------------------------------------------------------------------------

	reks.get_test_sys().usrData = &surf;
	//((surface_chem*)reks.get_test_sys().usrData)->bulkTemp = 300.0;

	if( !REKSsolve.solve(reks, out_file) ) {
		exit_cond.first.push_back(1);
		exit_cond.second.push_back("Homogeneous kinetic solver failed");
		return exit_cond;
	}

	if(mode == "design")
		length = reks.get_my_reader().rxr_length / 100.;

	setGasComp(reks, gasw);

	if(mode == "eval")
		conversion = computeConversion(gasin, gasw);

   cout << " converstion " << conversion << endl;
	if(conversion < 0.0 || conversion > 1.0) {
		exit_cond.first.push_back(-1);
		char str[50], str1[25];
		strcpy(str,"Fuel conversion ");
		sprintf(str1,"%lf",&conversion);
		strcat(str,str1);
		exit_cond.second.push_back(str1);
		exit_cond.first.push_back(-1);
		exit_cond.second.push_back("Fuel conversion out of range 0 to 1");
		return exit_cond;
	}
	
	//-------------------------------------------------------------------------
	// Get Pressure Drop, Just Bernoulli's equation for channel; Avg inlet outlet gas
	//		A better way would be to integrate the pressure drop in REKS
	//-------------------------------------------------------------------------
	
	// Assumes all species in therm.dat file for thermo class are in the gasin, gasw objects
	// This is the same issue for the dump combustor with the get temperature function of the
	// thermo class that was fixed.  Fix this too
	double density = 0.5* (gasin.gas_composite.density() + gasw.gas_composite.density());
	double vel2    = inlet_velocity * gasin.gas_composite.density() / 
			         gasw.gas_composite.density();
	vector<double> comp = gasin.gas_composite.comp_specie;
	for(i=0; i<comp.size(); i++)
		comp[i] = 0.5*(comp[i] + gasw.gas_composite.comp_specie[i]);
	double velavg = 0.5*(vel2+inlet_velocity);
	double mu = gasin.thermo_database->viscosity_mix(gasin.gas_composite.T, gasw.gas_composite.T, comp);
	double Re = density * surf.Dh * velavg / mu;
	double Friction = 0.0;
	if(Re < 2300.) 
		Friction = velavg*surf.Acs*length*mu/density*128./3.141592653/pow(surf.Dh, 4.0); // Pa * kg/m3
	else {
		double fanning = pow(1.82*log10(Re) - 1.64, -2.0)/4.0;
		Friction = 2.0*fanning*length*velavg*velavg/surf.Dh;
	}
	pressure_drop = density*(Friction + ((vel2*vel2)-(inlet_velocity*inlet_velocity))/2); // Pa
		// p drop is positive, p change is negative; the above is p drop

	if(pressure_drop > gasw.gas_composite.P) {
		exit_cond.first.push_back(1);
		exit_cond.second.push_back("Computed pressure drop > gas pressure, setting to zero");
		pressure_drop = 0.0;
	}

	gasw.gas_composite.P -= pressure_drop;

	tauAvg = surf.Acs * length / gasin.gas_composite.M * 0.5 * density;


	return exit_cond;
}


//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void cat_comb::setSurfChemGasComp(Gas& gasw) {
	// note, should have all the species in kinetic mech and kinetic thermo
	// as in the gas thermo, or at least as in the current gas list
	int i;
	for(i=0; i < surf.nGasSp; i++)
		surf.bulkMolf[i] = 0.0;

	// loop through all species in SURF gas, if species is in gasw, set the species in surf gas

	map<string, int>::iterator it;
	for(i=0; i < surf.nGasSp; i++) {
		it = gasw.specie.find(surf.spNames[i]);
		if(it != gasw.specie.end())
			surf.bulkMolf[i] = gasw.gas_composite.comp_specie[it->second];
		//cout << "\nSpecies: " << name << "\t" << REKSgas.specs[i].mole_fraction;
	}
	surf.normalize(surf.bulkMolf);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////
		
void cat_comb::setREKSgasComp(reks_container& reks, Gas& gasw) {
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
	reks.norm_molf();
}

//////////////////////////////////////////////////////////////////////////////////////////////////////////////

void cat_comb::setGasComp(reks_container& reks, Gas& gasw) {
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

double cat_comb::computeConversion(Gas& gasin, Gas& gasw) {
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

