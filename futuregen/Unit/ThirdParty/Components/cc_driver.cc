#pragma warning(disable: 4786)
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!1
// REMEMBER TO HARDWIRE THE NUMBER SURFACE AND GAS SPECIES IN THE
// SURFACE READER CONSTRUCTOR FOR THE SURFACE MECHANISM
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#include "cat_comb.h"

using namespace std;
//using namespace REI;


int main() {

int i;
pair<vector<int>, vector<string> > exit_condition;

//-----------------------------------------------------------------------
// INITIALIZE INLET GAS
//-----------------------------------------------------------------------

Gas gasin, gasw;
/*
gasin.gas_composite.T = 900.0;              // K
gasin.gas_composite.P = 14.7*101325.0;      // Pa
gasin.gas_composite.M = 350.0;              // kg/s

// Vision21 composition (cleaned High Pressure FC exhaust)

gasin.specie = gasin.thermo_database->get_nam_spec();
gasin.gas_composite.comp_specie.resize(gasin.thermo_database->get_mwt().size());
for(i = 0; i < gasin.gas_composite.comp_specie.size(); i++)
	gasin.gas_composite.comp_specie[i] = 0.0;
gasin.gas_composite.comp_specie[gasin.specie["CH4"]]  = 1.98008E-06;
gasin.gas_composite.comp_specie[gasin.specie["CO"]]   = 0.009062681;
gasin.gas_composite.comp_specie[gasin.specie["CO2"]]  = 0.058246607;
gasin.gas_composite.comp_specie[gasin.specie["H2"]]   = 0.007472281;
gasin.gas_composite.comp_specie[gasin.specie["H2O"]]  = 0.052322607;
gasin.gas_composite.comp_specie[gasin.specie["N2"]]   = 0.723600093;
gasin.gas_composite.comp_specie[gasin.specie["O2"]]   = 0.149292019;
gasin.gas_composite.normalize_specie();
*/

/*gasin.gas_composite.T = 1500.; //852.;              // K
gasin.gas_composite.P = 101325.0;           // Pa
gasin.gas_composite.M = 9.631E-6;              // kg/s

// Vision21 composition (cleaned High Pressure FC exhaust)

gasin.specie = gasin.thermo_database->get_nam_spec();
gasin.gas_composite.comp_specie.resize(gasin.thermo_database->get_mwt().size());
for(i = 0; i < gasin.gas_composite.comp_specie.size(); i++)
	gasin.gas_composite.comp_specie[i] = 0.0;
gasin.gas_composite.comp_specie[gasin.specie["CH4"]]  = 0.03055;
gasin.gas_composite.comp_specie[gasin.specie["CO"]]   = 1.0E-18;
gasin.gas_composite.comp_specie[gasin.specie["CO2"]]  = 1.0E-18;
gasin.gas_composite.comp_specie[gasin.specie["H2"]]   = 1.0e-18;
gasin.gas_composite.comp_specie[gasin.specie["H2O"]]  = 1.0E-18;
gasin.gas_composite.comp_specie[gasin.specie["N2"]]   = 0.76576;
gasin.gas_composite.comp_specie[gasin.specie["O2"]]   = 0.2036657;
gasin.gas_composite.normalize_specie();*/

// Seo et al composition

gasin.gas_composite.T = 918.; //852.;              // K
gasin.gas_composite.P = 101325.0;           // Pa
gasin.gas_composite.M = 7.13967E-06;              // kg/s

gasin.specie = gasin.thermo_database->get_nam_spec();
gasin.gas_composite.comp_specie.resize(gasin.thermo_database->get_mwt().size());
for(i = 0; i < gasin.gas_composite.comp_specie.size(); i++)
	gasin.gas_composite.comp_specie[i] = 0.0;
gasin.gas_composite.comp_specie[gasin.specie["CH4"]]  = 0.0294;
gasin.gas_composite.comp_specie[gasin.specie["N2"]]   = 0.766774;
gasin.gas_composite.comp_specie[gasin.specie["O2"]]   = 0.203826;

/*gasin.gas_composite.comp_specie[gasin.specie["N2"]]  = 0.714932127;
gasin.gas_composite.comp_specie[gasin.specie["O2"]]   = 0.200476;
gasin.gas_composite.comp_specie[gasin.specie["H2"]]   = 8.51829e-14;
gasin.gas_composite.comp_specie[gasin.specie["H2O"]]   = 0.000190356;
gasin.gas_composite.comp_specie[gasin.specie["CO"]]   = 8.81569e-11;
gasin.gas_composite.comp_specie[gasin.specie["CH4"]]   = 0.027282;*/
gasin.gas_composite.normalize_specie();

gasw.copy(gasin);

//-----------------------------------------------------------------------
// INITIALIZE CATALYTIC COMBUSTOR SURFACE CHEM PARAMS
// THIS COULD GO IN THE CAT COMBUSTOR CALCULATE FUNCTION TO 
//    SIMPLIFY THE UI IN V21 WORKBENCH
//-----------------------------------------------------------------------

string surf_mech_name = "Chou_Surf_Chem_ch4.dat"; 

cat_comb cat(surf_mech_name);

if(cat.fError) {
	cerr << "Error Initializing Surface Mechanism";
	return 0;
}

// Initialize surface chem gas compositions

cat.setSurfChemGasComp(gasin);                // bulk gas  
cat.surf.molf     = cat.surf.bulkMolf;        // surface gas  bulkMolf is size nGasSp, molf is nGasSp + nSurfSp
cat.surf.bulkTemp = gasin.gas_composite.T;    // K
cat.surf.surfTemp = cat.surf.bulkTemp;        // K
cat.surf.pres     = gasin.gas_composite.P;    // Pa

// Initialize surface species

for(i=cat.surf.nGasSp; i<cat.surf.spNames.size(); i++)
	cat.surf.molf.push_back(1.0);         
cat.surf.molf[cat.surf.speciesMap.find("PT(S)")->second] = 1000.;
cat.surf.normalize(cat.surf.molf);

// Initialize channel properties (monolith rxr)

cat.length           = 0.078;                // m
cat.surf.eff_factor  = 1.0;                 // effectiveness factor
cat.surf.Dh          = 0.0012;	            // m, hydraulic diameter						
cat.surf.siteDensity = 2.7E-9*10000.0;      // moles/m2
cat.inlet_velocity   = 16.7;                 // m/s
								              // overrides gas flow for now; later make dep on geom

//-----------------------------------------------------------------------------
// SOLVER
//-----------------------------------------------------------------------------


exit_condition = cat.calculate(gasin, gasw);

if(exit_condition.first.size() != exit_condition.second.size()) {
	cerr << "\nERROR EXIT CONDITION PAIR SIZES NOT THE SAME\n";
	return 0;
}
if(exit_condition.first.size() == 0) {
	cout << "\nSUCCESSFUL EXECUTION\n";
	return 0;
}
else {
	int i;
	string s1;
	for(i=0; i < exit_condition.first.size(); i++) 
		cout << endl << ( (exit_condition.first[i] == 1) ? "Warning: " : "Error: " )
			 << exit_condition.second[i];
	return 0;
}


	return 0;
}