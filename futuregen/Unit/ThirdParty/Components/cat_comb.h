#ifndef CAT_COMB_H
#define CAT_COMB_H

#include <ThirdParty/Reks/surface_chem.h>
#include <V21Helper/Datatypes/Gas.h>
#include <V21Helper/Datatypes/GasCell.h>
#include <ThirdParty/Reks/reks_container.h>
#include <ThirdParty/Reks/reks.h>
#include <ThirdParty/Reks/reks_solve.h>
#include <cmath>
#include <utility>

using std::pair;
using std::vector;
using std::string;

namespace Vision21 {

class cat_comb {

public:
	
	surface_chem		surf;
	bool				fError;
	double				length;          // m/s
	double              inlet_velocity;  // m/s
	double              desiredConversion;  // fraction
	double              pressure_drop;   // Pa
	double              conversion;      // fraction
	double              Tig;             // K (not used as of 8/9/03)
	double              Acs_monolith;    // m2
	double				tauAvg;          // residence time, seconds
	string              mode;            // "design" or "eval"

	cat_comb(string surf_mech_name);
	pair<vector<int>, vector<string> > calculate(Gas& gasin, Gas& gasw);

	void   setSurfChemGasComp(Gas& gasw);
	void   setREKSgasComp(reks_container& reks, Gas& gasw);
	void   cat_comb::setGasComp(reks_container& reks, Gas& gasw);
	double cat_comb::computeConversion(Gas& gasin, Gas& gasw);


};


} // end namespace Vision21




#endif
