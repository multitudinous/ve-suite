
    // class definition for thermo class

#ifndef _fc_h_
#define _fc_h_

#include <V21Helper/Therm/thermo.h>
#include "aspen_stream.h"
#include <iostream>

namespace Vision21 {

using namespace std;

class fuel_cell
{

public:

	class fc_params
	{
	public:

		bool internal_reformer;
		double pressure;
		double fuel_util;

		double athick;
		double cthick;           // thicknesses
		double ethick;

		double anode_A;
		double cathode_A;
		double electrolyte_A;    // these A's and E's are for Resistivity R=A*exp(E/T)
		double anode_E;
		double cathode_E;
		double electrolyte_E;

		double area_per_unit_cell;
		double number_of_cells;
	};

	
public:
	
	fuel_cell(const thermo& thm);
	double fc_electric_power(const double& tanode);
	double fc_thermo_power(const double& tanode);
	bool calc_power(double& temp, double& fc_power);
	void set_cathode_stream(const aspen_stream& cathin);
	void set_anode_stream(const aspen_stream& anodein);
	void set_fc_params(const fc_params& fcp){fcparams = fcp;}
	aspen_stream get_anodeout(){return(anodeout);}
	aspen_stream get_cathout(){return(cathout);}
	void get_output_stream(aspen_stream&);



private:
	
	const thermo& thm;

	fc_params fcparams;

	aspen_stream cathin;
	aspen_stream d_cathin;
	aspen_stream o2_flow;
	aspen_stream cathout;

	aspen_stream anodein1;
	aspen_stream anodein2;
	aspen_stream anodeout;
};

} //# End namespace Vision21

#endif
