#ifndef SURFACE_CHEM_H
#define SURFACE_CHEM_H

#include <iostream>
#include <sstream>
#include <string>
#include <fstream>
#include <cmath>
#include <vector>
#include <map>

#include "reactionObject.h"
#include "surf_parser.h"

class surface_chem {

public:
	///////////////// data members

	std::vector<reactionObject>			rxnsData;               // surface reaction data
	std::vector<std::string>			spNames;                // nGasSp followed by nSurfSp (size of surf and gas sp)
	std::vector<std::vector<double> >	thermoCoeffs;           // thermo cp coeffs (unitless)
	std::vector<std::vector<double> >   tempRanges;             // thermo tlow, thigh, tmid
	std::vector<double>					spMWs;                  // g/mol
	std::map<std::string, int>			speciesMap;             // one to one with spNames
	std::vector<int>                    rxnGasSpInd;            // vector of positions of gas species in surf rxns

	std::vector<int>                    surfSp2reksSp;          // map, index value is corresponding position in reks list
                                                
   	int									nGasSp;
	int									nSurfSp;                 
	bool								fSurfChemError;
	
	std::vector<double>                 molf;                   // Note mixed array (surf and gas)
	std::vector<double>                 bulkMolf;               // composition of bulk gas (size of nGasSp)
	double                              surfTemp;
	double                              bulkTemp;
	double                              pres;

	double                              siteDensity;             // moles/m2
	double                              Dh;                      // channel hydraulic diameter m
	double                              Acs;                     // m2
	double                              mdot;                    // kg/s
	double                              position;                // meters; current in integration

	double                              Rgas;

	std::vector<double>                 s_lj;                    // L.J. transport parameters 
	std::vector<double>                 ek_lj;                   // size of gas species
	std::vector<double>                 delta_lj;
	
	double								eff_factor;              // effectiveness factor 

	bool								*steady_state;			 // used to solve for surface comp

	///////////////// accessors

	int get_nSurfSp() {return nSurfSp;}
	double get_molf(int is) {return molf[is+nGasSp];}
	void set_molf(int is, double molef) {molf[is+nGasSp] = molef;}

	// linear solfver

	int SolveLAE(vector<vector<double> >& a, const vector<double>& func, vector<double>& x);

	///////////////////  constructors

	surface_chem(std::string mechFileName);
	
	///////////////////  transport properties

	double				viscosity_mix(const double& temp_gas, const double& temp, 
									  const std::vector<double>& composition);
	double				thermal_cond_mix(const int& ieuck, double& temp_gas, double& temp,
                              const std::vector<double>& composition);
	std::vector<double> get_diffus(const double& temp_gas, const double& temp,
								   const std::vector<double>& composition, const double& pres);

	double				sigmam(const double& tek, const double& del);
	double				sigmad(const double& tek);
	void				find(const double& arg, double& ans, double* x, double* y, const int& npts);

	std::vector<double> kMass();                           // mass transfer coeff for gas species size nGasSp
	double              kHeat();                           // heat transfer coeff, W/m2*K

	/////////////////// thermodynamic properties

	double				cp_i(int i, double& temp_gas);
	double				cp_mix(double& temp_gas, std::vector<double>& gasComp);
	double				dHRxnI(const int i, double& Temp);               // enthalpy of reaction
	double				dSRxnI(const int i, double& Temp);               // entropy of reaction
	double				enthalpySp(const int i, double& Temp);
	double				entropySp(const int i, double& Temp);            // standard entropy of pure species at Temp
	
	double				KcRxnI(const int i, double& Temp);               // rxn equilibrium constant 
	double				KpRxnI(const int i, double& Temp);               // rxn equilibrium constant

	/////////////////// kinetics

	double              kfRxnI(const int i, double& Temp);                    // foreward rate constant
	double              krRxnI(const int i, double& Temp, double kf = -1);    // only for reversible reaction
	std::vector<double> rxnRateOfProgress(std::vector<double>& comp, double& Temp);
	std::vector<double> gasSpProdRates(double& Temp);                         // size nGasSp
	std::vector<double> surfSpProdRates(std::vector<double>& comp, double& Temp);  // size nSurfSp
	double				temp_rate(const std::vector<double>& sources);

	/////////////////// Gas Properties

	double              concI(const int i);
	std::vector<double> conc(std::vector<double>& comp, double& Temp);
	void                normalize(std::vector<double>& comp);     // normalize nGasSp and nSurfSp parts of molf Array
	std::vector<double> getFilmMolf();                            // size of nGasSp
	double              getMeanMW(std::vector<double>& gasMolf);  // gasMolf size of nGasSp or nGasSp+nSurfSp

	/////////////////// Solver

	bool                solveSurfaceComposition();                // Newton's Method
	bool                getJacobian(std::vector<std::vector<double> >& Jac, double &Temp, 
		                            std::vector<double>& surfRates);
	bool				solveSurfFixedPoint();                    // Alternate solver fixed point method
	std::vector<double> getBulkGasSources_Hetero();               // 1-D Heterogeneous model (diff Bulk and surf comp/temp)
	std::vector<double> getBulkGasSources_Homo();                 // 1-D Homogeneous model (surf temp, gas comp = bulk)

};



#endif
