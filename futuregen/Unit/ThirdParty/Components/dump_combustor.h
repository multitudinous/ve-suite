#ifndef DUMP_COMBUSTOR_H
#define DUMP_COMBUSTOR_H

/* Dump combustor model.  
   David Lignell, REI May 2003.
   
   Single PFR Call to modified REKS.  Two operational Modes:
	1. Evaluation Mode (caseType = 0).  Specify unit volume, compute conversion, etc.
	2. Design Mode (caseType = 1).  Specify desired conversion, compute required volume, etc.

	Inputs: 
			SET DATA MEMBERS
			volume (case 0)
			desiredConversion (case 1)
			caseType (0 or 1)
			fHeatLoss (0 to 1)
			deltaP

			PASS INTO CALCULATE FUNCTION ARGUMENT LIST
			V21 incomming gas

	Outputs:
			AS DATA MEMBERS
			heatLoss (kW)
			conversion (fraction of fuel h2 co ch4)
			average residence time

			PASS OUT OF CALCULATE FUNCTION ARGUMENT LIST
			V21 outgoing gas

*/
#include <winsock2.h> //have to include this to prevent the windows.h's mess up with winsock.h
#include <iostream>
#include <fstream>
#include <vector>
#include <utility>
#include <cstdio>
#include <string>
#include <V21Helper/Datatypes/Gas.h>
#include <V21Helper/Datatypes/GasCell.h>
//#include "InpReader.h"
#include <ThirdParty/Reks/reks_container.h>
#include <ThirdParty/Reks/reks.h>
#include <ThirdParty/Reks/reks_solve.h>

namespace Vision21 {

class dump_combustor {

	public:

		double volume;                        // specified combustion volume
		double desiredConversion;             // specified conversion for case type
		double exitTemp;                      // computed exit temperature
		int    caseType;                      // 0 for detail spec (evaluation), 1 for spec conv (design) 
		double fHeatLoss;                     // Fraction of inlet sensible enthalpy relative to 298 K
		double heatLoss;                      // kW
		double deltaP;					      // pressure drop (psi positive)
		double conversion;                    // computed fuel conversion (CO H2 CH4, or those present)
		double tauAvg;                        // computed average residence time (based on in and out, not integrated)

		dump_combustor();
		pair<vector<int>, vector<std::string> > calculate(Gas& gasin, Gas& gasw);

	private:

		void setREKSgasComp(reks_container& reks, Gas& gasw);      // convert V21 to Reks gas
		void setGasComp(reks_container& reks, Gas& gasw);          // convert reks gas to V21 gas
		double computeConversion(Gas& gasin, Gas& gasw);   // return fuel conversion
};

} //# End namespace Vision21

#endif 
