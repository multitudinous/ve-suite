// NOTE: This parser assumes a correct mechanism file, not a user input file
//       No error handling
// Taken and modified (reduced, no error handling) from David Lignell, 2003 
//		 Chemkin II mechanism and thermo db reader.

#ifndef SURF_PARSER_H
#define SURF_PARSER_H

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <vector>

#include "GlobalConst.h"
#include "reactionObject.h"
#include "thermo_db.h"


class surf_parser {

public:
	surf_parser();
	std::vector<reactionObject>   rxnsData;
	std::vector<std::string>      spNames;
	std::map<std::string, int>    speciesMap;
	std::vector<double>           s_lj;
	std::vector<double>           ek_lj;
	std::vector<double>           delta_lj;

	int	                          nGasSp;
	int                           nSurfSp;

	thermo_db thermoReader; 
	
	bool parse_surf_mech(std::string mechFileName);
	
	void processReactionLine(std::string s1);
	void processAuxilaryLine(std::string s1);
	
	void getNextLine(std::ifstream& input, std::string& line);



};

#endif

