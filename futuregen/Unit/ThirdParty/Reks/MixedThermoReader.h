// MixedThermoReader.h: interface for the MixedThermoReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#ifndef MIXEDTHERMOREADER_H
#define MIXEDTHERMOREADER_H

#include "StringParseUtil.h"
#include "SpecieThermo.h"
#include "REKS_Thrm_Info.h"
class REKS_specie_thermo;

class REKS_mixed_thermo_reader  
{
public:
	REKS_mixed_thermo_reader();
	virtual ~REKS_mixed_thermo_reader();

	// parse the original strict format file
	void parse_thermo(std::ifstream & inp, int& line_number,REKS_element_list elems, vector<string> specs);
	// parse the REI's free style file
	// a line started with !@ if a line for REI free style
	void parse_freestyle(char* buffer, std::ifstream &inp,REKS_specie_thermo *& p_spec_thermo, int& line_number);

	void parse(vector<string> inp_files);

	void dump();

	REKS_Thrm_Info *reks_thrm_info;
};

#endif 


