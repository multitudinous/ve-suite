// SpecieReader.h: interface for the SpecieReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#ifndef SPECIEREADER_H
#define SPECIEREADER_H

#pragma warning(disable : 4786)
#include "REKS_Thrm_Info.h"

#include <string>
#include <vector>

using std::string;
using std::vector;

class REKS_Thrm_Info;

class REKS_specie_reader  
{
public:
	REKS_specie_reader();
	virtual ~REKS_specie_reader();

	//specie name must be short than 16 characters
	bool spec_length_check();
	//check if it is already in the m_specs list
	bool already_in(string spec);
	//line parser to fill the specs vector with the species in this line
	bool fill_specs(vector<string>specs, int& pos, int line_num);

	void parse(vector<string> inp_files);

	void dump();

	REKS_Thrm_Info* reks_thrm_info;
};

#endif 

