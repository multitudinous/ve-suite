// ReactionReader.h: interface for the ReactionReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#ifndef REACTIONREADER_H
#define REACTIONREADER_H

#pragma warning(disable : 4786)
#include "StringParseUtil.h"
#include "SpecieThermo.h"
#include "Reaction.h"
#include "REKS_Thrm_Info.h"
#include "reks.h"
class REKS_reaction_reader  
{
public:
	REKS_reaction_reader();
	virtual ~REKS_reaction_reader();
	
	//driving function to parse the file
	//speclist passed in by the thermo specie parser 
	void parse_reaction(std::ifstream & inp, vector<REKS_specie_thermo *> speclist, int &line_number);
	
	//Check if this line contains a reaction equation
	int reaction_line(char* buffer);
	
	//Get the tokens in a line which contains a reaction equation and fill them in the toks list
	int get_reaction_token(char* current_line, vector<string>& toks);

	//from the tokens list, put the species on left side of "=", "=>" or "<=>" into reactants list
	//and put the species on right side into products list
	unsigned int fill_reactants_products(vector<string> toks, vector<string> &reactants, vector<string> &products, vector<string> &third_specs);
	
	//check if the first arg is a specie name by looking it up in the second arg
	//and check if there are any stoichiometric coefficiency, and assigned it to the third arg
	//if not specified, it is 1
	//the return value is a pointer to specie_thermo class of that specie
	REKS_specie_thermo * spec_keyword(string spec_name, vector<REKS_specie_thermo *> speclist, REAL &eff);

	//Check if it is a reaction Auxilury data keyword and return a number
	//for a switch-case condition to check
	//-1 for 'END'
	//specie name not counted
	//0 for not a keyword (specie name is not a keyword for this function)
	int reaction_keyword(string token);

	//check if the input is a real number
	//just call the global utility is_number
	bool is_a_param_REAL(string word);

	//basically, this means there is 1 number and only 1 number 
	//after the tokens at postion pos in the vector
	bool is_param_spec(vector<string> tokens, int pos, vector<REKS_specie_thermo *> speclist);

	void parse(vector<string> inp_files);

	void dump();

public:
	REKS_sys * kinetics;
	
};

#endif 

