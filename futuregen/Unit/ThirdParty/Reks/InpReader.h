// InpReader.h
//
// Copyright 2002  Reaction Engineering International
// by Yang
/////////////////////////////////////////////////////

#ifndef INPREADER_H
#define INPREADER_H

#include "StringParseUtil.h"
#include "usr_gas.h"

#define RTOL  1.0e-12          /* scalar relative tolerance    */
#define ATOL  1.0e-16         /* initial time           */

class psr_spec {

public:	
	REKS_specie_thermo* specie;
	REAL mol_fraction;
	REAL mas_fraction;
	REAL positive_valence;
	REAL negtive_valence;
	void calc_valence();
};

class REKS_inp_reader
{

private:
	int case_flag;


 public:
  REKS_inp_reader();
  //check if this is a key word for the input file	
  int is_inp_keyw(string word);
  //parse the input file to set up the gas system
  int parse_inp(std::ifstream & inp, vector<REKS_specie_thermo *> speclist, int &line_number);
  void dump();
  int get_case_type(){return(case_flag);}
  void set_case_type(int flag){case_flag = flag;} 
  ~REKS_inp_reader();
 public:
  REKS_gas the_gas;
  REAL rxn_time;
  REAL deltT;
  REAL rtol;
  REAL atol;
  string time_temp_profile_fname;
  vector<string> radical_specs;
  REAL to_ttim;
  REAL slope_ttim;
  REAL start_temp;
  int ttime_type;
  REAL tau;
  REAL temp;
  REAL tinl;
  REAL volume;
  REAL qloss;
  ///////////////////////////////////////////////
  REAL mass_flow_rate;   // dol  g/s
  REAL rxr_length;       // dol  cm
  REAL rxr_Acs;          // dol  cm2
  REAL fuelConvWant;     // mkd in behalf of dol
  bool is_spec_tau;
 protected:
	int psr_preset; //3 way switch. 0 is the initial value
	//When the parser encounter REAC, set it to 1 then give error message to "FUEL, OXID, EQUI"
	//When the parser encounter "FUEL, OXID, EQUI", set it to 2, give error message to "REAC"
	vector<psr_spec *> fuel;
	vector<psr_spec *> oxid;
//Deal with portially premixed FUEL and OXID calculation
	void premix(vector<REKS_specie_thermo *> speclist);
	REAL equi;

};

#endif
