//Reaction Engineering Chemkin system
//this class put things together to compute reactio rates
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////

#ifndef REKS_H
#define REKS_H

#include "Reaction.h"
#include "REKS_Thrm_Info.h"
#include "usr_gas.h"
#include "expl_dll.h"
#include "surface_chem.h"

class reks_solve;

class REKS_sys{
 public:
  
//  vector<REKS_reaction *>* reactions;
  REKS_gas * gas_sys;
  surface_chem *usrData;                //dol catalytic combustor
  vector<REAL> rate_prog_v;
  //since they are the same order now, just use plain vector
  //map<REKS_specie_thermo *,REAL> prod_rate;
  //map<REKS_specie_thermo *,REAL> masf_prodrate;
  vector<REAL> prod_rate;
  vector<REAL> masf_prodrate;

  REAL* wdot;
  REAL* Y;
  REAL rxr_length;        // dol
  REAL rxr_Acs;           // dol
  REAL mass_flow_rate;    // dol

  //a pointer to the reks solver
  reks_solve * solver;

//The place hold for reaction and thermo informations
  vector<REKS_reaction *> m_p_reactions;
  REKS_Thrm_Info* reks_thrm_info;
 
 public:

  //returns the pointer of a vector hold ihng the rate of progress variable for all reactoins
  vector<REAL>*  rate_of_progress();

  //compute the rate of all the species in the input file
  //map<REKS_specie_thermo *,REAL>* comp_rate_all();
  vector<REAL>* comp_rate_all();

  //compute the rate of all the species in the reduced file
  //  map<REKS_specie_thermo *,REAL>* REKS_sys::comp_rate_allRED();
  vector<REAL>* comp_rate_allRED();
  //compute the rate of the specie
  REAL comp_rate(string spec);

  //calc the third body coefficiency for the reaction
  REAL third_body_eff(REKS_reaction* cur_rxn);

  //a place holder to put initialization things
  void Initialize();

  //set the gas system pointer to all the reactions
  void set_gas_rxn(REKS_gas * the_gas);
 
  //update the system's information with new gas system;
  void update_sys();
  
  //Set the dll obj for the reduced mechanism;
  void set_red(expl_dll* red) {edll = red; };

  REAL temp_rate();

  REAL temp_rate_v();

  void dump(FILE *output);
  void dump2(FILE *output);

  //Read the Kinetics infomation
  void ReadKinetics(vector<string> files);

 private:
  expl_dll* edll;
  
};

#endif
