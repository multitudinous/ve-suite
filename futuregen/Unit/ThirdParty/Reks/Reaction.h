// Reaction.h: interface for the CReaction class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#ifndef REACTION_H
#define REACTION_H

#pragma warning(disable : 4786)

#include <math.h>
#include <map>
#include "GlobalConst.h"
#include "StringParseUtil.h"
#include "SpecieThermo.h"
#include "usr_gas.h"

class Arrhenius{
 public:
  REAL A;
  REAL beta;
  REAL E;
  inline REAL CKABE(REAL T) 
  { 
    //REAL pa1 = ;
    //float pa1= (float) (beta*log(float(T)-E/Rc/T));
    REAL result = A*exp(beta*log(T)-E/Rc/T);
    //REAL result = A*exp((float)pa1);
    
    return result;
  };
};

typedef struct Troe_param {
	REAL a;
	REAL T3star;
	REAL Tstar;
	REAL T2star;
} TTroe_param; 

typedef struct SRI_param {
	REAL a; REAL b; REAL c; REAL d; REAL e;
} TSRI_param;

typedef struct LT_param {
	REAL Bi;
	REAL Ci;
} TLT_param;

typedef struct Spec_order_param{
	REKS_specie_thermo* spec;
	REAL vki1;
	REAL vki2; 
} TSpec_order_param;


typedef map<string, REAL> THIRDBODY_EFF;

#define THIRD_BODY 0x00000001
#define THIRD_BODY_SPEC 0x00000010
#define PRES_DEP 0x00000100
#define PHOTON_RAD 0x00010000
#define ELECTRON 0x01000000

class REKS_reaction  
{
 public:
  REKS_reaction();
  REKS_reaction(REKS_reaction* dup);
  virtual ~REKS_reaction();
  unsigned int reaction_flag;
  int type;                 
  // Reaction type.
  
  //PRIMARY DATA FOR REACTIONS
  
  /** 
   * list of species that participate as reactants,
   * and their stoichiometric coefficients
   */
  vector<REKS_specie_thermo *> reactants;
  vector<REAL> reactant_effs;    
  /** 
   * list of species that participate as products,
   * and their stoichiometric coefficients
   */
  vector<REKS_specie_thermo *> products;
  vector<REAL> product_effs;    
  
  
  Arrhenius arrhe_params;
  
  
  //AUXILARY DATA FOR REACTIONS	
  
  /**
   * map from species names to enhanced third-body collision efficiencies
   */
  
  //Thirdbody efficienceis
  bool is_three_body;      // True if reaction is a three-body reaction.
    
  THIRDBODY_EFF thirdbodies;
  vector<REKS_specie_thermo* > third_specs;  
  bool is_third_spec;

  //Presure Dependent Reaction Parameters
  bool is_fall_of;        // True if reaction is a falloff reaction.
  bool is_chem_act;        // True if reaction is a chemical activation reaction.
	
  Arrhenius fall_of;
  Arrhenius chem_act;
  
  //Lindemann formulation
  bool is_Lindemann; // no additional parameters are needed
  
  //Troe Pressure-dependent reaction,
  bool is_Troe; //a, T***, T*, T**
  
  TTroe_param troe_params;
  
  bool is_SRI; //a, b, c, d, e;
  
  TSRI_param sri_params;
  
  bool is_LandauTeller; //Bi, Ci
  
  TLT_param lt_params;
  
  bool if_spec_Eq79; // 9 parametess
  REAL eq79_bn[9];
  
  bool if_spec_Eq80; //4 parameters
  REAL eq80_bn[4];
  
  bool if_spec_HV; //specified wave_length
  REAL wave_length;
  
  bool if_spec_temp_dep;
  vector <string> temp_dep_specs;
  
  bool if_spec_energy_loss;
  REAL e_loss;
  
  bool is_MOME;
  bool is_XSMI;
  
  bool if_spec_reverse;        // True if reaction is reversible.
  Arrhenius reverse_params; 


  bool if_spec_order_forward;
  vector<TSpec_order_param> forward_order;
  inline REAL getVk1(REKS_specie_thermo *spec)
    {
      int i;
      for (i=0; i<order.size(); i++)
	if (order[i].spec==spec)
	  return order[i].vki1;
      return 0;
    }

  inline REAL getVk2(REKS_specie_thermo *spec)
    {
      int i;
      for (i=0; i<order.size(); i++)
	if (order[i].spec==spec)
	  return order[i].vki2;
      return 0;
    }
  
  bool if_spec_order_reverse;
  vector<TSpec_order_param> reverse_order;
  
  
  bool if_spec_rxn_units;
  vector<string> units;
  
  bool is_duplicate;         // True if reaction is declared to be a duplicate;

  /**
   * reaction number this one is a duplicate to (declared or not). 
   * If the reaction is not a duplicate, the value is zero.
   */
  int duplicate;
  // Reaction number.
  int rxn_number; //Just generate a number, started for 0


  //Processed data
  
  map<REKS_specie_thermo *, REAL> REKS_specs;    
  map<REKS_specie_thermo *, REAL> REKS_specs_v1;
  map<REKS_specie_thermo *, REAL> REKS_specs_v2; 
  vector<TSpec_order_param> order;

  REKS_gas * mygas;  // the gas system
  REAL Kpi; //equilibrium constant
  REAL rev_rate; //reverse rate;
  REAL for_rate; //forward rate;
  bool Fstoichiometric;

  // check if the Unit is one of the following
  // MOLE(CULE)
  // "CAL"
  // "KCAL"
  // "JOUL"
  // "KJOUL"
  // "KELV(IN)"
  // "EVOL(TS)"
  bool is_legal_unit(string);

  bool operator==(const REKS_reaction& rxn) const;
  void dump();
  
  //check if elements balance
  bool balance_check();
  
  //fill up the vector for name and vk, v1 and v2
  //this routin put the reactants and products together and get the vki
  void stoichiometric(); 

  //compute equilibrium const for reaction rate
  REAL equilibrium_const();

  //compute reverse rate from the reverse Arrhenius args
  //Or from the equilibrium_const and forword rate
  REAL reverse_rate();

  //compute forward rate according to different flags
  REAL forward_rate();

  //set the gas system for the reaction
  //And this call stoichiometric, equilibrum_const and reverse_rate
  void set_gas(REKS_gas* the_gas);
  //update the reaction rates according to new gas information
  void update();

  //check if the specie is already in the reactants vector, handle reaction like CH2 + CH2=>C+CH4
  int already_in_rct(REKS_specie_thermo * rct);
  //check if the specie is already in the product vector //hand reaction like O2= O + O
  int already_in_pdt(REKS_specie_thermo * pdt);
};

#endif 

