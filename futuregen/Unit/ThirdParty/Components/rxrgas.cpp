// Gas class for Arsenic partitioning model 7-8/02

#include "rxrgas.h"
#include <V21Helper/Therm/thermo.h>
#include <string>
#include <map>
#include <vector>
#include <cmath>
#include <cstdlib>
#ifndef M_PI
#define M_PI  3.14159265359
#endif

using namespace std;

//**********************************************************************
rxrgas::rxrgas() : thermo_data("therm.dat")
{
  int i = 0;
 
  names = thermo_data.get_nam_spec();
  Rgas = 8.31451;           //             J/mol/K
  nspec = ( thermo_data.get_mwt() ).size();
  for(i=0; i<nspec; i++)
    comp.push_back(0.0);
  T = 298.15;          
  P = 101325.0;        
  Moles = 1;
  // Specifiy mole fractions explicitly
}

//**********************************************************************
rxrgas::rxrgas(Gas& v21gas)
{
	thermo_data = *(v21gas.thermo_database);        
	names = thermo_data.get_nam_spec();
	nspec = thermo_data.get_mwt().size();
	map<string, int>::iterator iter;
	int i;
	comp.resize(nspec,0.0);
	int num = v21gas.gas_composite.comp_specie.size();
	for(i=0; i<nspec; i++){
		iter = v21gas.specie.find(thermo_data.get_spec_nam()[i]);
		if(iter!=v21gas.specie.end())
			comp[i] = v21gas.gas_composite.comp_specie[iter->second];
	}
//	for(iter = v21gas.specie.begin(); iter != v21gas.specie.end(); iter++)
//		comp[names[iter->first]] = v21gas.gas_composite.comp_specie[v21gas.specie[iter->first]];
	Normalize();

	Rgas = 8.31451;
	T = v21gas.gas_composite.T;
	P = v21gas.gas_composite.P;
	Moles = v21gas.gas_composite.moles();
}

//**********************************************************************
void rxrgas::copy(const rxrgas &gaso)
{
	T = gaso.T;
	P = gaso.P;
	Moles = gaso.Moles;
	int i = 0;
	comp  = gaso.comp;
	// thermo member should already be initialized through the constructor
}

//**********************************************************************
void rxrgas::rxrgas_2_v21gas(Gas& v21gas)  
{
	v21gas.gas_composite.T = T;
	v21gas.gas_composite.P = P;
	v21gas.gas_composite.M = Moles * MW() / 1000.0; // kg/s
	map<string, int>::iterator iter;
	for(iter = v21gas.specie.begin(); iter != v21gas.specie.end(); iter++) 
		v21gas.gas_composite.comp_specie[iter->second] = comp[names.find(iter->first)->second];
	v21gas.gas_composite.normalize_specie();
}

//**********************************************************************
void rxrgas::Normalize()
{
  int i = 0;
  double sum = 0;
  for(i=0; i<nspec; i++)
    sum += comp[i];
  for(i=0; i<nspec; i++)
    comp[i]/=sum;
}

//**********************************************************************
double rxrgas::moles(double amount, string species) 
{
  double sum = 0;
  int k = 0;
  map<string, int>::iterator i, j;
  j = names.find(species);
 
  if(amount + comp[j->second] * Moles < 0.0) {
	  cout << "\nERROR REMOVING MORE MOLES OF " << species 
		   << " THAN HAVE IN rxrgas::moles(double,string)\n";
	  return -1;
  }

  if(amount + Moles == 0.0) {                // only of mole frac species == 1
	for(k=0;k<comp.size(); k++)
	  comp[k] = 0.0;
	comp[j->second] = 1.0;
	Moles == 0.0;
	return Moles;
  }

  for (i=names.begin(); i!=names.end(); i++){
    if(i->second != j->second) {
      comp[i->second] = comp[i->second] * Moles / (amount + Moles);
      sum += comp[i->second];
    }
    continue;
  }
  comp[j->second] = 1-sum;
  Moles += amount;  
  return Moles;               // gram-moles
}
/////////////////////////////////////////////////////////////////////////
double rxrgas::moles(string species)
{
	map<string,int>::iterator j;
	j = names.find(species);
	double dummy = comp[j->second];
	return comp[j->second] * Moles;
}

//**********************************************************************
double rxrgas::MW()
{
  double sum = 0;
  map<string, int>::iterator i;
  for(i=names.begin(); i != names.end(); i++) 
    sum += comp[i->second] * thermo_data.mweight(i->first);
  return sum;         // g/mol, not kg/mol
}

//**********************************************************************
double rxrgas::Density()
{
  return MW()/1000.0 * P / Rgas / T; // kg/m3
}

