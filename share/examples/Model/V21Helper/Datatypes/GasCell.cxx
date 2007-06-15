/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
/*
 * GasCell.cc
 *
 * Mike Maguire
 * Reaction Engineering Intl
 * April 2001
 */

#include "GasCell.h"
#include "Gas.h"
#include <map>
#include <cmath>
#include <iomanip>
#include <iostream>

///////////////////////////////////////////////////////////////////////////////////////

GasCell::GasCell (Gas *parent)
{
  gas_parent = parent;
}

///////////////////////////////////////////////////////////////////////////////////////

GasCell::~GasCell ()
{
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::zero ()
{
  int i;
  velocity.clear();
  for(i=0; i<3; i++)
    velocity.push_back(0);
  node_location.clear();
  for(i=0; i<3; i++)
    node_location.push_back(0);
  icell.clear();
  for(i=0; i<3; i++)
    icell.push_back(0);
  area = 0;
  eff = 0;
  eta = 0;
  chi = 0;
  T = 0;
  P = 0;
  M = 0;
  soot = 0;
  tar = 0;
  ynu = 0;
  mean_size = 0;
  size_variance = 0;
  T_particle = 0;
  M_particle = 0;
  for(i=0; i<comp_specie.size(); i++)
    comp_specie[i] = 0;
  for(i=0; i<comp_particle.size(); i++)
    comp_particle[i] = 0;
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::copy (const GasCell& p) 
{
  // check for self assign.
  if(this==&p)
    return;

  unsigned int i;
  
  icell.clear();
  icell = p.icell;
  //for(i=0; i<p.icell.size(); i++)
  //  icell.push_back(p.icell[i]);

  velocity.clear();
  velocity = p.velocity;
  //for(i=0; i<p.velocity.size(); i++) 
  //  velocity.push_back(p.velocity[i]);

  node_location.clear();
  node_location = p.node_location;
  //for(i=0; i<p.node_location.size(); i++) 
  //  node_location.push_back(p.node_location[i]);

  comp_specie.clear();
  comp_specie = p.comp_specie;
  //for(i=0; i<p.comp_specie.size(); i++) 
  //  comp_specie.push_back(p.comp_specie[i]);

  comp_particle.clear();
  comp_particle = p.comp_particle;
  //for(i=0; i<p.comp_particle.size(); i++) 
  //  comp_particle.push_back(p.comp_particle[i]);

  area = p.area;
 
  eff = p.eff;
  eta = p.eta;
  chi = p.chi;
  T = p.T;
  P = p.P;
  M = p.M;
  soot = p.soot;
  tar = p.tar;
  ynu = p.ynu;
  mean_size = p.mean_size;
  size_variance = p.size_variance;
  T_particle = p.T_particle;
  M_particle = p.M_particle;
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::mw ()
{
  std::map< std::string, int >::iterator map_iter;
  double mwt = 0;
  int i = 0;
  // DOL CHANGED;  DON'T USE int i IN comp_specie[i]: USE AS SHOWN, ESLE INDEXING IS OFF.
  for(map_iter=gas_parent->specie.begin(); map_iter!=gas_parent->specie.end(); map_iter++) {
    mwt += gas_parent->thermo_database->mweight(map_iter->first) * comp_specie[map_iter->second];
    i++;
  }
  return mwt;
 
 
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::density ()
{
  return P / (8314 * T) * mw();
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::enthalpy (double T1)
{
  if(T1==0) T1 = T;

  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);

  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions (this again will be replaced by scirun code)
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::enthalpy: species " << iter2->first 
	     << " not found - critical error!!!" << std::endl;
      
  }
  return gas_parent->thermo_database->enthalpy_mix(composition, T1);
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::entropy ()
{
  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);

  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::entropy: species " << iter2->first 
	     << " not found - critical error!!!" << std::endl;
      
  }
  return gas_parent->thermo_database->entropy_mix(composition, T, P);
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::cp_mix (double T1)
{
  if(T1==0) T1 = T;

  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);
  
  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::cp_mix: species " << iter2->first 
	     << " not found - critical error!!!" << std::endl;
      
  }
  return gas_parent->thermo_database->cp_mix(composition, T1) / mw();
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::thermal_conduct (double T1)
{
  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);
  
  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::thermal_conduct: species " << iter2->first 
	     << " not found - critical error!!!" << std::endl;
      
  }
  return gas_parent->thermo_database->thermal_cond_mix(1, T1, T1, composition);
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::getFrac (std::string specie)
{
  std::map< std::string, int >::iterator iter;

  iter = gas_parent->specie.find(specie);
  
  if(iter==gas_parent->specie.end())
    return -1.0;

  return(comp_specie[iter->second]);
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::setFrac (std::string specie, double conc)
{
  std::map< std::string, int >::iterator iter;
  
  //  std::cout<<"setFrac cp1 "<<std::endl; fflush(NULL);
  iter = gas_parent->specie.find(specie);
  //  std::cout<<"setFrac cp2 "<<std::endl; fflush(NULL);
  if(iter==gas_parent->specie.end()) {
    //  std::cout<<"setFrac cp3 "<<std::endl; fflush(NULL);
  gas_parent->addSpecie(specie);
  //  std::cout<<"setFrac cp4 "<<std::endl; fflush(NULL);
  iter = gas_parent->specie.find(specie);
  //  std::cout<<"setFrac cp5 "<<std::endl; fflush(NULL);
  }

  comp_specie[iter->second] = conc;
  //  std::cout<<"setFrac cp6 "<<std::endl; fflush(NULL);
	
  return comp_specie[iter->second];
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::getPFrac (std::string specie)
{
  std::map< std::string, int >::iterator iter;

  iter = gas_parent->particle.find(specie);
  
  if(iter==gas_parent->particle.end())
    return -1.0;

  return(comp_particle[iter->second]);
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::setPFrac (std::string specie, double conc)
{
  std::map< std::string, int >::iterator iter;

  iter = gas_parent->particle.find(specie);
  
  if(iter==gas_parent->particle.end())
    return -1.0;

  comp_particle[iter->second] = conc;
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::normalize_specie () {
  double total=0;
  int i;
 
  for(i=0; i<comp_specie.size(); i++)
    total += comp_specie[i];
  for(i=0; i<comp_specie.size(); i++)
    comp_specie[i] = comp_specie[i] / total;
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::balance_co2 () {
  int i, co2, o2, co, counter;
  double logK, k_sq, total_kmols;
  std::vector<double> table_logK, table_T;
    
  table_logK.push_back(-103.762); table_T.push_back(298);
  table_logK.push_back(-57.616); table_T.push_back(500);
  table_logK.push_back(-23.529); table_T.push_back(1000);
  table_logK.push_back(-17.871); table_T.push_back(1200);
  table_logK.push_back(-13.842); table_T.push_back(1400);
  table_logK.push_back(-10.83); table_T.push_back(1600);
  table_logK.push_back(-8.497); table_T.push_back(1800);
  table_logK.push_back(-6.635); table_T.push_back(2000);
  table_logK.push_back(-5.12); table_T.push_back(2200);
  table_logK.push_back(-3.86); table_T.push_back(2400);
  table_logK.push_back(-2.801); table_T.push_back(2600);
  table_logK.push_back(-1.894); table_T.push_back(2800);
  table_logK.push_back(-1.111); table_T.push_back(3000);
  table_logK.push_back(-0.429); table_T.push_back(3200);
  table_logK.push_back(0.169); table_T.push_back(3400);
  table_logK.push_back(0.701); table_T.push_back(3600);
  table_logK.push_back(1.176); table_T.push_back(3800);
  table_logK.push_back(1.599); table_T.push_back(4000);
  table_logK.push_back(2.49); table_T.push_back(4500);
  table_logK.push_back(3.197); table_T.push_back(5000);
  table_logK.push_back(3.771); table_T.push_back(5500);
  table_logK.push_back(4.245); table_T.push_back(6000);

  if(T < table_T[0]) {
    std::cerr << "In GasCell::balance_co2, T below table bounds, using lowest table value.\n";
    logK = table_logK[0];
  }
  else if(T >= table_T[table_T.size()-1]) {
    std::cerr << "In GasCell::balance_co2, T above table bounds, using highest table value.\n";
    logK = table_logK[table_logK.size()-1];
  }
  else {
    i=0;
    while(!((T >= table_T[i]) && (T < table_T[i+1])))
	  i++;
    logK = (T - table_T[i]) / (table_T[i+1] - table_T[i])
      * (table_logK[i+1] - table_logK[i]) + table_logK[i];
  }
  k_sq = pow(exp(logK), 2);

  normalize_specie();  
    
  std::map<std::string, int>::iterator map_iter;
  double mwt = mw();
  i = 0;
  total_kmols = 0;
  
  map_iter = gas_parent->specie.find("CO2");
  if(map_iter==gas_parent->specie.end()) {
    std::cerr << "GasCell::balance_co2: No CO2 in stream.\n";
    return;
  } else {
    co2 = map_iter->second;
  }
  map_iter = gas_parent->specie.find("CO");
  if(map_iter==gas_parent->specie.end()) {
    std::cerr << "GasCell::balance_co2: No CO in stream.\n";
    return;
  } else {
    co = map_iter->second;
  }
  map_iter = gas_parent->specie.find("O2");
  if(map_iter==gas_parent->specie.end()) {
    std::cerr << "GasCell::balance_co2: No O2 in stream.\n";
    return;
  } else {
    o2 = map_iter->second;
  }

  for(i=0; i<comp_specie.size(); i++) {
    comp_specie[i] = comp_specie[i] / mwt;
    total_kmols += comp_specie[i];
  }

  double x=0, f=9999, f_prime;
  counter = 0;
  while((f > 1.0e-20) || (f < -1.0e-20) && counter < 500) {
    f = k_sq * (total_kmols + x/2) * pow((comp_specie[co2] - x), 2) 
      - pow((comp_specie[co] + x), 2) * (comp_specie[o2] + x/2);
    f_prime = k_sq/2 * pow((comp_specie[co2] - x), 2)
      - 2 * k_sq * (total_kmols + x/2) * (comp_specie[co2] - x)
      - 2 * (comp_specie[co] + x) * (comp_specie[o2] + x/2)
      - pow((comp_specie[co] + x), 2) / 2;
    x = x - f / f_prime;
  }
  
  if(counter>=500) {
    std::cerr << "GasCell::balance_co2 did not converge\n";
    return;
  }

  comp_specie[co2] = comp_specie[co2] - x;
  comp_specie[co] = comp_specie[co] + x;
  comp_specie[o2] = comp_specie[o2] + x/2;

  // comp_specie back to mol-frac.
  double sum=0;
  for(i=0; i<comp_specie.size(); i++)
    sum += comp_specie[i];
  for(i=0; i<comp_specie.size(); i++)
    comp_specie[i] = comp_specie[i] / sum;

  normalize_specie();
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::moles ()
{
  return M / (mw()/1000.0);   
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::moles (double change_by, std::string component) 
{
  // function used to change the # of moles of the gas phase.
  // works in terms of mole fractions and total mass
  // mole fractions are updated.
  // mass is changed.

  double sum = 0;
  double nmoles = moles();
  /*
  for(int ii=0; ii < comp_specie.size(); ii++) 
    if(ii != gas_parent->specie[component]) {
      comp_specie[ii] = comp_specie[ii] * nmoles / 
	(change_by + nmoles);
      sum += comp_specie[ii];
    }
  cout << endl << "sum in GasCell::moles is " << sum;
  comp_specie[gas_parent->specie[component]] = 1.0-sum;
  cout << endl <<  "MW OF COMPONENT = " << gas_parent->thermo_database->mweight(component);
  cout << endl << " change by in GasCell::moles = " << change_by;
  M += change_by * gas_parent->thermo_database->mweight(component)/1000.0;
  return M;

  */

  std::map< std::string, int>::iterator iter, iter2;
 // for(iter=gas_parent->specie.begin(); iter != gas_parent->specie.end(); iter++)
  //  cout << "\n species: " << iter->first << " " << comp_specie[iter->second];
  iter2 = gas_parent->specie.find(component);
  if(iter2==gas_parent->specie.end()) {
    std::cout << component << " not found" << std::endl;
    return -1.0;
  }
  for(iter=gas_parent->specie.begin();iter != gas_parent->specie.end(); iter++)
    if(iter != iter2) {
      comp_specie[iter->second] = comp_specie[iter->second] * nmoles / 
	(change_by + nmoles);
      sum += comp_specie[iter->second];
    }
  //cout << "sum is " << sum << endl;
  //cout << "change by is " << change_by << endl;
  comp_specie[iter2->second] = 1.0-sum; // fix
  M += change_by * gas_parent->thermo_database->mweight(component)/1000.0;
  
  //for(iter=gas_parent->specie.begin(); iter != gas_parent->specie.end(); iter++)
  //  cout << "species: " << iter->first << " " << comp_specie[iter->second] << endl;
  return M;
  

}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::moles (std::string component) 
{
  std::map<std::string,int>::iterator it;
  it = gas_parent->specie.find(component);
  if(it==gas_parent->specie.end()){
     std::cout << component << " not found" << std::endl;
     return(-1.0);
  }
  return moles()*comp_specie[it->second];
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::Qvol () 
{
  return moles()*8.31451*T/P;  // m3 per time if any
}

///////////////////////////////////////////////////////////////////////////////////////

double GasCell::Visc  ()
{
  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);

  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions (this again will be replaced by scirun code)
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::Visc: species " << iter2->first
	     << " not found - critical error!!!" << std::endl;
      
  }

  return gas_parent->thermo_database->viscosity_mix(T, T, composition);
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::Property_Output  ()
{
  std::cout << "\nGas Properties: "
       << "\n  Temperature (F) " << (T-273.15)*1.8+32
       << "\n  Pressure (psia) " << P/6894.75729
       << "\n  Mass (kg/s)     " << M
       << "\n  Moles (mol/s)   " << moles()
       << "\n  Mole Fractions: " << std::endl;
  std::map< std::string, int >::iterator map_iter;
  for(map_iter = gas_parent->specie.begin(); map_iter!=gas_parent->specie.end(); map_iter++)
    std::cout << std::setw(10) << (map_iter->first).c_str() << " " << comp_specie[map_iter->second] << std::endl;
}

///////////////////////////////////////////////////////////////////////////////////////

bool GasCell::equilb ()
{
  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition and massf
  std::vector<REAL> composition;
  std::vector<REAL> massf;

  for(i=0;i<nspecies;i++) {
    composition.push_back(0.0);
    massf.push_back(0.0);
  }

  // Fill in composition and begin conversion to mass fractions for massf
  double mmw = 0.0;
  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
    if(iter!=name_map.end()) {
	// now manually set mole fractions 
      composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      massf[(*iter).second] = composition[(*iter).second] *
	gas_parent->thermo_database->mweight(iter2->first);
      mmw += massf[(*iter).second];
    }
    else { 
      std::cout << "GasCell::equilb: species " << iter2->first 
	   << " not found - critical error!!!" << std::endl;
      return false;
    }
  }

  // finish conversion to mass fraction
  for(i=0; i<nspecies; i++){
    massf[i] /= mmw;
    composition[i] = massf[i]/gas_parent->thermo_database->get_mwt()[i];
  }

  // Perform equilibrium calculation
  gas_parent->thermo_database->
    equilb(false, T, P, gas_parent->thermo_database->enthalpy_mix(composition, T),
	   massf, composition);

  // Convert to mole fractions
  for(i=0, mmw=0.0; i<nspecies; i++) mmw += composition[i];
  mmw = 1./mmw;
  for(i=0; i<nspecies; i++) composition[i] *= mmw;
  
  // Replace composition
  for(iter=name_map.begin(); iter!=name_map.end(); iter++) {
    // if gas doesn't contain non-zero specie from equilb, then add it.
    if(gas_parent->specie.find(iter->first)==gas_parent->specie.end()) {
      if(composition[iter->second] > 1.0e-08) {
	gas_parent->addSpecie(iter->first);
	gas_parent->gas_composite.comp_specie[gas_parent->specie[iter->first]]
	  = composition[iter->second];
      }
    }
    else 
      gas_parent->gas_composite.comp_specie[gas_parent->specie[iter->first]]
	= composition[iter->second];
  }

  // Erase gas_cells
  gas_parent->gas_cell.clear();
}

///////////////////////////////////////////////////////////////////////////////////////

void GasCell::find_temperature (REAL &t, REAL &c, REAL &e)
{
  if(t==0) t = T;
  if(c==0) c = cp_mix();
  if(e==0) e = enthalpy();

  std::map< std::string, int >::const_iterator iter, iter2;
  int i;
  
  // grab map for species names
  const std::map<std::string, int>& name_map = 
    gas_parent->thermo_database->get_nam_spec();
	
  // how many species are in the thermo file?
  int nspecies = gas_parent->thermo_database->get_spec_nam().size();
	
  // initialize composition
  std::vector<REAL> composition;
  for(i=0;i<nspecies;i++)
    composition.push_back(0.0);

  for(iter2=gas_parent->specie.begin(); iter2!=gas_parent->specie.end(); iter2++) {
    iter = name_map.find(iter2->first);
      
      if(iter!=name_map.end())
	// now manually set mole fractions (this again will be replaced by scirun code)
	composition[(*iter).second] = gas_parent->gas_composite.comp_specie[iter2->second];
      else 
	std::cout << "GasCell::Visc: species " << iter2->first
	     << " not found - critical error!!!" << std::endl;
  }

  gas_parent->thermo_database->find_temperature(t, c, e, composition);
}

///////////////////////////////////////////////////////////////////////////////////////

bool GasCell::dataOut (FILE *stream)
{
  int i;

  // Scalars
  fprintf(stream, "%12.4E \n", area);
  fprintf(stream, "%12.4E \n", eff);
  fprintf(stream, "%12.4E \n", eta);
  fprintf(stream, "%12.4E \n", chi);
  fprintf(stream, "%12.4E \n", T);
  fprintf(stream, "%12.4E \n", P);
  fprintf(stream, "%12.4E \n", M);
  fprintf(stream, "%12.4E \n", soot);
  fprintf(stream, "%12.4E \n", tar);
  fprintf(stream, "%12.4E \n", ynu);
  fprintf(stream, "%12.4E \n", mean_size);
  fprintf(stream, "%12.4E \n", size_variance);
  fprintf(stream, "%12.4E \n", T_particle);
  fprintf(stream, "%12.4E \n", M_particle);
  
  // icell
  fprintf(stream, "%d %d %d \n",
	  icell[0], icell[1], icell[2]);

  // velocity
  fprintf(stream, "%12.4E %12.4E %12.4E \n",
	  velocity[0], velocity[1], velocity[2]);

  // node_location
  fprintf(stream, "%12.4E %12.4E %12.4E \n",
	  node_location[0], node_location[1], node_location[2]);
  
  // Sizes
  fprintf(stream, "%d %d \n", comp_specie.size(), comp_particle.size());

  // Gas composition
  for(i=0; i<comp_specie.size(); i++)
    fprintf(stream, "%12.4E \n", comp_specie[i]);
  
  // Particle composition
  for(i=0; i<comp_particle.size(); i++)
    fprintf(stream, "%12.4E \n", comp_particle[i]);

  return true;
}

///////////////////////////////////////////////////////////////////////////////////////

bool GasCell::dataIn (FILE *stream)
{
  int i;
  int comp_specie_size;
  int comp_particle_size;

  // Scalars
  fscanf(stream, "%f \n", area);
  fscanf(stream, "%f \n", eff);
  fscanf(stream, "%f \n", eta);
  fscanf(stream, "%f \n", chi);
  fscanf(stream, "%f \n", T);
  fscanf(stream, "%f \n", P);
  fscanf(stream, "%f \n", M);
  fscanf(stream, "%f \n", soot);
  fscanf(stream, "%f \n", tar);
  fscanf(stream, "%f \n", ynu);
  fscanf(stream, "%f \n", mean_size);
  fscanf(stream, "%f \n", size_variance);
  fscanf(stream, "%f \n", T_particle);
  fscanf(stream, "%f \n", M_particle);
  
  // icell
  icell.resize(3);
  fscanf(stream, "%d %d %d \n",
	  icell[0], icell[1], icell[2]);

  // velocity
  velocity.resize(3);
  fscanf(stream, "%f %f %f \n",
	  velocity[0], velocity[1], velocity[2]);

  // node_location
  node_location.resize(3);
  fscanf(stream, "%f %f %f \n",
	  node_location[0], node_location[1], node_location[2]);
 
  // Sizes
  fscanf(stream, "%d %d \n", comp_specie_size, comp_particle_size);

  // Gas composition
  comp_specie.resize(comp_specie_size);
  for(i=0; i<comp_specie_size; i++)
    fscanf(stream, "%f \n", comp_specie[i]);
  
  // Particle composition
  comp_particle.resize(comp_particle_size);
  for(i=0; i<comp_particle_size; i++)
    fscanf(stream, "%f \n", comp_particle[i]);

  return true;
}

///////////////////////////////////////////////////////////////////////////////////////
