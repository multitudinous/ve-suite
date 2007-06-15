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

#include "V21Helper.h"

///////////////////////////////////////////////////////////////////////////////////////

V21Helper::V21Helper (const char* therm_path)  
{
  thermo_database  = new thermo(therm_path);
  steam67_database = new Steam67();
}

///////////////////////////////////////////////////////////////////////////////////////

V21Helper::~V21Helper ()
{
  delete thermo_database;
  delete steam67_database;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::IntToGas (Interface *it, Gas &gs)
{
  unsigned int i;
  bool ok = true;

  gs.clear();

  // Gas
  
  gs.CoalCal = it->getDouble("COALCAL", &ok);
  gs.AshCal  = it->getDouble("ASHCAL",  &ok);
  gs.AshpH   = it->getDouble("ASHPH",   &ok);

  gs.pressure_drop = it->getDouble("PRESSURE_DROP", &ok);
 
  gs.thermo_database = thermo_database;

  // GasCell

  gs.gas_composite.area = it->getDouble("AREA", &ok);
  gs.gas_composite.eff  = it->getDouble("EFF",  &ok);
  gs.gas_composite.eta  = it->getDouble("ETA",  &ok);
  gs.gas_composite.chi  = it->getDouble("CHI",  &ok);

  gs.gas_composite.T = it->getDouble("TEMPERATURE", &ok);
  gs.gas_composite.P = it->getDouble("PRESSURE",    &ok);
  gs.gas_composite.M = it->getDouble("FLOWRATE",    &ok);
  if(!ok) ok = true; // ERROR - MISSING A GAS DEFINITION

  gs.gas_composite.soot = it->getDouble("SOOT", &ok);
  gs.gas_composite.tar  = it->getDouble("TAR", &ok);
  gs.gas_composite.ynu = it->getDouble("YNU", &ok);

  // TEMPORARY COAL
  gs._wic_C         = it->getDouble("WIC_C", &ok);
  gs._wic_H         = it->getDouble("WIC_H", &ok);
  gs._wic_O         = it->getDouble("WIC_O", &ok);
  gs._wic_N         = it->getDouble("WIC_N", &ok);
  gs._wic_S         = it->getDouble("WIC_S", &ok);
  gs._wic_CL        = it->getDouble("WIC_CL", &ok);
  gs._ash_ult       = it->getDouble("ASH_ULT", &ok);
  gs._ash_prox      = it->getDouble("ASH_PROX", &ok);
  gs._proxH2O       = it->getDouble("PROXH2O", &ok);
  gs._proxVM        = it->getDouble("PROXVM", &ok);
  gs._proxFC        = it->getDouble("PROXFC", &ok);
  gs._hhv           = it->getDouble("HHV", &ok);
  gs._comp1         = it->getDouble("COMP1", &ok);
  gs._comp2         = it->getDouble("COMP2", &ok);
  gs._comp3         = it->getDouble("COMP3", &ok);
  gs._comp4         = it->getDouble("COMP4", &ok);
  gs._comp5         = it->getDouble("COMP5", &ok);
  gs._comp6         = it->getDouble("COMP6", &ok);
  gs._comp7         = it->getDouble("COMP7", &ok);
  gs._comp8         = it->getDouble("COMP8", &ok);
  gs._comp9         = it->getDouble("COMP9", &ok);
  gs._comp10        = it->getDouble("COMP10", &ok);
  gs._comp11        = it->getDouble("COMP11", &ok);
  gs._comp12        = it->getDouble("COMP12", &ok);
  gs._coal_feedRate = it->getDouble("COALFEEDRATE", &ok);

  // Particle
  gs.gas_composite.mean_size     = it->getDouble("MEAN_SIZE",     &ok);
  gs.gas_composite.size_variance = it->getDouble("SIZE_VARIANCE", &ok);
  gs.gas_composite.T_particle    = it->getDouble("T_PARTICLE",    &ok);
  gs.gas_composite.M_particle    = it->getDouble("M_PARTICLE",    &ok);

  // Compositions

  std::vector<std::string> comp_name = it->getString1D("COMP_NAME", &ok);
  std::vector<double>      comp_conc = it->getDouble1D("COMP_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO GAS COMPOSITION

  std::vector<std::string> part_name = it->getString1D("PART_NAME", &ok);
  std::vector<double>      part_conc = it->getDouble1D("PART_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO PARTICLE COMPOSITION
  
  std::vector<std::string> wic_name = it->getString1D("WIC_NAME", &ok);
  std::vector<double>      wic_conc = it->getDouble1D("WIC_CONC", &ok);
  if(!ok) ok = true; // ERROR - NO WIC COMPOSITION

  for(i=0; i<comp_name.size(); i++)
    gs.addSpecie(comp_name[i], comp_conc[i]);

  for(i=0; i<part_name.size(); i++)
    gs.addParticle(part_name[i], part_conc[i]);

  for(i=0; i<wic_name.size(); i++)
    gs.addWic(wic_name[i], wic_conc[i]);

  // Done

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::GasToInt (Gas *gs, Interface &it)
{
  it.clear();

  // Gas

  it.setDouble("COALCAL", gs->CoalCal);
  it.setDouble("ASHCAL",  gs->AshCal);
  it.setDouble("ASHPH",   gs->AshpH);

  it.setDouble("PRESSURE_DROP", gs->pressure_drop);

  // GasCell

  it.setDouble("AREA", gs->gas_composite.area);
  it.setDouble("EFF",  gs->gas_composite.eff);
  it.setDouble("ETA",  gs->gas_composite.eta);
  it.setDouble("CHI",  gs->gas_composite.chi);

  it.setDouble("TEMPERATURE", gs->gas_composite.T);
  it.setDouble("PRESSURE",    gs->gas_composite.P);
  it.setDouble("FLOWRATE",    gs->gas_composite.M);

  it.setDouble("SOOT", gs->gas_composite.soot);
  it.setDouble("TAR",  gs->gas_composite.tar);
  it.setDouble("YNU", gs->gas_composite.ynu);

  it.setDouble("MEAN_SIZE",     gs->gas_composite.mean_size);
  it.setDouble("SIZE_VARIANCE", gs->gas_composite.size_variance);
  it.setDouble("T_PARTICLE",    gs->gas_composite.T_particle);
  it.setDouble("M_PARTICLE",    gs->gas_composite.M_particle);

  // Compositions

  std::map<std::string, int>::iterator iter;

  std::vector<std::string> name;
  std::vector<double>      conc;

  for(iter=gs->specie.begin(); iter!=gs->specie.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->gas_composite.comp_specie[iter->second]);
  }
  it.setString1D ("COMP_NAME", name); name.clear();
  it.setDouble1D ("COMP_CONC", conc); conc.clear();

  for(iter=gs->particle.begin(); iter!=gs->particle.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->gas_composite.comp_particle[iter->second]);
  }
  it.setString1D ("PART_NAME", name); name.clear();
  it.setDouble1D ("PART_CONC", conc); conc.clear();

  for(iter=gs->wics.begin(); iter!=gs->wics.end(); iter++) {
    name.push_back(iter->first);
    conc.push_back(gs->comp_wics[iter->second]);
  }
  it.setString1D ("WIC_NAME", name); name.clear();
  it.setDouble1D ("WIC_CONC", conc); conc.clear();

  // TEMPORARY COAL
  it.setDouble("WIC_C"       , gs->_wic_C);
  it.setDouble("WIC_H"       , gs->_wic_H);
  it.setDouble("WIC_O"       , gs->_wic_O);
  it.setDouble("WIC_N"       , gs->_wic_N);
  it.setDouble("WIC_S"       , gs->_wic_S);
  it.setDouble("WIC_CL"      , gs->_wic_CL);
  it.setDouble("ASH_ULT"     , gs->_ash_ult);
  it.setDouble("ASH_PROX"    , gs->_ash_prox);
  it.setDouble("PROXH2O"     , gs->_proxH2O);
  it.setDouble("PROXVM"      , gs->_proxVM);
  it.setDouble("PROXFC"      , gs->_proxFC);
  it.setDouble("HHV"         , gs->_hhv);
  it.setDouble("COMP1"       , gs->_comp1);
  it.setDouble("COMP2"       , gs->_comp2);
  it.setDouble("COMP3"       , gs->_comp3);
  it.setDouble("COMP4"       , gs->_comp4);
  it.setDouble("COMP5"       , gs->_comp5);
  it.setDouble("COMP6"       , gs->_comp6);
  it.setDouble("COMP7"       , gs->_comp7);
  it.setDouble("COMP8"       , gs->_comp8);
  it.setDouble("COMP9"       , gs->_comp9);
  it.setDouble("COMP10"      , gs->_comp10);
  it.setDouble("COMP11"      , gs->_comp11);
  it.setDouble("COMP12"      , gs->_comp12);
  it.setDouble("COALFEEDRATE", gs->_coal_feedRate);

  // Done
   return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::IntToWat (Interface *it, Water &wt)
{
  bool ok = true;
  
  wt.T = it->getDouble("TEMPERATURE", &ok);
  wt.P = it->getDouble("PRESSURE",    &ok);
  wt.H = it->getDouble("ENTHALPY",    &ok);
  wt.Q = it->getDouble("QUALITY",     &ok);
  wt.M = it->getDouble("FLOWRATE",    &ok);

  if(!ok) { /* ERROR - MISSING A WATER DEFINITION */ }

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::WatToInt (Water *wt, Interface &it)
{
  it.clear();

  it.setDouble("TEMPERATURE", wt->T);
  it.setDouble("PRESSURE",    wt->P);
  it.setDouble("ENTHALPY",    wt->H);
  it.setDouble("QUALITY",     wt->Q);
  it.setDouble("FLOWRATE",    wt->M);

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::IntToSum (Interface *it, summary_values &sv)
{
  sv.clear();

  // FILL IN SV - WHEN IS THIS NEEDED ANYWAY?

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////

int V21Helper::SumToInt (summary_values* sv, Interface &it)
{
  unsigned int i;
  std::string desc,val;

  it.clear();

  for(i=0; i<sv->size(); i++)
    it.setString(sv->get_description(i), sv->get_value(i));

  return 0;
}

///////////////////////////////////////////////////////////////////////////////////////
