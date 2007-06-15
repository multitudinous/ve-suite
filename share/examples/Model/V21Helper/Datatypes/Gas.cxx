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

#include "Gas.h"
#include <Therm/thermo.h>

///////////////////////////////////////////////////////////////////////////////////////

Gas::Gas ()
  :gas_composite(this)
{

}

///////////////////////////////////////////////////////////////////////////////////////

Gas::Gas (const Gas& p)
  :gas_composite(this)
{
  copy(p);
}

///////////////////////////////////////////////////////////////////////////////////////

Gas::~Gas ()
{ 

}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::clear ()
{
  specie.clear();
  particle.clear();
  wics.clear();
  comp_wics.clear();
  hh0.clear();
  gas_cell.clear();

}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::copy (const Gas& p)
{
  if(this==&p)
    return;

  specie.clear();
  specie = p.specie;
  
  particle.clear();
  particle = p.particle;
  
  wics.clear();
  wics = p.wics;
  
  comp_wics.clear();
  comp_wics = p.comp_wics;
  
  hh0.clear();
  hh0 = p.hh0;
 
  gas_composite.copy(p.gas_composite);
  
  gas_cell.clear();

  CoalCal = p.CoalCal;
  AshCal = p.AshCal;
  AshpH = p.AshpH;
  
  _wic_C = p._wic_C;
  _wic_H = p._wic_H;
  _wic_O = p._wic_O;
  _wic_N = p._wic_N;
  _wic_S = p._wic_S;
  _wic_CL = p._wic_CL;
  _ash_ult = p._ash_ult;
  _ash_prox = p._ash_prox;
  _proxH2O = p._proxH2O;
  _proxVM = p._proxVM;
  _proxFC = p._proxFC;
  _hhv = p._hhv;
  _comp1 = p._comp1;
  _comp2 = p._comp2;
  _comp3 = p._comp3;
  _comp4 = p._comp4;
  _comp5 = p._comp5;
  _comp6 = p._comp6;
  _comp7 = p._comp7;
  _comp8 = p._comp8;
  _comp9 = p._comp9;
  _comp10 = p._comp10;
  _comp11 = p._comp11;
  _comp12 = p._comp12;
  _coal_feedRate = p._coal_feedRate;

  pressure_drop = p.pressure_drop;

  thermo_database = p.thermo_database;
}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::average ()
{
  std::vector<GasCell>::iterator cell;
  int i, num_cells = gas_cell.size();;
  double kmol_s = 0.0;
  double mw = 0.0;

  gas_composite.zero();
 
  gas_composite.comp_specie.clear();
  for(i=0; i<specie.size(); i++)
    gas_composite.comp_specie.push_back(0);

  gas_composite.comp_particle.clear();
  for(i=0; i<particle.size(); i++)
    gas_composite.comp_particle.push_back(0);

  gas_composite.velocity.clear();
  gas_composite.node_location.clear();
  for(i=0; i<3; i++) {
    gas_composite.velocity.push_back(0);
    gas_composite.node_location.push_back(0);
  }

  for(cell = gas_cell.begin(); cell != gas_cell.end(); cell++) {
    kmol_s = 0.0;

    mw = (*cell).mw();
    for(i=0; i<(*cell).comp_specie.size(); i++) {
      gas_composite.comp_specie[i] = gas_composite.comp_specie[i] +
	((*cell).comp_specie[i] * (*cell).M) / mw;
      kmol_s = kmol_s + gas_composite.comp_specie[i];
    }
 
    //gas_composite.density += (*cell).M/(*cell).density;
    gas_composite.area += (*cell).area;
    // convert mixture fractions to mass fractions and sum
    gas_composite.eff += (*cell).eff/((1.-(*cell).eta)*(1.-(*cell).chi))*(*cell).M;
    gas_composite.eta += (*cell).eta/(1.-(*cell).chi)*(*cell).M;
    gas_composite.chi += (*cell).chi*(*cell).M;
    gas_composite.T += (*cell).T*(*cell).M;
    gas_composite.P += (*cell).P*(*cell).area;
    //gas_composite.H += (*cell).H*(*cell).M;
    gas_composite.M += (*cell).M;
    gas_composite.soot += (*cell).soot*(*cell).M;
    gas_composite.tar += (*cell).tar*(*cell).M;
    gas_composite.ynu += (*cell).ynu*(*cell).M;
    gas_composite.mean_size += (*cell).mean_size*(*cell).M_particle;
    gas_composite.size_variance += (*cell).size_variance*(*cell).M_particle;
    gas_composite.T_particle += (*cell).T_particle*(*cell).M_particle;
    gas_composite.M_particle += (*cell).M_particle;

    for(i=0; i<(*cell).comp_particle.size(); i++)
      gas_composite.comp_particle[i] += (*cell).comp_particle[i];

    for(i=0; i<3; i++) {
      gas_composite.velocity[i] += (*cell).velocity[i];
      gas_composite.node_location[i] += (*cell).node_location[i];
    }
  }
  
  for(i=0; i<gas_composite.comp_specie.size(); i++)
    gas_composite.comp_specie[i] = gas_composite.comp_specie[i] / kmol_s;

  //gas_composite.density = gas_composite.density / gas_composite.M;
  //gas_composite.density = 1./gas_composite.density;
  
  gas_composite.eff = gas_composite.eff / gas_composite.M;
  gas_composite.eta = gas_composite.eta / gas_composite.M;
  gas_composite.chi = gas_composite.chi / gas_composite.M;
  // convert back to mixture fractions
  gas_composite.eta = gas_composite.eta / (1.-gas_composite.chi);
  gas_composite.eff = gas_composite.eff / (1.-gas_composite.eta)*(1.-gas_composite.chi);
  gas_composite.T = gas_composite.T / gas_composite.M;
  gas_composite.P = gas_composite.P / gas_composite.area;
  //gas_composite.H = gas_composite.H() / gas_composite.M;
  // Check on tar, soot------------------------------------------------------------
  gas_composite.soot = gas_composite.soot / gas_composite.M;
  gas_composite.tar = gas_composite.tar / gas_composite.M;
  gas_composite.ynu = gas_composite.ynu / gas_composite.M;
  gas_composite.mean_size = gas_composite.mean_size / gas_composite.M_particle;
  gas_composite.size_variance = gas_composite.size_variance / gas_composite.M_particle;
  gas_composite.T_particle = gas_composite.T_particle / gas_composite.M_particle;
 
  for(i=0; i<gas_composite.comp_particle.size(); i++) 
    gas_composite.comp_particle[i] = gas_composite.comp_particle[i]
      / gas_composite.M_particle;
  
  gas_composite.velocity[0] = gas_composite.M/(gas_composite.density()*gas_composite.area);
  for(i=1; i<3; i++) 
    gas_composite.velocity[i] = 0.0;
  for(i=0; i<3;i++)
    gas_composite.node_location[i] = gas_composite.node_location[i] / num_cells;
}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::addSpecie  (std::string spec, double value)
{
  std::map< std::string, int >::iterator iter;
  std::vector<GasCell>::iterator cell;

  if(specie.find(spec)==specie.end()) {
    int sz = specie.size();
    specie[spec] = sz;
    for(cell = gas_cell.begin(); cell != gas_cell.end(); cell++) 
      cell->comp_specie.push_back(value);
    gas_composite.comp_specie.push_back(value);    
  }
}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::addParticle (std::string spec, double value)
{
  std::map< std::string, int >::iterator iter;
  std::vector<GasCell>::iterator cell;

  if(particle.find(spec)==particle.end()) {
    int sz = particle.size();
    particle[spec] = sz;
    for(cell = gas_cell.begin(); cell != gas_cell.end(); cell++) 
      cell->comp_particle.push_back(value);
    gas_composite.comp_particle.push_back(value);    
  }
}

///////////////////////////////////////////////////////////////////////////////////////

void Gas::addWic (std::string spec, double value)
{
  std::map< std::string, int >::iterator iter;
  std::vector<GasCell>::iterator cell;

  if(wics.find(spec)==wics.end()) {
    int sz = wics.size();
    wics[spec] = sz;
    comp_wics.push_back(value);    
  }
}

///////////////////////////////////////////////////////////////////////////////////////

bool Gas::dataOut (FILE *stream)
{
  int i;
  std::map<std::string, int>::iterator iter;
   
  // Scalars
  fprintf(stream, "%12.4E \n", CoalCal);
  fprintf(stream, "%12.4E \n", AshCal);
  fprintf(stream, "%12.4E \n", AshpH);
  fprintf(stream, "%12.4E \n", pressure_drop); 

  // Sizes
  fprintf(stream, "%d %d %d %d %d \n",
	  specie.size(),
	  particle.size(),
	  wics.size(),
	  hh0.size(),
	  gas_cell.size());

  // Gas composition
  for(iter=specie.begin(); iter!=specie.end(); iter++)
    fprintf(stream, "%s %d \n",
	    iter->first.c_str(),
	    iter->second);

  // Particle composition
  for(iter=particle.begin(); iter!=particle.end(); iter++)
    fprintf(stream, "%s %d \n",
	    iter->first.c_str(),
	    iter->second);

  // Wics
  for(iter=wics.begin(); iter!=wics.end(); iter++)
    fprintf(stream, "%s %d \n",
	    iter->first.c_str(),
	    iter->second);

  // HH0
  for(i=0; i<hh0.size(); i++)
    fprintf(stream, "%12.4E \n", hh0[i]);

  // Gas composite
  if(!gas_composite.dataOut(stream)) {
    fclose(stream);
    return false;
  }

  // GasCells
  for(i=0; i<gas_cell.size(); i++)
    if(!gas_cell[i].dataOut(stream)) {
      fclose(stream);
      return false;
    }
  
  fclose(stream);

  return true;
}

///////////////////////////////////////////////////////////////////////////////////////

bool Gas::dataIn (FILE *stream)
{
  int i;
  int specie_size, particle_size, wics_size, hh0_size, gas_cell_size;

  char sbuf[12];
  double dbuf;
  int ibuf;

  // Scalars
  fscanf(stream, "%f \n", CoalCal);
  fscanf(stream, "%f \n", AshCal);
  fscanf(stream, "%f \n", AshpH);
  fscanf(stream, "%f \n", pressure_drop); 

  // Sizes
  fprintf(stream, "%d %d %d %d %d \n",
	  specie_size,
	  particle_size,
	  wics_size,
	  hh0_size,
	  gas_cell_size);

  // Gas composition
  specie.clear();
  for(i=0; i<specie_size; i++) {
    fscanf(stream, "%s %d \n", sbuf, ibuf);
    specie[sbuf] = ibuf;
  }

  // Particle composition
  particle.clear();
  for(i=0; i<particle_size; i++) {
    fscanf(stream, "%s %d \n", sbuf, ibuf);
    particle[sbuf] = ibuf;
  }

  // Wics
  wics.clear();
  for(i=0; i<wics_size; i++) {
    fprintf(stream, "%s %d \n", sbuf, ibuf);
    wics[sbuf] = ibuf;
  }

  // HH0
  hh0.clear();
  for(i=0; i<hh0.size(); i++) {
    fprintf(stream, "%f \n", dbuf);
    hh0.push_back(dbuf);
  }

  // Gas composite
  if(!gas_composite.dataIn(stream)) {
    fclose(stream);
    return false;
  }

  // GasCells
  gas_cell.clear();
  for(i=0; i<gas_cell_size; i++) {
    GasCell gc(this);
    if(!gc.dataIn(stream)) {
      fclose(stream);
      return false;
    }
    gas_cell.push_back(gc);
  }
  
  fclose(stream);

  return true;
}
