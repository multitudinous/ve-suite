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
#ifndef REI_GAS_H
#define REI_GAS_H

#include "GasCell.h"
#include <Therm/thermo.h>

#include <vector>
#include <map>
#include <string>

class Gas {

public:

  Gas  ();
  Gas  (const Gas&);
  ~Gas ();
  
  void clear ();
  void copy  (const Gas&);
  
  void average ();
  
  void addSpecie   (std::string spec, double value=0.0);
  void addParticle (std::string spec, double value=0.0);
  void addWic      (std::string spec, double value=0.0);

  // specie: Specie composition.
  std::map<std::string, int> specie; // Used by (GasCell) comp_specie.  // DOL MADE CONST TO ALLOW EASY COPYING FROM THERMO'S MAP
  // particle: Particle compostion.
  std::map<std::string, int> particle; // Used by (GasCell) comp_particle.
  // wics: Wic composition.
  std::map<std::string, int> wics; // Used by (Gas) comp_wics.

  std::vector<double> comp_wics; // Map (Gas) wics gives ordering.

  std::vector<double> hh0; // Ordered : Char, Ash, Water, Coal

  GasCell gas_composite;
  std::vector<GasCell> gas_cell;

  // This stuff is used by Baghouse module
  double CoalCal;
  double AshCal;
  double AshpH;

  // Temporarily add coal data
  double _wic_C;
  double _wic_H;
  double _wic_O;
  double _wic_N;
  double _wic_S;
  double _wic_CL;
  double _ash_ult;
  double _ash_prox;
  double _proxH2O;
  double _proxVM;
  double _proxFC;
  double _hhv;
  double _comp1;
  double _comp2;
  double _comp3;
  double _comp4;
  double _comp5;
  double _comp6;
  double _comp7;
  double _comp8;
  double _comp9;
  double _comp10;
  double _comp11;
  double _comp12;
  double _coal_feedRate;

  // This keeps a running tally of pressure drop.
  double pressure_drop;

  // A pointer to a global thermo object, if none exists create one
  thermo *thermo_database;

  bool dataOut(FILE *stream);
  bool dataIn(FILE *stream);
};

#endif
