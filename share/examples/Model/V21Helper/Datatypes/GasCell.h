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
#ifndef GASCELL_H
#define GASCELL_H

//#include "Gas.h"
#include <Therm/thermo.h>

#include <vector>
#include <string>

class Gas;

class GasCell {

public:
  
  GasCell  (Gas *); 
  ~GasCell ();

  void copy (const GasCell&);
  void zero ();

  double mw              ();
  double density         ();
  double enthalpy        (double T1=0);
  double entropy         ();
  double cp_mix          (double T1=0);
  double thermal_conduct (double T1);

  double getFrac  (std::string specie);
  double setFrac  (std::string specie, double conc);
  double getPFrac (std::string specie);
  double setPFrac (std::string specie, double conc);

  // DOL ADDED 6/24/02
  double moles ();                                    
  double moles (double change_by, std::string component);
  double moles (std::string component);
  double Qvol  ();
  double Visc  ();

  // DOL ADDED
  void Property_Output  ();
  
  bool equilb           ();
  void find_temperature (REAL &t, REAL &c, REAL &e);

  void normalize_specie ();
  void balance_co2      ();

  Gas *gas_parent;

  std::vector<int> icell; // 3-D index for cell. Ordered i, j, k.
  std::vector<double> velocity; // Ordered u, v, w magnitude.
  std::vector<double> node_location; // Ordered x, y, z coordinate.

  std::vector<double> comp_specie; // Map (gas_parent) specie gives ordering.

  // Soot
  double soot; // Soot mass fraction
  double tar; // Tar
  double ynu; // Soot part # / Mass

  double area;
  double eff;
  double eta;
  double chi;

  double T; // Temperature
  double P; // Pressure
  double M; // Mass flowrate

  // PARTICLE INFORMATION
  double mean_size;
  double size_variance;
  double T_particle;
  double M_particle;
  std::vector<double> comp_particle; // Map (gas_parent) particle gives ordering.

  bool dataOut (FILE *stream);
  bool dataIn  (FILE *stream);
};

#endif
