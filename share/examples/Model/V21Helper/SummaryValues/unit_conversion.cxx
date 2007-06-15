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

#include "unit_conversion.h"

///////////////////////////////////////////////////////////////////////////////////////

UnitConverter::UnitConverter ()
{
  dummy d;
  conversion_table.clear();
  
  // Mass
  d.units[si] = d.units[metric] = "kg";
  d.units[english] = "lb";
  
  // From si
  d.factors[si] = d.factors[metric] = 1.0f;
  d.factors[english] = 2.2046f;
  add_dummy(d);
  
  // Length
  d.units[si] = d.units[metric] = "m";
  d.units[english] = "ft";
  
  // From si
  d.factors[si] = d.factors[metric] = 1.0f;
  d.factors[english] = 3.2808f;
  add_dummy(d);
  
  // Resistance
  d.units[si] = "m^2*K/W";
  d.units[metric] = "m^2*hr*C/kcal";
  d.units[english] = "ft^2*hr*F/Btu";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 1.163f;
  d.factors[english] = 5.6783f;
  add_dummy(d);
  
  // Velocity
  d.units[si] =  d.units[metric] = "m/s";
  d.units[english] = "ft/s";
  
  // From si
  d.factors[si] = d.factors[metric] = 1.0f;
  d.factors[english] = 3.2808f;
  add_dummy(d);

  // Energy
  d.units[si] = "J";
  d.units[metric] = "kcal";
  d.units[english] = "Btu";

  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 2.3885E-04f;
  d.factors[english] = 9.4778E-04f;
  add_dummy(d);

  // Flow Rate
  d.units[si] = "kg/s";
  d.units[metric] = "kg/hr";
  d.units[english] = "lb/hr";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 3600.0f;
  d.factors[english] = 7936.5f;
  add_dummy(d);
  
  // Heat Transfer
  d.units[si] = "W";
  d.units[metric] = "kcal/hr";
  d.units[english] = "Btu/hr";

  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 8.5985E-01f;
  d.factors[english] = 3.4122f;
  add_dummy(d);

  // Firing Rate has the same units as heat transfer, so they're already there
  
  // h
  d.units[si] = "W/m^2*K";
  d.units[metric] = "kcal/m^2*hr*C";
  d.units[english] = "Btu/ft^2*hr*F";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 8.5985E-01f;
  d.factors[english] = 1.7612E-01f;
  add_dummy(d);
  
  // Viscosity
  d.units[si] = "kg/m*s";
  d.units[metric] = d.units[english] = "CP";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = d.factors[english] = 1.0E+03f;
  add_dummy(d);
  
  // Density
  d.units[si] = d.units[metric] = "kg/m^3";
  d.units[english] = "lb/ft^3";
  
  // From si/metric
  d.factors[si] = d.factors[metric] = 1.0f;
  d.factors[english] = 6.2426E-02f;
  add_dummy(d);
  
  // Enthalpy
  d.units[si] = "kJ/kg";
  d.units[metric] = "kcal/kg";
  d.units[english] = "Btu/lb";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 2.3885E-01f;
  d.factors[english] = 4.2992E-01f;
  add_dummy(d);
  
  // Sp
  d.units[si] = "kJ/kg*K";
  d.units[metric] = "kcal/kg*C";
  d.units[english] = "Btu/lb*F";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = d.factors[english] = 2.3885E-01f;
  add_dummy(d);
  
  // Rw has the same units as resistance, so they're already there
  
  // Heat Flux
  d.units[si] = "W/m^2";
  d.units[metric] = "kcal/m^2*hr";
  d.units[english] = "Btu/ft^2*hr";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 8.5985E-01f;
  d.factors[english] = 3.1700E-01f;
  add_dummy(d);
  
  // Gravity
  d.units[si] = d.units[metric] = "m/s^2";
  d.units[english] = "ft/s^2";
  
  // From si/metric
  d.factors[si] = d.factors[metric] = 1.0f;
  d.factors[english] = 3.2808f;
  add_dummy(d);
  
  // Pressure
  d.units[si] = "Pa";
  d.units[metric] = "kg/cm^2";
  d.units[english] = "psi";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 1.0194E-05f;
  d.factors[english] = 1.4504E-04f;
  add_dummy(d);
  
  // %
  d.units[si]      = "%";
  d.units[metric]  = "%";
  d.units[english] = "%";
  
  // From si
  d.factors[si]      = 1.0f;
  d.factors[metric]  = 1.0f;
  d.factors[english] = 1.0f;
  add_dummy(d);

  // PPM
  d.units[si]      = "PPM";
  d.units[metric]  = "PPM";
  d.units[english] = "PPM";
  
  // From si
  d.factors[si]      = 1.0f;
  d.factors[metric]  = 1.0f;
  d.factors[english] = 1.0f;
  add_dummy(d);
  
  // Mole fraction
  d.units[si] = d.units[metric] = d.units[english] = "mole frac.";
  
  // From si
  d.factors[si] = 1.0f;
  d.factors[metric] = 1.0f;
  d.factors[english] = 1.0f;
  add_dummy(d);
  
  // Empty units
  d.units[si] = d.units[metric] = d.units[english] = "";
  d.factors[si] = d.factors[metric] = d.factors[english] = 1.0f;
  add_dummy(d);
  conversion_table.insert(TablePair("none", d));
}

///////////////////////////////////////////////////////////////////////////////////////

UnitConverter::~UnitConverter ()
{
  // Nothing to do for the destructor
}

///////////////////////////////////////////////////////////////////////////////////////

UnitPair UnitConverter::convert (const std::string& orig_units, const float& value, const unit_type& units_out) const
{
  std::string new_units;
  UnitPair up;
  std::map<std::string, dummy>::const_iterator map_iterator;
  
  // check for temperatures
  if(orig_units=="K"||orig_units=="F"||orig_units=="C")
    return(convert_temp(orig_units, value, units_out));
  
  // find units in conversion map
  map_iterator = conversion_table.find(orig_units);
  if(map_iterator != conversion_table.end()){
    up.first  = (*map_iterator).second.units[units_out];
    up.second = value*(*map_iterator).second.factors[units_out];
  }
  else 
    up = UnitPair(std::string("(no units)"), value);
  
  return(up);
}

///////////////////////////////////////////////////////////////////////////////////////

UnitPair UnitConverter::convert_temp (const std::string& orig_units, const float& value,
				      const unit_type& units_out) const
{
  UnitPair up;
  
  if(orig_units=="K"){
    switch(units_out){
    case si:
      up.first  = orig_units;
      up.second = value;
      break;
    case metric:
      up.first  = "C";
      up.second = value-273.15f;
      break;
    case english:
      up.first  = "F";
      up.second = 9.0/5.0*(value-273.15)+32.0;
      break;
    }
  }
  else if(orig_units=="C"){
    switch(units_out){
    case si:
      up.first  = "K";
      up.second = value+273.15f;
      break;
    case metric:
      up.first  = orig_units;
      up.second = value;
      break;
    case english:
      up.first  = "F";
      up.second = 9.0/5.0*(value)+32.0;
      break;
    }
  }
  else {
    switch(units_out){
    case si:
      up.first  = "K";
      up.second = 5.0/9.0*(value-32.0)+273.15;
      break;
    case metric:
      up.first  = "C";
      up.second = 5.0/9.0*(value-32.0);
      break;
    case english:
      up.first  = orig_units;
      up.second = value;
      break;
    }
  }
  return(up);
}

///////////////////////////////////////////////////////////////////////////////////////

void UnitConverter::add_dummy (dummy d)
{
  conversion_table.insert(TablePair(d.units[si], d));
  
  // From metric
  d.factors[si] = 1.0 / d.factors[metric];
  d.factors[metric] = 1.0f;
  d.factors[english] = d.factors[si] * d.factors[english];
  conversion_table.insert(TablePair(d.units[metric], d));
  
  // From english
  d.factors[metric] = 1.0 / d.factors[english];
  d.factors[si] = d.factors[metric] * d.factors[si];
  d.factors[english] = 1.0f;
  conversion_table.insert(TablePair(d.units[english], d));
}

///////////////////////////////////////////////////////////////////////////////////////
