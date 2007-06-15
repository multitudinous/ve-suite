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

#ifndef UNIT_CONVERSION_H
#define UNIT_CONVERSION_H

#include <map>
#include <string>
#pragma warning (disable:4786)
///////////////////////////////////////////////////////////////////////////////////////

class dummy {

public:

  dummy () {};
  dummy (const dummy& d) {
    for(int i=0;i<3;i++)units[i]=d.units[i];
    memcpy(factors,d.factors,3*sizeof(float));
  }

  dummy& operator= (const dummy& d) {
    // check for self assign.
    if(this==&d)
      return(*this); 
    for(int i=0;i<3;i++)
      units[i]=d.units[i]; 
    memcpy(factors,d.factors,3*sizeof(float));
    return(*this);
  }

  std::string units[3];
  float       factors[3];

};

///////////////////////////////////////////////////////////////////////////////////////

typedef std::pair<std::string, float> UnitPair;
typedef std::pair<std::string, dummy> TablePair;

enum unit_type {si=0, metric=1, english=2};

///////////////////////////////////////////////////////////////////////////////////////

class UnitConverter {

public:

  UnitConverter  ();
  ~UnitConverter ();

  UnitPair convert(const std::string& orig_units, const float& value,	 const unit_type& units_out) const;
  UnitPair convert_temp (const std::string& orig_units, const float& value,
			 const unit_type& units_out) const;

protected:
 
  void add_dummy (dummy d);

  std::map<std::string, dummy> conversion_table;

};

///////////////////////////////////////////////////////////////////////////////////////

#endif
