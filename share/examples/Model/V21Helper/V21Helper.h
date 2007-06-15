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
#ifndef V21HELPER_H
#define V21HELPER_H

#include <Datatypes/Gas.h>
#include <Therm/thermo.h>

#include <Datatypes/Water.h>
#include <Steam67/Steam67.h>

#include <interface.h>
#include <SummaryValues/summary_values.h>

class V21Helper {

public:

  V21Helper  (const char* thermo_path);
  ~V21Helper ();

  int IntToGas (Interface *it, Gas &gs);
  int GasToInt (Gas *gs, Interface &it);
  
  int IntToWat (Interface *it, Water &wt);
  int WatToInt (Water *wt, Interface &it);

  int IntToSum (Interface *it, summary_values &sv);
  int SumToInt (summary_values* sv, Interface &it);

  thermo  *thermo_database;
  Steam67 *steam67_database;
 
private:

};


#endif
