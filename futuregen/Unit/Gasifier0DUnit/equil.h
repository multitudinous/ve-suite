
    // class definition for equil class

#ifndef _equil_h_
#define _equil_h_

#include <cstdio>
#include <cstring>
#include <string>
#include <iostream>
#include <vector>
#include <map>

#include <V21Helper/Therm/thermo.h>
#include <V21Helper/Therm/stream.h>
#include <V21Helper/Therm/REAL.h>
//#include "thermo.h"
//#include "stream.h"
//#include "REAL.h"

    
class equil
{

public:

   // mutators
  	equil(){};
   ~equil(){};
   static void equilibrium(thermo& thm, stream& strm, REAL qht, REAL burnout, REAL mdot,
                 REAL& temp, std::vector<REAL>& comp, std::vector<REAL>& mol);
   static REAL cp_h2o_l(const REAL temp);
   static REAL cp_coal(const REAL temp);
   static REAL cp_ash(const REAL temp);
   static REAL enthalpy_ash(const REAL temp);
   static REAL vap_pres_h2o(const REAL temp, REAL& hfg);

   // accessors

protected:

   static REAL enthalpy_h2o_l(const REAL temp);
   static REAL enthalpy_coal(const REAL temp);
};

#endif
