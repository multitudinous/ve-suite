    
// class definition for transiet_mpf class
    
#ifndef _part_kinetics_h_
#define _part_kinetics_h_
    
    
#include <V21Helper/Therm/REAL.h>
//#include "REAL.h"
#include "equil.h"
#include <vector>
#include <cstdio>
#include <string>

class model
{
public:

   // mutators
   model(){};
   virtual ~model(){};

   virtual void initialize(REAL x, std::vector<REAL>& y0, std::vector<REAL>& yscal) = 0;
   virtual void derivs(REAL& x, std::vector<REAL>& y,
      std::vector<REAL>& dydx) = 0;
   virtual void update(REAL& x, REAL& x2, REAL& x0, std::vector<REAL>& y,
      std::vector<REAL>& dydx) = 0;
   virtual void push_back(REAL& x, std::vector<REAL>& y,
      std::vector<REAL>& dydx) = 0;
   virtual void output(std::vector<REAL>& xp, std::vector< std::vector<REAL> >& yp,
      char* dir) = 0;

protected:

};

class part_kinetics: public model
{

public:

   // mutators
   part_kinetics();
   part_kinetics(std::vector<REAL>& typ_frac, std::vector< std::vector<REAL> >& adev0,
      std::vector< std::vector<REAL> >& edev0, std::vector< std::vector<REAL> >& ydev0,
      std::vector< std::vector<REAL> >& ahet0, std::vector< std::vector<REAL> >& nhet0,
      std::vector< std::vector<REAL> >& ehet0, std::vector< std::vector<REAL> >& nuhet0,
      std::vector< std::vector<std::string> >& oxyd0, std::vector<REAL>& part_den0, 
      std::vector< std::vector<REAL> >& part_dia0,
      std::vector< std::vector<REAL> >& size_frac0, std::vector<REAL>& omega_srf0,
      std::vector<REAL>& omega_char0, std::vector<REAL>& omega_liq0, 
      std::vector<REAL>& omega_ash0, REAL& temp_init, thermo& thm, stream& stm);
   virtual ~part_kinetics(){};
   virtual void initialize(REAL x, std::vector<REAL>& y0, std::vector<REAL>& yscal);
   virtual void derivs(REAL& x, std::vector<REAL>& y, std::vector<REAL>& dydx);
   virtual void update(REAL& x, REAL& x2, REAL& x0, std::vector<REAL>& y, std::vector<REAL>& dydx);
   virtual void push_back(REAL& x, std::vector<REAL>& y,
      std::vector<REAL>& dydx);
   virtual void output(std::vector<REAL>& xp, std::vector< std::vector<REAL> >& yp, char* dir);

   // accessors
   REAL& get_temp_gas() {return(temp_gas);}
   REAL& get_temp_gas1() {return(temp_gas1);}
   REAL& get_temp_gas2() {return(temp_gas2);}
   REAL& get_burnout() {return(burnout);}
   REAL& get_res_time1() {return(res_time1);}
   REAL& get_res_time2() {return(res_time2);}
   std::vector<REAL>& get_start_time() {return(start_time);}
   REAL& get_ash_enth_init_1() {return(ash_enth_init_1);}
   REAL& get_ash_enth_init_2() {return(ash_enth_init_2);}
   REAL& get_ash_enth() {return(ash_enth);}
   REAL& get_dp_mean() {return dp_mean;}
   REAL& get_dp_var() {return dp_var;}
   void reset_burnedout() {burnedout = false;}
   void set_desired_burnout(REAL burnout) {desired_burnout = burnout;}

protected:

   thermo *thm;
   stream *stm;

   std::vector<REAL> part_den;
   std::vector< std::vector<REAL> > adev, edev, ydev;
   std::vector< std::vector<REAL> > ahet, nhet, ehet, nuhet;
   std::vector< std::vector<std::string> > oxyd;
   std::vector< std::vector<REAL> > solid_raw_fuel, Char, liq, ash, part_dia, size_frac;
   std::vector<REAL> type_frac, start_time;
   std::vector< std::vector<REAL> > solid_raw_fuel_tm, Char_tm, liq_tm;
   std::vector< std::vector<REAL> > omega_srf, omega_char, omega_liq, omega_ash;
   std::vector< std::vector<REAL> > num_flow, mass_cp;
   std::vector<REAL> comp, mol; //, diff;
   std::vector< std::vector<REAL> > comp_table;
   std::vector< std::vector< std::vector<REAL> > > diff;
   REAL temp_gas, temp_gas1, temp_gas2, burnout, initial_fuel, temp_init, tboil;
   REAL res_time1, res_time2, desired_burnout;
   REAL ash_enth_init_1, ash_enth_init_2, ash_enth, dp_mean, dp_var;
   bool burnedout;
};

#endif
