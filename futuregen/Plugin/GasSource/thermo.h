
    // class definition for thermo class

#ifndef _thermo_h_
#define _thermo_h_

#include <cstdio>
#include <cstring>
#include <iostream>

#include <string>
#include <vector>
#include <map>

#include "REAL.h"
#include "matrix.h"

using std::string;

class thermo
{

public:

   // mutators
  	thermo();
  	thermo(const string thermo_file);
   ~thermo(){};
   bool read_thermo(const std::string& file);
   bool equilb(bool const_temp,
               REAL& temp, const REAL& pres, const REAL& enth,
               std::vector<REAL>& mass, std::vector<REAL>& mol);
   REAL mweight(const int& index);
   REAL mweight(const std::string& name);
   REAL mweight(const std::vector<REAL>& composition);
   REAL atweight(const std::string& name);
   REAL enthalpy_i(const int& is, REAL temp); // enthalpy normalized by rgas
   REAL enthalpy_is(const int& is, REAL temp);
   REAL enthalpy_is(const std::string& name, REAL temp);
   REAL enthalpy_mix(const std::vector<REAL>& composition, const REAL& temp);
   REAL entropy_i(const int& is, REAL temp); // entropy normalized by rgas
   REAL entropy_is(const std::string& name, REAL temp);
   REAL entropy_is(const int& is, REAL temp);
   REAL entropy_mix(const std::vector<REAL>& composition, const REAL& temp, const REAL& pres);
   REAL cp_i(const int& is, REAL temp); // cp normalized by rgas
   REAL cp_is(const int& is, REAL temp);
   REAL cp_is(const std::string& name, REAL temp);
   REAL cp_mix(const std::vector<REAL>& composition, const REAL& temp);
   bool find_temperature(REAL& temp, REAL& cp_mix, const REAL& enth, const std::vector<REAL>& mol);
   bool find_temperature(REAL& temp, const REAL& entropy,const std::vector<REAL>& composition, const REAL& pres);
   REAL thermal_cond_mix(const int& ieuck,const REAL& temp_gas, const REAL& temp,const std::vector<REAL>& composition);
   REAL viscosity_mix(const REAL& temp_gas, const REAL& temp,const std::vector<REAL>& composition);
   REAL prandtl_mix(const int& ieuck,const std::vector<REAL>& composition, const REAL& temp_gas, const REAL& temp_film_ref);
   std::vector<REAL> get_diffus(const REAL& temp_gas, const REAL& temp,
				const std::vector<REAL>& composition, const REAL& pres);
   int spec_int(const string& name);
   void check_temp_range(const std::vector<REAL>& mol, const REAL& temp);


   // accessors
   const std::map< std::string, int >& get_nam_spec() const {return(nam_spec);};
   const std::map< std::string, int >& get_nam_el() const {return(nam_el);};
   const std::vector< REAL >& get_mwt() const {return(mwt);};
   const std::vector<std::string>& get_spec_nam() const {return(spec_nam);};
   const REAL& get_rgas() const {return(rgas);};
   const std::vector< std::vector<int> >& get_atom() const {return(atom);};
   const std::vector< std::vector<REAL> >& get_mol_at_spc() const {return(mol_at_spc);};
   const int& get_nel() const {return(nel);};
   const std::vector<REAL>& get_atwt() const {return(atwt);};
   const std::vector<REAL>& get_valence() const {return(valence);};
   const std::vector<std::string>& get_el_nam() const {return(el_nam);};

   bool get_has_ljs() const { return (has_ljs); };

protected:

   // functions

   // data
   REAL rgas;
   int nel;
   std::vector<REAL> atwt;
   std::vector<REAL> valence;
   std::map<std::string, int> nam_el;
   std::vector<std::string> el_nam;
   std::vector<std::vector<int> > atom;
   std::vector<std::vector<REAL> > mol_at_spc;
   int nspc;
   std::vector<REAL> mwt;
   std::vector<std::vector<REAL> > zcoef;
   std::vector<std::string> spec_nam;
   std::map<std::string, int> nam_spec;
   std::vector<std::string > phase;
   std::map<std::string, int> nam_phase;
   std::vector< std::string > phase_nam;
   std::vector<int> iphase;
   std::vector<REAL> sum_phase, sumh_phase;
   std::vector<REAL> tmin;
   std::vector<REAL> tmax;
   std::vector<REAL> tmid;
   std::vector<REAL> totmol;
   REAL tmp_min;
   REAL tmp_max;
//   std::vector<int> irow;
   std::vector<REAL> b0;
   std::vector<REAL> dmol, enth0, cp0;
   REAL denom[6];
   std::vector<REAL> gfe, gfe0; // Gibbs free energy
   // Leonard Jones Parameters
   std::vector<REAL> s_lj, ek_lj, delta_lj;


   //solver
   matrix mtx;
   
   bool has_ljs; // Whether thermo used has L-J's.

private:

   REAL sigmad(const REAL& tek);
   REAL sigmam(const REAL& tek, const REAL& del);
   void find(const REAL& arg, REAL& ans, REAL* x, REAL* y, const int& npts);

};

//inlines
#endif
