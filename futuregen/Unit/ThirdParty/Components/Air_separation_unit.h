// Black Box cryogenic air separation unit for Vision 21.  
// Model has inlet compressed air stream and outlet O2 and N2 product
// streams.  Assume No O2 in N2 product stream for simplicity.
// Assume complete removal of all air components except O2, N2, Ar in 
// molecular sieve.  For energy balance, assume the molecular sieve species
// are at 298.15 K.  
// User specifies O2 product purity, T,P of O2, N2 streams.
// Assume 75% of inlet Ar to O2 product, 25% to N2 product.



#ifndef AIR_SEPARATION_UNIT
#define AIR_SEPARATION_UNIT

#include <V21Helper/Datatypes/Gas.h>
#include <V21Helper/Datatypes/GasCell.h>
#include <cmath>
#include <iostream>

using namespace std;

namespace Vision21 {

class Air_separation_unit {

 public:

  double energy_requirement;         // kW
  double T_O2stream, P_O2stream;     // inputs: T in K, P in bar.
  double T_N2stream, P_N2stream;
  double O2_purity;                  // O2 product Percent
   
  Air_separation_unit();
  bool Calculate(Gas &AirIn, Gas &O2stream, Gas&N2stream); 
  double Psat(Gas &strm, string species);                     

};

} // end namespace Vision21
  
#endif
