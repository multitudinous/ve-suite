////////////////////////////////////////////////////
// David Lignell 
// REI
// Vision 21 modules
// June 2002
////////////////////////////////////////////////////

#ifndef CHLORINE_BED_H
#define CHLORINE_BED_H

#include <iostream>
#include <cmath>
#include <V21Helper/Datatypes/Gas.h>

namespace Vision21 {

class Chlorine_Bed {
 public:
  double Rgas;          // J/mol*K
  double Pdrop;         // psi
  double Toperating;    // F
  double HClppm;
  double HCleff;        // %

  // pressure drop needs:
  bool HCLflag;
  bool calcDp;          // calling routine sets flag true if user enters geometry etc.
  double Bed_diam;      // m
  double void_frac;    
  double Bed_L;         // m
  double sphericity;   
  double particle_size; // m  
  
  Chlorine_Bed();
  bool calculate(Gas &gasin, Gas &gas2);	   
};
} // end namespace 
#endif
