////////////////////////////////////////////////////
// David Lignell 
// REI
// Vision 21 modules
// June 2002
////////////////////////////////////////////////////

#ifndef BULK_DESULFURIZER_H
#define BULK_DESULFURIZER_H

#include <cmath>
#include <Packages/REI/Core/Datatypes/Gas.h>

namespace Vision21 {

using namespace REI;

class Bulk_Desulfurizer {

 public:
  double Rgas;           // J/mol*K
  double Pdrop;          // psi
  double Toperating;     // F
  double H2Sppm;
  double H2Seff;         // %
  double COSppm; 
  double COSeff;         // %

  Bulk_Desulfurizer();
  bool calculate(Gas &gasin, Gas &gas2);
  
  // pressure drop needs;
  bool H2Sflag;
  bool COSflag;
  bool calcDp;       // flag for user spec inputs. Default F, calling routine to set T
  double Bed_diam;         // m
  double void_frac;    
  double Bed_L;            // m
  double sphericity;   
  double particle_size;    // m
  double particle_dens;    // kg/m3
  double solids_flow;      // kg/s
  
};

}  // end namespace Vision21
#endif
