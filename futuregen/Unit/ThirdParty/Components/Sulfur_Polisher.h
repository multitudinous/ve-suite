////////////////////////////////////////////////////
// David Lignell 
// REI
// Vision 21 modules
// June 2002
////////////////////////////////////////////////////



#ifndef SULFUR_POLISHER_H
#define SULFUR_POLISHER_H

#include <Packages/REI/Core/Datatypes/Gas.h>

using std::cout;
using std::endl;

namespace Vision21 {

using namespace REI;

class Sulfur_Polisher {

 public:
  double Rgas;             // J/mol*K
  double Toperating;       // F
  double Pdrop;            // psia
  double H2Seff;           // %
  double H2Sppm;
  double COSeff;           // %
  double COSppm;

  // pressure drop needs:
  
  bool H2Sflag;
  bool COSflag;
  bool calcDp;             // user specified geom inputs? default F, calling routine set T
  double Bed_diam;         // m
  double void_frac;        
  double Bed_L;            // m
  double sphericity;     
  double particle_size;    // m

  
  Sulfur_Polisher();
  bool calculate(Gas &gasin, Gas &gas2);

};

} // end namespace

#endif

