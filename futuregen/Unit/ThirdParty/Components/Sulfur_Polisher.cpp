////////////////////////////////////////////////////
// David Lignell 
// REI
// Vision 21 modules
// June 2002
////////////////////////////////////////////////////

#include <iostream>
#include <cmath>
#include "Sulfur_Polisher.h"


using std::cout;
using std::endl;

namespace Vision21 {


Sulfur_Polisher::Sulfur_Polisher() 
{
  Rgas         = 8.31451;                 // J/mol*K
  Pdrop        = 5;                       // psi
  Toperating   = 750;                     // F 
  H2Seff       = -1;                      // %
  COSeff       = -1;                      // %
  H2Sppm       = 1;
  COSppm       = 0.1;

  calcDp         =  0;                    // flag reset in calling routine else default = 0
  Bed_diam       = -1;                    // have to spec -1's
  void_frac      = -1;           
  Bed_L          = -1;      
  sphericity     = 0.65;
  particle_size  = -1;

}

bool Sulfur_Polisher::calculate(Gas &gasin, Gas &gas2)
{
  
  //cout << endl
  //     << "**************************************************"
  //     << "\n*************  Sulfur Polisher  ******************"
  //     << "\n**************************************************";
  //cout << "\nInlet Gas Properties:";
  //gasin.gas_composite.Property_Output();


  gas2.copy(gasin);
  
  //////// TEMPERATURE //////////
  //  gas2.gas_composite.T = (Toperating-32.0)/1.8+273.15;        // F to K
  gas2.gas_composite.T += Toperating * 5.0/9.0;     // add value in F to a K value
  if(gas2.gas_composite.T < 273.15) {
    cout << "\nError in Sulfur Polisher: Operating Temp < 0 deg C";
    return false;
  }

  ////////// PRESSURE //////////
  if(!calcDp) {
    if(Pdrop < 0) {
      cout << "\n Pdrop must be positive or zero";
      return false;
    }
    gas2.gas_composite.P -= Pdrop*6894.75729;                  // psi to Pa
    if(gas2.gas_composite.P < 0) {
      cout << "\nError in Sulfur Polisher: Operating Pressure < 0";
      return false;
    }
  }
  else {       // compute Delta P given user inputs
    cout << "\nCalculate Pressure Drop from User Inputs: ";
    cout << "\n  Bed diameter (m)    " << Bed_diam
	 << "\n  Bed depth (m)       " << Bed_L
	 << "\n  Bed void fraction   " << void_frac
	 << "\n  Particle Size (m)   " << particle_size
	 << "\n  Particle Sphericity " << sphericity;
    if(Bed_diam <= 0 || void_frac <= 0 || Bed_L <= 0 || 
       particle_size <= 0 || sphericity <= 0) {
      cout << "\nError in Chlorine Guard:"
	   << "\n\tUser inputs for pressure drop "
	   << "\n\tcalculation improperly specified";
      return false;
    }
    double mu   = gas2.gas_composite.Visc();
    double Acol = M_PI/4.*pow(Bed_diam,2.);
    double vo   = gas2.gas_composite.Qvol()/Acol;
    double rho  = gas2.gas_composite.density();
    cout << "\n  Gas superficial velocity (m/s) " << vo
	 << "\n  Gas density (kg/m3)            " << rho
	 << "\n  Gas viscosity (Pa*s)           " << mu
	 << "\n  Empty flow area (m2)           " << Acol;
    // Ergun equation See Adels Fluidization Engineering book (Red) Kunii & Levenspeil
    // page 66.  (Assuming a fixed bed, or a moving bed with neglible particle vel)
    gas2.gas_composite.P -= Bed_L * (150.*pow(1-void_frac,2)/pow(void_frac,3)*
		       mu*vo/pow(sphericity*particle_size,2) + 
		       1.75*(1-void_frac)/pow(void_frac,3)*rho*vo*vo/
		       sphericity/particle_size);
    if(gas2.gas_composite.P < 0 || 
       gas2.gas_composite.P > gasin.gas_composite.P) {
      cout << "\n Error in Bulk desulf: Pressure < 0 or P out > P in";
      return false;
    }
  }  
  
  ////////// ACID GASES //////////
  if(H2Sflag==0) {                              // removal eff specified
    if(H2Seff < 0 || H2Seff > 100) {
      cout << "\nError in Sulfur Polisher: need 0 < H2Seff < 100";
      return false;
    }
    if (gas2.gas_composite.moles("H2S") > 0.0) 
    gas2.gas_composite.moles(-H2Seff/100*gas2.gas_composite.moles("H2S"),"H2S");
  }
  else {
    if (gas2.gas_composite.moles("H2S") > 0.0) {
      if(H2Sppm < 0) {
	cout << "\nError in Sulfur Polisher: Specify Positive H2S conc";
	return false;
      }
      gas2.gas_composite.moles(-1.*
			     (H2Sppm/1.E6*gas2.gas_composite.moles()-
			      gas2.gas_composite.moles("H2S"))/
			     (H2Sppm/1.E6-1.),"H2S");
      if(gas2.gas_composite.moles("H2S") > gasin.gas_composite.moles("H2S")) {
	cout << "\nWarning in Sulfur Polisher: more H2S out than in";
      }
      if(gas2.gas_composite.moles("H2S") <=0) {
	cout << "\nError in Sulfur Polisher: Negative H2S out.\n";
	return false;
      }    }
  }
  
  if(COSflag==0) {                              // removal eff specified
    if(COSeff < 0 || COSeff > 100) {
      cout << "\nError in Sulfur Polisher: need 0 < COSeff < 100";
      return false;
    }
    if(gas2.gas_composite.moles("COS") > 0.0)
    gas2.gas_composite.moles(-COSeff/100*gas2.gas_composite.moles("COS"),"COS");
  }
  
  else {
    if(gas2.gas_composite.moles("COS") > 0.0) {
      if(COSppm < 0) {
	cout << "\nError in Sulfur Polisher: Specify positive COS conc";
	return false;
      }
      gas2.gas_composite.moles(-1.*
			     (COSppm/1.E6*gas2.gas_composite.moles()-
			      gas2.gas_composite.moles("COS"))/
			     (COSppm/1.E6-1.),"COS");  
      if(gas2.gas_composite.moles("COS") > gasin.gas_composite.moles("COS")) {
	cout << "\nWarning in Sulfur Polisher: more COS out than in";
      }
    }
  }

  //cout << "\nOutlet Gas Properties: ";
  //gas2.gas_composite.Property_Output();
  return true;

}

} // end namespace 



