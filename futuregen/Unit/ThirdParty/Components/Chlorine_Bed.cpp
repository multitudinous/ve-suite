////////////////////////////////////////////////////
// David Lignell 
// REI
// Vision 21 modules
// June 2002
////////////////////////////////////////////////////



#include "Chlorine_Bed.h"

using std::cout;
using std::endl;

namespace Vision21 {

Chlorine_Bed::Chlorine_Bed()
{
  // Initialization and Default values
  Rgas       = 8.31451;                    // J/mol*K
  Pdrop      = 15;                         // psi
  Toperating = 1100;                       // F
  HClppm     = 1;
  HCleff     = -1;                         // %
  HCLflag = 0;

  calcDp         =  0;      // flag: user geometry input for pressure drop ?
  Bed_diam       = -1;      // have to spec -1's
  void_frac      = -1; 
  Bed_L          = -1;
  sphericity     = 0.65;
  particle_size  = -1;
} 

bool Chlorine_Bed::calculate(Gas &gasin, Gas &gas2)
{
  
  //cout << endl
  //     << "**************************************************"
  //     << "\n***********  Chlorine Guard Bed  *****************"
  //     << "\n**************************************************";
  //cout << "\nInlet Gas Properties:";
  //gasin.gas_composite.Property_Output();


  gas2.copy(gasin);
  
////////// TEMPERATURE //////////
  //gas2.gas_composite.T = (Toperating-32.0)/1.8+273.15;        // F to K  
  gas2.gas_composite.T += Toperating * 5.0/9.0;     // add value in F to a K value
    if(gas2.gas_composite.T < 273.15) {
    cout << "\nError in Chlorine Guard: Operating Temp < 0 deg C";
    return false;
  }

  ////////// PRESSURE //////////
  if(!calcDp) {
    if(Pdrop < 0) {
      cout << "\nPdrop must positive or zero";
      return false;
    }
    gas2.gas_composite.P -= Pdrop*6894.75729;                  // psi to Pa
    if(gas2.gas_composite.P < 0) {
      cout << "\nError in Chlorine Guard: Operating Pressure < 0";
      return false;
    }
  }
  else {                     // compute Delta P given user inputs
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
	 << "\n  Empty flow area (m2)           " << Acol << endl;
    // Ergun equation See Adels Fluidization Engineering book (Red) Kunii & Levenspeil
    // page 66
    gas2.gas_composite.P -= Bed_L * (150.*pow(1-void_frac,2)/pow(void_frac,3)*
		       mu*vo/pow(sphericity*particle_size,2) + 
		       1.75*(1-void_frac)/pow(void_frac,3)*rho*vo*vo/
		       sphericity/particle_size);
    if(gas2.gas_composite.P < 0 || gas2.gas_composite.P > 
       gasin.gas_composite.P) {
      cout << "\n Error in Chlorine Guard: Pressure < 0 or P out > P in";
      return false;
    }
  }  

  ////////// ACID GASES //////////
  if(gas2.gas_composite.moles("HCL") <= 0) {
    //cout << "\nOutlet Gas Properties: ";
    //gas2.gas_composite.Property_Output();
    return true;
  }

  if(HCLflag==0) {                              // removal eff specified
    if(HCleff < 0 || HCleff > 100) {
      cout << "\nError in Chlorine Guard: need 0 < HCleff < 100";
      return false;
      }
    gas2.gas_composite.moles(-HCleff/100*gas2.gas_composite.moles("HCL"),"HCL");
    //cout << "\nOutlet Gas Properties: ";
    //gas2.gas_composite.Property_Output();
    return true;
  } else {
  if(HClppm < 0) {
    cout << "\nError in Chlorine Guard: specify positive ppm of HCl";
    return false;
  }
  //cout << "\nHCL ppm = " << HClppm;
  //cout << "\n nHcl = " << gas2.gas_composite.moles();
  //cout << "\ngas2 yhcl = " << gas2.gas_composite.comp_specie[gas2.gas_composite.gas_parent->specie["HCL"]];
  //cout << endl << "change_by in chlorinebed.cc " << (HClppm/1.E6*gas2.gas_composite.moles()-
  //						     gas2.gas_composite.moles("HCL"))/(1.0-HClppm/1.E6);
  
  gas2.gas_composite.moles((HClppm/1.E6*gas2.gas_composite.moles()-
			    gas2.gas_composite.moles("HCL"))/
			   (1.0-HClppm/1.E6), "HCL");
  //cout << "\n nHcl = " << gas2.gas_composite.moles();
  //cout << "\ngas2 yhcl = " << gas2.gas_composite.comp_specie[gas2.gas_composite.gas_parent->specie["HCL"]];
  //cout <<"\ngas2.HCL is "<<gas2.gas_composite.moles("HCL")<<endl;
  //cout <<"gasin.HCL is "<<gasin.gas_composite.moles("HCL")<<endl;
  if(gas2.gas_composite.moles("HCL") > gasin.gas_composite.moles("HCL")) {
    cout << "\nWarning in Chlorine Guard: more moles HCl out than in" << endl;
  }

  //cout << "\nOutlet Gas Properties: ";
  //gas2.gas_composite.Property_Output();
  return true;
  }
}

} // end namespace 



