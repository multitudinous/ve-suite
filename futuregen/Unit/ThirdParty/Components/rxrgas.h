// Gas class for Arsenic partitioning model 7-8/02

#ifndef RXRGAS_H
#define RXRGAS_H

#include <vector>
#include <map>
#include <string>
#include <V21Helper/Therm/thermo.h>
#include <V21Helper/Datatypes/Gas.h>

using namespace std;

class rxrgas{

public:
  vector<double> comp;
  double T;                            // K
  double P;                            // Pa
  double Moles;                        // gram-moles
  double Rgas;                         // J/mol*k
  map<string, int> names;        
  thermo thermo_data; 
  int nspec;

  rxrgas();
  rxrgas(Gas& v21gas);                 // construct using v21gas
  double Conc() { return P/Rgas/T;}    // mol/m3
  void Normalize();                    
  double moles(double amount, string species);
  double moles(string species);
  double Density();                    // kg/m3
  double MW();                         // g/gmol

  void copy(const rxrgas& gaso);       // copy within class (rxngas to rxngas)
  void rxrgas_2_v21gas(Gas& v21gas);   // update v21gas from rxrgas

};

#endif
