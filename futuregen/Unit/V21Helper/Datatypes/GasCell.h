#ifndef GASCELL_H
#define GASCELL_H

//#include "Gas.h"
#include <Therm/thermo.h>

#include <vector>
#include <string>

class Gas;

class GasCell {

public:
  
  GasCell  (Gas *); 
  ~GasCell ();

  void copy (const GasCell&);
  void zero ();

  double mw              ();
  double density         ();
  double enthalpy        (double T1=0);
  double entropy         ();
  double cp_mix          (double T1=0);
  double thermal_conduct (double T1);

  double getFrac  (std::string specie);
  double setFrac  (std::string specie, double conc);
  double getPFrac (std::string specie);
  double setPFrac (std::string specie, double conc);

  // DOL ADDED 6/24/02
  double moles ();                                    
  double moles (double change_by, std::string component);
  double moles (std::string component);
  double Qvol  ();
  double Visc  ();

  // DOL ADDED
  void Property_Output  ();
  
  bool equilb           ();
  void find_temperature (REAL &t, REAL &c, REAL &e);

  void normalize_specie ();
  void balance_co2      ();

  Gas *gas_parent;

  std::vector<int> icell; // 3-D index for cell. Ordered i, j, k.
  std::vector<double> velocity; // Ordered u, v, w magnitude.
  std::vector<double> node_location; // Ordered x, y, z coordinate.

  std::vector<double> comp_specie; // Map (gas_parent) specie gives ordering.

  double area;
  double eff;
  double eta;
  double chi;

  double T; // Temperature
  double P; // Pressure
  double M; // Mass flowrate

  double tar; // Aiolos generated
  double soot; // Aiolos generated

  // PARTICLE INFORMATION
  double mean_size;
  double size_variance;
  double T_particle;
  double M_particle;
  std::vector<double> comp_particle; // Map (gas_parent) particle gives ordering.

  bool dataOut (FILE *stream);
  bool dataIn  (FILE *stream);
};

#endif
