#ifndef REI_GAS_H
#define REI_GAS_H

#include "GasCell.h"
#include <Therm/thermo.h>

#include <vector>
#include <map>
#include <string>

class Gas {

public:

  Gas  ();
  Gas  (const Gas&);
  ~Gas ();
  
  void clear ();
  void copy  (const Gas&);
  
  void average ();
  
  void addSpecie   (std::string spec, double value=0.0);
  void addParticle (std::string spec, double value=0.0);
  void addWic      (std::string spec, double value=0.0);

  // specie: Specie composition.
  std::map<std::string, int> specie; // Used by (GasCell) comp_specie.  // DOL MADE CONST TO ALLOW EASY COPYING FROM THERMO'S MAP
  // particle: Particle compostion.
  std::map<std::string, int> particle; // Used by (GasCell) comp_particle.
  // wics: Wic composition.
  std::map<std::string, int> wics; // Used by (Gas) comp_wics.

  std::vector<double> comp_wics; // Map (Gas) wics gives ordering.

  std::vector<double> hh0; // Ordered : Char, Ash, Water, Coal

  GasCell gas_composite;
  std::vector<GasCell> gas_cell;

  // This stuff is used by Baghouse module
  double CoalCal;
  double AshCal;
  double AshpH;

  // This keeps a running tally of pressure drop.
  double pressure_drop;

  // A pointer to a global thermo object, if none exists create one
  thermo *thermo_database;

  bool dataOut(FILE *stream);
  bool dataIn(FILE *stream);
};

#endif
