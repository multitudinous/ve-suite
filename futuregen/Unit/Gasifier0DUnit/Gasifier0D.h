#ifndef GASIFIER0D_H
#define GASIFIER0D_H

#include "V21Helper.h"

#include <iostream>
#include <fstream>
#include <string>
#include <map>

using namespace std;

extern "C"
{
  void histfit_ (double *ps, double *pmf, double *passed_areas, int *size,
		 double *percent_through_100, double *percent_through_200);
  void createbins_(double *ps, double *pmf, double *passed_areas, int *size,
		   double *m1, double *sg);
}

class Gasifier0D {


public:
  Gasifier0D (thermo *t);
  ~Gasifier0D ();
  
  void setCoalType (std::string coaltype);
  void normalizeUlt ();
  void calculateProx ();

  void execute (Gas *ox_in, Gas *stage2in,
		Gas *gas_out, summary_values *summaries);

  void slagvis (double temp, double* ashcomp, double& CoeffA, double& CoeffB, double& visc);
  void viscosity (double* ashcomp, double& temp, double& visc, int& flag,
		  double& CoeffA, double& CoeffB);

  // Flows
  double _ox_temp   [3];
  double _ox_flow   [3];
  double _stm_temp  [3];
  double _stm_flow  [3];
  double _slur_temp [3];
  double _slur_flow [3];
  double _coal_pct  [3];
  double _char_pct  [3];

  // Char Recycle
  double _char_flow [3];
  double _char_size;
  double _char_sd;
  double _ash_in_char;

  // WICS ...
  double _wic_C;
  double _wic_H;
  double _wic_O;
  double _wic_N;
  double _wic_S;
  double _wic_CL;
  double _ash_ult;
  double _ash_prox;
  double _proxH2O;
  double _proxVM;
  double _proxFC;
  double _hhv;

  // Ash composition
  double _comp1;  // SiO2
  double _comp2;  // Al2O3
  double _comp3;  // TiO2
  double _comp4;  // Fe2O3
  double _comp5;  // CaO
  double _comp6;  // MgO
  double _comp7;  // Na2O
  double _comp8;  // K2O
  double _comp9;  // SO3
  double _comp10; // P2O5
  double _comp11; // BaO
  double _comp12; // SrO

  // Coal Kinetics
  double _devol_a1;
  double _devol_a2;
  double _devol_e1;

  double _devol_e2;
  double _devol_y1;
  double _devol_y2;

  double _oxid_a;
  double _oxid_n;
  double _oxid_e;

  double _co2gas_a;
  double _co2gas_n;
  double _co2gas_e;

  double _h2ogas_a;
  double _h2ogas_n;
  double _h2ogas_e;

  // particle sizes
  double _size_50;
  double _size_200;

  // Gasifier type (1 : 1-stage)
  int _stage;

  // Pressure drop
  double _press_drop;

  // Execution / Misc.
  double _LD;
  double _diameter;
  double _length1;
  double _length2;

  double _rwall1;
  double _rwall2;
  double _emis1;
  double _emis2;
  
  double _back_temp;
  double _slag_eff;

  double _heatloss_gui1;
  double _heatloss_gui2;
  double _burnout_gui;

  int _specify_geom;
  int _design_mode;

  // Thermo
  thermo *thm;

  // Results
  double _thermal_input;

};

#endif
