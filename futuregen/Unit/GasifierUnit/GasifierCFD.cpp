
#include "V21Helper.h"

#include "GasifierCFD.h"

#ifndef WIN32
#include <dlfcn.h>
#else
#include <windows.h>
#include <direct.h>
#endif

#include <sys/types.h>
#include <sys/stat.h>
#ifndef WIN32
#include <unistd.h>
#endif
#include <cstdlib>
#include <cstdio>
#include <cmath>

class GasifierCFD* GAS_GLACIER_PTR;

#include "externc.h"   // callback prototypes

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

GasifierCFD::GasifierCFD ()
{
  _prt_restart = 0;
  _gas_restart = 0;

  abort_glacier = false;
  running = false;
  _summaries = NULL;

  _work_dir = "./case";
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

GasifierCFD::~GasifierCFD ()
{
 
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::setCoalType (std::string coaltype)
{
  if(coaltype == "Pittsburg_#8") {
    _wic_C    = 76.62;
    _wic_H    = 4.96;
    _wic_O    = 8.19;
    _wic_N    = 1.48;
    _wic_S    = 1.64;
    _wic_CL   = 0.098;
    _ash_ult  = 7.01;
    _ash_prox = 7.01;
    _proxH2O  = 1.44;
    _proxVM   = 30.22;
    _proxFC   = 61.99;
    _hhv      = 13729.1;
    
    // Ash composition
    _comp1  = 45.37; // SiO2
    _comp2  = 24.18; // Al2O3
    _comp3  = 1.14; // TiO2
    _comp4  = 20.28; // Fe2O3
    _comp5  = 4.77; // CaO
    _comp6  = 1.03; // MgO
    _comp7  = 1.35; // Na2O
    _comp8  = 1.28; // K2O
    _comp9  = 0.0; // SO3
    _comp10 = 0.6; // P2O5
    _comp11 = 0.0; // BaO
    _comp12 = 0.0; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if(coaltype == "Illinois_#5") {
    _wic_C    = 69.230892;
    _wic_H    = 4.189073;
    _wic_O    = 14.328181;
    _wic_N    = 1.191795;
    _wic_S    = 0.0;
    _wic_CL   = 0.0;
    _ash_ult  = 11.06;
    _ash_prox = 9.3015;
    _proxH2O  = 15.9;
    _proxVM   = 34.17824;
    _proxFC   = 41.6203;
    _hhv      = 9672.34;

    // Ash composition - USING SAME AS ILLINOIS #6 !!!!
    _comp1  = 54.49; // SiO2
    _comp2  = 21.63; // Al2O3
    _comp3  = 0.84; // TiO2
    _comp4  = 15.0; // Fe2O3
    _comp5  = 2.3; // CaO
    _comp6  = 0.94; // MgO
    _comp7  = 0.52; // Na2O
    _comp8  = 2.09; // K2O
    _comp9  = 2.19; // SO3
    _comp10 = 0.0; // P2O5
    _comp11 = 0.0; // BaO
    _comp12 = 0.0; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if(coaltype == "Illinois_#6") {
    _wic_C    = 71.725923;
    _wic_H    = 5.063006;
    _wic_O    = 7.740774;
    _wic_N    = 1.406391;
    _wic_S    = 2.824032;
    _wic_CL   = 0.326283;
    _ash_ult  = 10.913591;
    _ash_prox = 9.7;
    _proxH2O  = 11.12;
    _proxVM   = 34.99;
    _proxFC   = 44.19;
    _hhv      = 11666;
    
    // Ash composition
    _comp1  = 54.49; // SiO2
    _comp2  = 21.63; // Al2O3
    _comp3  = 0.84; // TiO2
    _comp4  = 15.0; // Fe2O3
    _comp5  = 2.3; // CaO
    _comp6  = 0.94; // MgO
    _comp7  = 0.52; // Na2O
    _comp8  = 2.09; // K2O
    _comp9  = 2.19; // SO3
    _comp10 = 0.0; // P2O5
    _comp11 = 0.0; // BaO
    _comp12 = 0.0; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "Petcoke") {
    _wic_C    = 87.48;
    _wic_H    = 2.74;
    _wic_O    = 3.09;
    _wic_N    = 0.99;
    _wic_S    = 5.17;
    _wic_CL   = 0.01;
    _ash_ult  = 0.52;
    _ash_prox = 0.4836;
    _proxH2O  = 7.0;
    _proxVM   = 12.4;
    _proxFC   = 80.1164;
    _hhv      = 14282;

    // Ash composition
    _comp1  = 54.49; // SiO2
    _comp2  = 21.63; // Al2O3
    _comp3  = 0.84; // TiO2
    _comp4  = 15.0; // Fe2O3
    _comp5  = 2.3; // CaO
    _comp6  = 0.94; // MgO
    _comp7  = 0.52; // Na2O
    _comp8  = 2.09; // K2O
    _comp9  = 2.19; // SO3
    _comp10 = 0.0; // P2O5
    _comp11 = 0.0; // BaO
    _comp12 = 0.0; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if(coaltype == "Pike_County") {
    _wic_C    = 74.87;
    _wic_H    = 4.85;
    _wic_O    = 10.45;
    _wic_N    = 1.43;
    _wic_S    = 0.82;
    _wic_CL   = 0.13;
    _ash_ult  = 7.41;
    _ash_prox = 7.41;
    _proxH2O  = 2.33;
    _proxVM   = 33.8;
    _proxFC   = 56.46;
    _hhv      = 0.0; /* WHAT IS THIS ? */
    
    // Ash composition
    _comp1  = 55.0; // SiO2
    _comp2  = 33.76; // Al2O3
    _comp3  = 1.68; // TiO2
    _comp4  = 5.1; // Fe2O3
    _comp5  = 1.81; // CaO
    _comp6  = 0.59; // MgO
    _comp7  = 0.32; // Na2O
    _comp8  = 1.51; // K2O
    _comp9  = 0.0; // SO3
    _comp10 = 0.23; // P2O5
    _comp11 = 0.0; // BaO
    _comp12 = 0.0; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "Pocahantas_#3") {
    _wic_C    = 87.85;
    _wic_H    = 4.01;
    _wic_O    = 1.31;
    _wic_N    = 1.13;
    _wic_S    = 0.74;
    _wic_CL   = 0.0;
    _ash_ult  = 4.83;
    _ash_prox = 4.83;
    _proxH2O  = 6.13;
    _proxVM   = 17.11;
    _proxFC   = 71.93;
    _hhv      = 14079;

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "E-Gas_Illinois_#6") {
    _wic_C    = 70.4;
    _wic_H    = 4.8;
    _wic_O    = 6.96;
    _wic_N    = 1.3;
    _wic_S    = 3.74;
    _wic_CL   = 0.2;
    _ash_ult  = 12.6;
    _ash_prox = 12.6;
    _proxH2O  = 13.0;
    _proxVM   = 40.0;
    _proxFC   = 47.4;
    _hhv      = 12528*(1.0-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "E-Gas_Utah") {
    _wic_C    = 73.5;
    _wic_H    = 5.27;
    _wic_O    = 6.64;
    _wic_N    = 1.33;
    _wic_S    = 0.66;
    _wic_CL   = 0.01;
    _ash_ult  = 12.59;
    _ash_prox = 12.59;
    _proxH2O  = 7.95;
    _proxVM   = 41.0;
    _proxFC   = 46.41;
    _hhv      = 12210*(1.0-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "E-Gas_Wyodak") {
    _wic_C    = 67.59;
    _wic_H    = 4.8;
    _wic_O    = 17.7;
    _wic_N    = 1.2;
    _wic_S    = 0.8;
    _wic_CL   = 0.01;
    _ash_ult  = 7.9;
    _ash_prox = 7.9;
    _proxH2O  = 26.6;
    _proxVM   = 47.0;
    _proxFC   = 45.1;
    _hhv      = 11757*(1-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "E-Gas_Wyoming") {
    _wic_C    = 69.05;
    _wic_H    = 4.75;
    _wic_O    = 17.01;
    _wic_N    = 1.0;
    _wic_S    = 0.53;
    _wic_CL   = 0.04;
    _ash_ult  = 7.63;
    _ash_prox = 7.63;
    _proxH2O  = 30.24;
    _proxVM   = 41.0;
    _proxFC   = 51.37;
    _hhv      = 11955*(1-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO
  }
  else if (coaltype == "E-Gas_AppMS") {
    _wic_C    = 77.74;
    _wic_H    = 5.14;
    _wic_O    = 5.7;
    _wic_N    = 1.5;
    _wic_S    = 2.24;
    _wic_CL   = 0.06;
    _ash_ult  = 7.63;
    _ash_prox = 7.63;
    _proxH2O  = 5.05;
    _proxVM   = 30.38;
    _proxFC   = 62.0;
    _hhv      = 13965*(1-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  }
  else if (coaltype == "E-Gas_AppLS") {
    _wic_C    = 76.0;
    _wic_H    = 4.9;
    _wic_O    = 6.45;
    _wic_N    = 1.5;
    _wic_S    = 0.68;
    _wic_CL   = 0.07;
    _ash_ult  = 10.4;
    _ash_prox = 10.4;
    _proxH2O  = 5.6;
    _proxVM   = 25.0;
    _proxFC   = 64.6;
    _hhv      = 13860*(1-_proxH2O/100.0);

    // Ash composition
    _comp1  = 40.937262; // SiO2
    _comp2  = 27.074046; // Al2O3
    _comp3  = 1.380885; // TiO2
    _comp4  = 15.733391; // Fe2O3
    _comp5  = 8.709362; // CaO
    _comp6  = 1.881048; // MgO
    _comp7  = 1.130804; // Na2O
    _comp8  = 1.815810; // K2O
    _comp9  = 8.600631; // SO3
    _comp10 = 0.239208; // P2O5
    _comp11 = 0.608894; // BaO
    _comp12 = 0.48929; // SrO

    // Kinetics
    _devol_a1 = 3.7e5;
    _devol_e1 = 7.36e7;
    _devol_y1 = 0.3;
    _devol_a2 = 1.5e13;
    _devol_e2 = 2.51e8;
    _devol_y2 = 0.6;
    _oxid_a   = 0.013856;
    _oxid_n   = 1.0;
    _oxid_e   = 1.00416e8;
    _co2gas_a = 0.024377;
    _co2gas_n = 1.0;
    _co2gas_e = 1.7522e8;
    _h2ogas_a = 0.024377;
    _h2ogas_n = 1.0;
    _h2ogas_e = 1.7522e8;
  } else {
    cout << "Coal " << coaltype << " not found.\n";
  }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::normalizeUlt ()
{
  double total = _wic_C + _wic_H + _wic_O + _wic_N + _wic_S + _wic_CL + _ash_ult;
  _wic_C   = _wic_C   / total * 100; 
  _wic_H   = _wic_H   / total * 100; 
  _wic_O   = _wic_O   / total * 100; 
  _wic_N   = _wic_N   / total * 100;
  _wic_CL  = _wic_CL  / total * 100;
  _ash_ult = _ash_ult / total * 100;

  calculateProx();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::calculateProx ()
{
  _ash_prox = _ash_ult * (1.0 - _proxH2O / 100);
  double total = _proxH2O + _proxVM + _ash_prox;
  _proxFC = 100 - total;
}
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

bool GasifierCFD::execute (Gas *ox_in, Gas *gas_out, summary_values *summaries)
{  
  _gas_out = gas_out;
  _summaries = summaries;

  // Restart?
  bool restart = false;
  struct stat buf;
  if(!stat((_work_dir + "/RESTRT").c_str(), &buf) &&
     !stat((_work_dir + "/INLET").c_str(), &buf) &&
     !stat((_work_dir + "/DATA").c_str(), &buf) &&
     !stat((_work_dir + "/THERMO").c_str(), &buf) &&
     !stat((_work_dir + "/GASIFIER_DATA").c_str(), &buf) &&
     !stat((_work_dir + "/GRID").c_str(), &buf) &&
     !stat((_work_dir + "/PARSOU").c_str(), &buf))
    {
      restart = true;
    }

  if(restart) {
    FILE *stream;
  
    if((stream=fopen((_work_dir+"/GASIFIER_DATA").c_str(), "rt")) == NULL) {
      _error = "Unable to open " + _work_dir + "/GASIFIER_DATA";
      return false;
    }

    int st;
    fscanf(stream, "%d\n", &st);
    fclose(stream);

    if(st != _stage) {
      _error = "Existing data and GUI gasifier types do not match.";
      _prt_restart = 0;
      _gas_restart = 0;
      return false;
    } else {
      _prt_restart = 1;
      _gas_restart = 1;
    }
  } else {
    string path = _work_dir;
    string basepath = "./Glacier/Cases";
    if(_stage == 0) basepath += "/base_twostage/";
    else               basepath += "/base_onestage/";
    system(("cp " + basepath + "DATA " + path + "/DATA").c_str());
    system(("cp " + basepath + "INLET " + path + "/INLET").c_str());
    system(("cp " + basepath + "THERMO " + path + "/THERMO").c_str());
    system(("cp " + basepath + "GRID " + path + "/GRID").c_str());

    FILE *stream;

    if((stream=fopen((_work_dir+"/GASIFIER_DATA").c_str(), "wt")) == NULL) {
      _error = "Unable to open " + _work_dir + "/GASIFIER_DATA";
      return false;
    }

    fprintf(stream, "%4d\n", _stage);
    fclose(stream);
  }

  _pressure = ox_in->gas_composite.P - _press_drop;
  
  if(!get_running()) {
    set_running(true);
    if(!get_abort_glacier()) {
      cerr<<"Starting Glacier.....\n";	
      load_and_run_glacier();
    }
 
    if(!get_abort_glacier())
      cerr << "Possible error occured in Glacier run.\n";
     
    set_running(false);
  }
  
  cout<<"execute return"<<endl;
  return true;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_and_run_glacier()
{
  /*
       If the value of FilePath is NULL, a value for the main application is
       returned. This allows dynamically loaded objects to look up symbols in
       the main executable, or for an application to examine symbols available
       within itself. */

  /* 
     If the value of pathname is 0, dlopen() provides a handle on
     a  global symbol object.  This object provides access to the
     symbols from an ordered set of  objects  consisting  of  the
     original  program image file, together with any dependencies
     loaded at program startup, and any objects that were  loaded
     using  dlopen()  together with the RTLD_GLOBAL flag.  As the
     latter set of objects can change during  process  execution,
     the set identified by handle can also change dynamically. */
  //For windows, the counter part of dlopen will be included in the conditional comple part
  // Initialize global glacier pointer to this
  //LoadDLL, GetSym is the window's version of the dl operation of linux
  GAS_GLACIER_PTR = this;

  const char *errmsg;
#ifndef WIN32
  void *glacier_handle;
#else
  HINSTANCE glacier_handle;
#endif
  
  typedef void WIN_PREFIX close_io_func_type();
  typedef void glacier_func_type (gas_abort_status_fp,
		       gas_load_scirun_groups_fp,
		       gas_send_scirun_specie_fp,
		       gas_load_scirun_coal_fp,
		       gas_load_scirun_coalMT_fp,
		       gas_update_sr_fp,
		       gas_update_sr_nox_fp,
		       gas_update_sr_begin_particles_fp,
		       gas_update_sr_end_particles_fp,
		       gas_load_scirun_wics_fp,
		       gas_load_geom_sr_fp,
		       gas_load_current_traj_point_fp,
		       gas_send_scirun_data_fp,
		       gas_sr_begin_particle_fp,
		       gas_sr_end_particle_fp,
		       gas_insert_summary_val_fp,
		       gas_insert_xdata_fp,
		       gas_insert_ydata_fp,
		       gas_update_plot_fp,
		       gas_load_scirun_hhv_fp,
		       gas_load_scirun_pd_fp,
		       gas_update_dbfile_fp,
		       gas_load_scirun_slag_fp,
		       gas_load_scirun_flags_fp);

  close_io_func_type * close_io_func;
  glacier_func_type* glacier_func;
  
  string path = _work_dir;
#ifndef WIN32
  if(chdir(path.c_str())) {
    cerr << "GasifierCFD: empty working directory path\n";
    return ;
  }
#else
  if(_chdir(path.c_str())) {
    cerr << "GasifierCFD: empty working directory path\n";
    return ;
  }
#endif

#ifndef WIN32
  std::string glac_lib = "./Glacier/make_glacier/glacier_gasifier.so";
#else
  std::string glac_lib = "./Glacier/Glacier.dll";	
#endif

#ifndef WIN32
  glacier_handle = dlopen(glac_lib.c_str(), RTLD_NOW);
#else
  glacier_handle = LoadLibrary(glac_lib.c_str());
#endif

  if(glacier_handle=='\0'){
#ifndef WIN32
    cerr<<"Failed to load lib: "<<dlerror()<<endl;
#else
    cerr<<"Failed to open dll: "<<glac_lib<<endl;
#endif
    return;
  }

#ifndef WIN32
  dlerror();  // clear errors

  ((void*)glacier_func) = dlsym(glacier_handle,"start_glacier");
  if((errmsg=dlerror())!=NULL){
    cerr<<"Didn't find glacier in so: "<<errmsg<<endl;
    return;
  }
#else
  glacier_func =  (glacier_func_type*) GetProcAddress(glacier_handle,"start_glacier");
  if(glacier_func=='\0'){
    cerr<<"Didn't find start_glacier in dll: "<<glac_lib<<endl;
    return;
  }
#endif
  
  glacier_func(gas_abort_status_,
	       gas_load_scirun_groups_,
	       gas_send_scirun_specie_,
	       gas_load_scirun_coal_,
	       gas_load_scirun_coalMT_,
	       gas_update_sr_,
	       gas_update_sr_nox_,
	       gas_update_sr_begin_particles_,
	       gas_update_sr_end_particles_,
	       gas_load_scirun_wics_,
	       gas_load_geom_sr_,
	       gas_load_current_traj_point_,
	       gas_send_scirun_data_,
	       gas_sr_begin_particle_,
	       gas_sr_end_particle_,
	       gas_insert_summary_val_,
	       gas_insert_xdata_,
	       gas_insert_ydata_,
	       gas_update_plot_,
	       gas_load_scirun_hhv_, 
	       gas_load_scirun_pd_,
	       gas_update_dbfile_,
	       gas_load_scirun_slag_,
 	       gas_load_scirun_flags_);

	cout<<"Done with glacier_func"<<endl;

#ifndef WIN32  
  dlerror();  // clear errors
  ((void*)close_io_func) = dlsym(glacier_handle,"close_io_");
  if((errmsg=dlerror())!=NULL){
    cerr<<"Didn't find close_io in so: "<<errmsg<<endl;
    return;
  }
#else
  close_io_func = (close_io_func_type*) GetProcAddress(glacier_handle,"CLOSE_IO");
  if(close_io_func=='\0'){
    cerr<<"Didn't find close_io_ in dll: "<<glac_lib<<endl;
    return;
  }
#endif

  close_io_func();
#ifndef WIN32
  dlclose(glacier_handle); 
#else
  FreeLibrary(glacier_handle);
#endif

	
  // Back to what?
   if(_chdir("../")) {
    cerr << "bad directory path\n";
    return ;
   }  //work_dir is ./case
//   if(chdir("../../../../../../../build")) {
//    cerr << "bad directory path\n";
//    return ;
//  }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_geom(int* fni,int* fnj,int* fnk,int* fnx,int* fny,int* fnz,
		        float *x_vals,float *y_vals,float *z_vals,int *icell_array)
{
  ni     = *fni;
  nj     = *fnj;
  nk     = *fnk;
  nx     = *fnx;
  ny     = *fny;
  nz     = *fnz;
  node_x = x_vals;
  node_y = y_vals;
  node_z = z_vals;
  
  // load pcell data
  pcell.resize(ni);
  for(int i=0;i<ni;i++) {
    pcell[i].resize(nj);
    for(int j=0;j<nj;j++) {
      pcell[i][j].resize(nk);
      for(int k=0;k<nk;k++) {
	pcell[i][j][k] = icell_array[i+j*nx+k*nx*ny];
      }
    }
  }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_pd(float *pd, float *pmf, int *nps, int *numstr) {
  int size = 10;
  double pd_local[20];
  double pmf_local[20];
  // last area is calculated by fortran.
  double passed_areas[19] = {.05, .06, .09,
			     .1, .2, .2,
			     .1, .09, .06,
			     0.0, 0.0, 0.0,
			     0.0, 0.0, 0.0,
			     0.0, 0.0, 0.0,
			     0.0};
  double pct_thru_50 = _size_50;
  double pct_thru_200 = _size_200;
  // pd/pmf are 2-dim arrays, pass pd/pmf_locals[] instead then fill pd/pmf.
#ifndef WIN32
  histfit_(pd_local, pmf_local, passed_areas, &size, &pct_thru_50, &pct_thru_200);
#else
  HISTFIT(pd_local, pmf_local, passed_areas, &size, &pct_thru_50, &pct_thru_200);
#endif //WIN32
  // Fill pd, convert from microns to meters.
  // check - pd/pmf(np,numstr) - how to fill?
  for(int i=0; i<size; i++) {
    *(pd + i) = pd_local[i] * 1e-06;
    *(pmf + i) = pmf_local[i];
  }
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_wics(float *wic, int *j, int *nlm, int *np, int *nel) {
  int j_val = *j;
  int nel_val = *nel;
  
  // Read Initial Values from GUI.
  *(wic + (j_val-1) * nel_val + 0) = (float)_wic_C /(100.-(float)_ash_ult);
  *(wic + (j_val-1) * nel_val + 1) = (float)_wic_H /(100.-(float)_ash_ult);
  *(wic + (j_val-1) * nel_val + 2) = (float)_wic_O /(100.-(float)_ash_ult);
  *(wic + (j_val-1) * nel_val + 3) = (float)_wic_N /(100.-(float)_ash_ult);
  *(wic + (j_val-1) * nel_val + 4) = (float)_wic_S /(100.-(float)_ash_ult);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_groups(float *f, float *t, float *p, int *Istage) {
  int st = _stage;

  if(st==1) {
    //# O2
    t[0] = _ox_temp[0];
    f[0] = _ox_flow[0];
    //# Steam
    t[1] = _stm_temp[1];
    f[1] = _stm_flow[1];
    //# Slurry
    //# Injection angle
    //# GasifierCFD type
    *Istage = 1;
  } else {
    int i;
    for(i=0; i<3; i++) {
      t[i]   = _ox_temp[i];
      t[i+3] = _stm_temp[i];
      f[i]   = _ox_flow[i];
      f[i+3] = _stm_flow[i];
    }
    //# GasifierCFD type
    *Istage = 2;
  }
  //# Pressure
  *p = _pressure;
  
  return;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_hhv(float *omegal, float *yy, float *omegaa, float *hc0) {
  *(omegal) = (float)_proxH2O/100.;
  *(omegaa) = (float)_ash_prox/100.;
  *(yy) = (float)_proxVM/100. / (1 - (*omegal) - (*omegaa));

  // Basis (kg)
  double basis = 1;
  // Heats of formation.
  double co2 = -94.05;
  double h2o = -68.32;
  double so2 = -71;
  
  // Convert wics from wt%-dry to gmols.
  double wicAsh = _ash_ult;
  double wicC = (_wic_C * basis) / ((100 - wicAsh) * 12.010) * 1000;
  double wicH = (_wic_H * basis) / ((100 - wicAsh) * 1.0079) * 1000;
  //double wicO = (_wic_O * basis) / ((100 - wicAsh) * 15.9994) * 1000;
  //double wicN = (_wic_N * basis) / ((100 - wicAsh) * 14.010) * 1000;
  double wicS = (_wic_S * basis) / ((100 - wicAsh) * 32.060) * 1000;

  // Convert hhv from BTU/lb (as received) to KCal/kg (daf).
  double hhv_1 = (-1) * _hhv;
  hhv_1 = hhv_1 / (1 - (_ash_prox + _proxH2O) / 100);
  hhv_1 = hhv_1 / 0.4536 * 252 / 1000;

  // HC0 in Glacier (J/kg @ 298K)
  // (1 kg basis - 1 mole as defined above)
  (*hc0) = (wicC*co2 + wicH/2*h2o + wicS*so2 - basis * hhv_1) * 4184;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_coal(float *coal_flows) {
  int i;

  if(_stage==1) {
    coal_flows[0] = _slur_flow[0] * (_coal_pct[0] + _char_pct[0])/100;
    coal_flows[1] = _slur_flow[0] * (100 - _coal_pct[0] + _char_pct[0])/100;
    for(i=2; i<9; i++)
      coal_flows[i] = 0.0;
  } else {
    for(i=0; i<3; i++) {
      coal_flows[i*3] = _slur_flow[i] * (_coal_pct[i] + _char_pct[i])/100;
      coal_flows[i*3+1] = _slur_flow[i] * (100 - _coal_pct[i] + _char_pct[i])/100;
      coal_flows[i*3+2] = 0.0;
    }
  }

  return;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::send_scirun_data(int *ns, int *nlm,
				float *sns, float *stb, float *sew,
				float *part_flow, float *part_temp, float *part_size, float *part_var,
				float *u, float *v, float *w, float *x, float *y, float *z,
				float *eff, float *eta, float *chi,
				float *t, float *p, float *h, float *den,
				float *spec_val, float *wic_val,
				float *part_char, float *part_ash, float *part_water, float *part_coal,
				float *hco, float *hwo, float *hao, float *hho,
				char *spec_name, char *wic_name, char *part_name,
				float *press_in,
				float *ht_conv, float *ht_netwall, float *ht_netexit,
				unsigned int s1len, unsigned int s2len, unsigned int s3len)
{
  int i, j, k;
  int ns_val = (*ns);
  int nlm_val = (*nlm);
  int FF = 7;
 

  printf("start call send_scirun_data\n");
  for(k=0; k<nk; k++)
    for(j=0; j<nj; j++) {
      if((pcell[0][j][k] == FF) &&
	 (pcell[1][j][k] == FF)) 
	_gas_out->gas_cell.push_back(outlet_cell(1, j, k, 0,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));
      else if((pcell[ni-1][j][k] == FF) &&
	      (pcell[ni-2][j][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(ni-2, j, k, 1,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));	
    }
//	printf("cp1\n"); fflush(NULL);
  for(k=1; k<nk-1; k++)
    for(i=1; i<ni-1; i++) {
      if((pcell[i][0][k] == FF) &&
	 (pcell[i][1][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, 1, k, 2,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));	
      else if((pcell[i][nj-1][k] == FF) &&
	      (pcell[i][nj-2][k] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, nj-2, k, 3,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));
    }
//	printf("cp2\n"); fflush(NULL);
  for(j=1; j<nj-1; j++)
    for(i=1; i<ni-1; i++) {
      if((pcell[i][j][0] == FF) &&
	 (pcell[i][j][1] == FF)) 
	_gas_out->gas_cell.push_back(outlet_cell(i, j, 1, 4,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));
      else if((pcell[i][j][nk-1] == FF) &&
	      (pcell[i][j][nk-2] == FF))
	_gas_out->gas_cell.push_back(outlet_cell(i, j, nk-2, 5,
						 ns_val, nlm_val,
						 sns, stb, sew,
						 part_flow, part_temp, part_size, part_var,
						 u, v, w, x, y, z,
						 eff, eta, chi,
						 t, p, h, den,
						 spec_val,
						 part_char, part_ash, part_water, part_coal,
						 hco, hwo, hao, hho));
    }
  
	/*
//This section of code is to take care of the \0 problem for Compaq Fortan compiler, it takes \0 as '\\0'
  char *copy_wic;
  char *copy_spec;
  char *copy_part;
  int len = strlen(wic_name);
  copy_wic = new char[len+1];
  strcpy(copy_wic, wic_name);
  int ci;
  for (ci=0; ci<len-1; ci++)
	  if (copy_wic[ci]=='\\'&& copy_wic[ci+1]=='0')
		  copy_wic[ci]='\0';
  len = strlen(spec_name);
  copy_spec = new char[len+1];
  strcpy(copy_spec, spec_name);
  for (ci=0; ci<len-1; ci++)
	  if (copy_spec[ci]=='\\'&&copy_spec[ci+1]=='0')
		  copy_spec[ci]='\0';
  len = strlen(part_name);
  copy_part = new char[len+1];
  strcpy(copy_part, part_name);
  for (ci=0; ci<len-1; ci++)
	  if (copy_part[ci]=='\\'&&copy_part[ci+1]=='0')
		  copy_part[ci]='\0';
  		*/

  for(i=0; i<nlm_val; i++) {
    _gas_out->comp_wics.push_back((double)*(wic_val + i));
    _gas_out->wics[string(/*copy_wic*/wic_name + i*9)] = i;
  }
 printf("cp3\n"); fflush(NULL);
  for(i=0; i<ns_val; i++)
    _gas_out->specie[string(/*copy_spec*/spec_name + i*9)] = i;
   
  for(i=0; i<4; i++) 
    _gas_out->particle[string(/*copy_part*/part_name + i*9)] = i;

  //delete copy_wic;
  //delete copy_spec;
  //delete copy_part;
      
  _gas_out->hh0.push_back((double)(*hho));
  _gas_out->hh0.push_back((double)(*hao));
  _gas_out->hh0.push_back((double)(*hwo));
  _gas_out->hh0.push_back((double)(*hco));

 // printf("cp4\n"); fflush(NULL);
  _gas_out->average();
 // printf("cp5\n"); fflush(NULL);
  // Pressure at inlet is gage.
  _gas_out->pressure_drop = _press_drop;
 // printf("cp6\n"); fflush(NULL);
  // Heat data
  //heat_data->conv =((double)(*ht_conv));
  //heat_data->net = ((double)(*ht_netwall));
  //heat_data->inc = (-1)*((double)(*ht_netexit));
 // printf("end call send_scirun_data\n"); fflush(NULL);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

GasCell GasifierCFD::outlet_cell(int i, int j, int k, int face,
			      int ns_val, int nlm_val,
			      float *sns, float *stb, float *sew,
			      float *part_flow, float *part_temp, float *part_size, float *part_var,
			      float *u, float *v, float *w, float *x, float *y, float *z,
			      float *eff, float *eta, float *chi,
			      float *t, float *p, float *h, float *den,
			      float *spec_val,
			      float *part_char, float *part_ash, float *part_water, float *part_coal,
			      float *hco, float *hwo, float *hao, float *hho)
{
  GasCell cell(_gas_out);
  int sp;
  int nx_ny_k = nx*ny*k;
  int nx_j = nx*j;
  float density;

  // One of these is incorrect, see switch below for correction.
  cell.icell.push_back(i);
  cell.icell.push_back(j);
  cell.icell.push_back(k);

  // One of these is incorrect, see switch below for correction.
  cell.node_location.push_back((double)*(x + nx_ny_k + nx_j + i));
  cell.node_location.push_back((double)*(y + nx_ny_k + nx_j + i));
  cell.node_location.push_back((double)*(z + nx_ny_k + nx_j + i));

  // If outlet cell is on a lower face (i,j or k=1), then
  // one of these is incorrect, see switch below for correction.
  cell.velocity.push_back((double)*(u + nx_ny_k + nx_j + i));
  cell.velocity.push_back((double)*(v + nx_ny_k + nx_j + i));   
  cell.velocity.push_back((double)*(w + nx_ny_k + nx_j + i));
 
  // This is 4-DIM array (NX,NY,NZ,NSP)
  for(sp=0; sp<ns_val; sp++)
    cell.comp_specie.push_back((double)*(spec_val + nx*ny*nz*sp
					  + nx_ny_k + nx_j + i));

  density = (double)*(den + nx_ny_k + nx_j + i);
  cell.eff = (double)*(eff + nx_ny_k + nx_j + i);
  cell.eta = (double)*(eta + nx_ny_k + nx_j + i);
  cell.chi = (double)*(chi + nx_ny_k + nx_j + i);
  cell.T = (double)*(t + nx_ny_k + nx_j + i);
  cell.P = (double)*(p + nx_ny_k + nx_j + i) + _pressure; // p is relative to mean
  //cell.H = (double)*(h + nx_ny_k + nx_j + i);
  cell.mean_size = (double)*(part_size + nx_ny_k + nx_j + i);
  cell.size_variance = (double)*(part_var + nx_ny_k + nx_j + i);
  cell.T_particle = (double)*(t + nx_ny_k + nx_j + i);
 
  // Outlet cells are on a face (ie. i-face has i=1 or i=ni).  But we
  // passed in the i,j,k for the adjacent interior cell (ie. i=2 or i=ni-1).
  // Normally this is where the "good" data is kept...but not always:
  //
  // node_location:  We definitely want the true (x,y,z) for this.
  // area: Computed differently depending upon face.
  // M: Computed using average of two cell densities (outlet cell and its adjacent
  //   interior), and cell area.
  // M_particle and comp_particle: In psolve.f these are only
  //   computed for - and stored at - outlet cells (interiors are ALL zero).
  // velocity: Stored offset up - ie. velocity for outlet cell (1,j,k) is stored
  //   at (2,j,k), so above is correct; but outlet cell (ni-1,j,k) is stored at
  //   (ni,j,k) so we must adjust.

  switch (face) {
  case 0: // i=1 face
    // velocity is correct from above calculation.
    cell.node_location[0]=(double)*(x + nx_ny_k + nx_j + (i-1));
    cell.area = (*(sns + j)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + (i-1))) / 2)
      * cell.velocity[0] * cell.area;
    break;
  case 1: // i=ni face
    cell.velocity[0]=(double)*(u + nx_ny_k + nx_j + (i+1));
    cell.node_location[0]=(double)*(x + nx_ny_k + nx_j + (i+1));
    cell.area = (*(sns + j)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + (i+1))) / 2)
      * cell.velocity[0] * cell.area;

    cell.M_particle = (double)*(part_flow + nx_ny_k + nx_j + (i+1));
    cell.comp_particle.push_back((double)*(part_coal + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_char + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_water + nx_ny_k + nx_j + (i+1)));
    cell.comp_particle.push_back((double)*(part_ash + nx_ny_k + nx_j + (i+1)));
    break;
  case 2: // j=1 face
    // velocity is correct from above calculation.
    cell.node_location[1]=(double)*(x + nx_ny_k + nx*(j-1) + i);
    cell.area = (*(sew + i)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx_j + i)) / 2)
      * cell.velocity[1] * cell.area;
    break;
  case 3: // j=nj face
    cell.velocity[1]=(double)*(v + nx_ny_k + nx*(j+1) + i);   
    cell.node_location[1]=(double)*(x + nx_ny_k + nx*(j+1) + i);
    cell.area = (*(sew + i)) * (*(stb + k));
    cell.M = ((density + (double)*(den + nx_ny_k + nx*(j+1) + i)) / 2)
      * cell.velocity[1] * cell.area;
    break;
  case 4: // k=1 face
    // velocity is correct from above calculation.
    cell.node_location[2]=(double)*(x + nx*ny*(k-1) + nx_j + i);
    cell.area = (*(sew + i)) * (*(sns + j));
    cell.M = ((density + (double)*(den + nx*ny*(k-1) + nx_j + i)) / 2)
      * cell.velocity[2] * cell.area;
    break;
  case 5: // k=nk face 
    cell.velocity[2]=(double)*(w + nx*ny*(k+1) + nx_j + i);
    cell.node_location[2]=(double)*(x + nx*ny*(k+1) + nx_j + i);
    cell.area = (*(sew + i)) * (*(sns + j));
    cell.M = ((density + (double)*(den + nx*ny*(k+1) + nx_j + i)) / 2)
      * cell.velocity[2] * cell.area;
    break;
  default: // Not on a face.
    cerr << "Outlet cell not on a face?/n";
    break;
  }
  return(cell);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::send_scirun_specie(int *ns, float *spec_val, char *spec_name,
				  unsigned int slen)
{
  int i, j, k, sp;
  int ns_val = (*ns);
  vector<GasCell>::iterator cell;
  
  for (cell=_gas_out->gas_cell.begin(); cell!=_gas_out->gas_cell.end(); cell++) {
    (*cell).comp_specie.clear();
    for(sp=0; sp<ns_val; sp++) {
      i = (*cell).icell[0];
      j = (*cell).icell[1];
      k = (*cell).icell[2];
      (*cell).comp_specie.push_back
	((double)*(spec_val + nx*ny*nz*sp + nx*ny*k + nx*j + i)); 
    }
  }
  _gas_out->specie.clear();
  for(i=0; i<ns_val; i++)
    _gas_out->specie[string(spec_name + i*9)] = i;
  
  _gas_out->average();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::insert_summary_val(char *description, float *value,
				  unsigned int slen)
{
	//cout<<"Yang: insert summary_val called on _summaries"<<endl;
	//cout<<description<<endl;
	//cout<<"try to print the value"<<endl;
	//cout<<value<<endl;
	//if (value)
	//	cout<<(float)(*value)<<endl;
	//else
	//	cout<<"value is NULL"<<endl;
	//cout<<"Value printed"<<endl;
	//if (_summaries!=NULL)
	_summaries->insert_summary_val(description, *value);
	//else
	//	cout<<"NULL _summaries"<<endl;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::insert_xdata(char *xname, float *values, int *num_values,
			    unsigned int slen)
{
   // load x data entries
    vector<float> xvalues;
    for(int i=0; i<(*num_values); i++)
      xvalues.push_back(values[i]);
 
    // create an empty YDataMap object
    YDataMap ydm;
  
    profile_map_iter map_iter = plot_data_map.find(xname);
    if(map_iter != plot_data_map.end()) 
      plot_data_map.erase(map_iter);

    // construct a pair using xvalues and ydm
    pair<vector<float>, YDataMap> new_pair(xvalues,ydm);
    plot_data_map[xname] = new_pair;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::insert_ydata(char *yname, float *values, int *num_values,
			    char *xdata_name, unsigned int s1len, unsigned int s2len)
{
  // load y data entries
  vector<float> yvalues;
  for(int i=0; i<(*num_values);i++)
    yvalues.push_back((double)values[i]);
 
  profile_map_iter map_iter = plot_data_map.find(xdata_name);
  if(map_iter != plot_data_map.end()) {
    YDataMapIter ymap_iter = map_iter->second.second.find(yname);
    if (ymap_iter != map_iter->second.second.end())
      if ((unsigned)(*num_values) != map_iter->second.first.size()) {
	cerr << "ERROR: Y DATA SET SIZE != X DATA SET SIZE.\n";
	return;
      }
      else
	map_iter->second.second.erase(ymap_iter);
    map_iter->second.second[yname] = yvalues;
  }
  else
    cerr << "ERROR: XDATA SET NOT FOUND.\n";
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::update_plot()
{
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void GasifierCFD::load_scirun_slag(double *deltaw, double *deltar, double *kw, double *kr,
				double *ks, double *kd, double *ha, double *rhos,
				double *ta1, double *tcv, double *emiss, double *ashcomp)
{
  //  DELTAW - thick. wall
  //  DELTAR - thick. ref. (m)
  //  KW - k wall
  //  KR - k ref.
  //  KS - k slag
  //  KD - k dry ash (W/(m deg K))
  //  EMISSS - emissivity of slag !!! Get from wall emis in abskg !!!
  //  HA - h air W/(m^2 K)
  //  RHOS - density slag - kg/m**3
  //  TA1 - Ambient air temp (K)
  //  TCV - temp. slag at critical visc. (deg K) !!! get from subroutine 
  
  int st = _stage;

  //* TEST
  if(st==1) {
    *deltaw = 0.0254;
    *deltar = 0.1;
    *kw = 51.91928;
    *kr = 11.07611;
    *ks = 3.4613;
    *kd = 3.4613;
    *ha = 567.7947;
    *rhos = 2498.88;
    *ta1 = 300.0;
    *tcv = 1350.0 + 273.0;
    *emiss = 0.75;
  } else {
    *deltaw = 0.0254;
    *deltar = 0.1;
    *kw = 43.0;
    *kr = 8.0;
    *ks = 1.9;
    *kd = 1.9;
    *ha = 50.0;
    *rhos = 2535.0;
    *ta1 = 300.0;
    *tcv = 1350.0 + 273.0;
    *emiss = 0.83;
  }

  *(ashcomp+0)  = _comp1;
  *(ashcomp+1)  = _comp2;
  *(ashcomp+2)  = _comp3;
  *(ashcomp+3)  = _comp4;
  *(ashcomp+4)  = _comp5;
  *(ashcomp+5)  = _comp6;
  *(ashcomp+6)  = _comp7;
  *(ashcomp+7)  = _comp8;
  *(ashcomp+8)  = _comp9;
  *(ashcomp+9)  = _comp10;
  *(ashcomp+10) = _comp11;
  *(ashcomp+11) = _comp12;
  
  return;
}

void GasifierCFD::load_scirun_flags (int *lrsrt, int *lprst)
{
  *lrsrt = _gas_restart;
  *lprst = _prt_restart;

  cerr << "GAS RESTART  = " << _gas_restart << endl;
  cerr << "PART RESTART = " << _prt_restart << endl;
}

//******************************************************************************
//******************************  C Functions **********************************
//******************************************************************************

int WIN_PREFIX gas_abort_status_()
{
  int ret;  // watch out for byte-sized bool's!
  
  if(GAS_GLACIER_PTR->get_abort_glacier()) ret=1;
  else                                     ret=0;

  return(ret);
}

void WIN_PREFIX gas_load_geom_sr_(int* ni,int* nj,int* nk,int* nx,int* ny,int* nz,
		       float *x_vals,float *y_vals,float *z_vals,int *icell_array)
{
  GAS_GLACIER_PTR->load_geom(ni,nj,nk,nx,ny,nz,x_vals,y_vals,z_vals,icell_array);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_(int* iteration_number)
{
  GAS_GLACIER_PTR->send_current_outputs(iteration_number);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_nox_(int* iteration_number)
{
  GAS_GLACIER_PTR->send_current_outputs_nox(iteration_number);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_current_traj_point_(int *IJKNT, float *XP,float *YP,
				  float *ZP, float *TIM, float *PNFRP,
				  float *SIGMAX,float *SIGMAY,float *SIGMAZ,
				  float *ALFT0P,float *PDIA,float *TMP,
				  float *SIGMA0)
{   
  GAS_GLACIER_PTR->load_current_traj_point(XP, YP, ZP,
					   TIM, PNFRP,
					   SIGMAX, SIGMAY, SIGMAZ,
					   ALFT0P, PDIA, TMP,
					   SIGMA0);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_begin_particles_()
{
  GAS_GLACIER_PTR->update_sr_begin_particles();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_sr_end_particles_(float* nix)
{
  GAS_GLACIER_PTR->update_sr_end_particles(nix);
}
	
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_sr_begin_particle_(int *ips, int *isl)
{
  GAS_GLACIER_PTR->sr_begin_particle(ips,isl);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_sr_end_particle_(int *ips, int *isl, int *nps, int *nsl)
{
  GAS_GLACIER_PTR->sr_end_particle(ips,isl,nps,nsl);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_wics_(float *wic, int *j, int *nlm, int *np, int *nel)
{
  GAS_GLACIER_PTR->load_scirun_wics(wic, j, nlm, np, nel);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/	

void WIN_PREFIX gas_load_scirun_groups_(float *f, float *t, float *p, int* istage)
{
  GAS_GLACIER_PTR->load_scirun_groups(f, t, p, istage);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_hhv_(float *omegal, float *yy, float *omegaa, float *hc0)
{
  GAS_GLACIER_PTR->load_scirun_hhv(omegal, yy, omegaa, hc0);;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_pd_(float *pd, float *pmf, int *nps, int *numstr)
{
  GAS_GLACIER_PTR->load_scirun_pd(pd, pmf, nps, numstr);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_coalMT_(float *coal_flows, float *coal_temps)
{
  GAS_GLACIER_PTR->load_scirun_coalMT(coal_flows, coal_temps);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_coal_(float *coal_flows)
{
  GAS_GLACIER_PTR->load_scirun_coal(coal_flows);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_send_scirun_data_(int *ns, int *nlm,
			   float *sns, float *stb, float *sew,
			   float *part_flow, float *part_temp, float *part_size, float *part_var,
			   float *u, float *v, float *w, float *x, float *y, float *z,
			   float *eff, float *eta, float *chi,
			   float *t, float *p, float *h, float *den,
			   float *spec_val, float *wic_val,
			   float *part_char, float *part_ash, float *part_water, float *part_coal,
			   float *hco, float *hwo, float *hao, float *hho,
			   char *spec_name, char *wic_name, char *part_name,
			   float *press_in,
			   float *ht_conv, float *ht_netwall, float *ht_netexit,
			   unsigned int s1len, unsigned int s2len, unsigned int s3len)
{
  GAS_GLACIER_PTR->send_scirun_data(ns, nlm,
				    sns, stb, sew,
				    part_flow, part_temp, part_size, part_var,
				    u, v, w, x, y, z,
				    eff, eta, chi,
				    t, p, h, den,
				    spec_val, wic_val,
				    part_char, part_ash, part_water, part_coal,
				    hco, hwo, hao, hho,
				    spec_name, wic_name, part_name,
				    press_in,
				    ht_conv, ht_netwall, ht_netexit,
				    s1len, s2len, s3len);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_send_scirun_specie_(int *ns, float *spec_val, char *spec_name,
			     unsigned int slen)
{
  GAS_GLACIER_PTR->send_scirun_specie(ns, spec_val, spec_name, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_summary_val_(char *description, float *value, unsigned int slen)
{
  GAS_GLACIER_PTR->insert_summary_val(description, value, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_xdata_(char *xname, float *values, int *num_values,
		       unsigned int slen)
{
  GAS_GLACIER_PTR->insert_xdata(xname, values, num_values, slen);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_insert_ydata_(char *yname, float *values, int *num_values,
		       char *xdata_name, unsigned int s1len, unsigned int s2len)
{
  GAS_GLACIER_PTR->insert_ydata(yname, values, num_values, xdata_name, s1len, s2len);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_plot_()
{
  GAS_GLACIER_PTR->update_plot();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_update_dbfile_()
{
  GAS_GLACIER_PTR->update_dbfile();
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_slag_(double *deltaw, double *deltar, double *kw, double *kr,
			   double *ks, double *kd, double *ha, double *rhos,
			   double *ta1, double *tcv, double *emiss, double *ashcomp)
{
  GAS_GLACIER_PTR->load_scirun_slag(deltaw, deltar, kw, kr,
				    ks, kd, ha, rhos,
				    ta1, tcv, emiss, ashcomp);
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void WIN_PREFIX gas_load_scirun_flags_ (int *lrsrt, int *lprst)
{  
  GAS_GLACIER_PTR->load_scirun_flags (lrsrt, lprst);
}

