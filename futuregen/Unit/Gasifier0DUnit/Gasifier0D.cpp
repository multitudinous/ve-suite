
#include "part_kinetics.h"
#include "sode_rk.h"

#include "Gasifier0D.h"

#include <cstdlib>
#include <cstdio>
#include <cmath>

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

Gasifier0D::Gasifier0D (thermo *t)
{
  thm = t;
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

Gasifier0D::~Gasifier0D ()
{
 
}

//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void Gasifier0D::setCoalType (std::string coaltype)
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

void Gasifier0D::normalizeUlt ()
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

void Gasifier0D::calculateProx ()
{
  _ash_prox = _ash_ult * (1.0 - _proxH2O / 100);
  double total = _proxH2O + _proxVM + _ash_prox;
  _proxFC = 100 - total;
}
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void Gasifier0D::execute (Gas *ox_in, Gas *stage2in,
			  Gas *gas_out, summary_values *summaries)
{  

  int i;
  int j;

  bool second_stage = (_stage != 1);

  double wic_c  = _wic_C;
  double wic_h  = _wic_H;
  double wic_o  = _wic_O;
  double wic_n  = _wic_N;
  double wic_s  = _wic_S;
  double wic_cl = _wic_CL;
  
  double pres0 = ox_in->gas_composite.P - _press_drop;
  
  double ashcomp[12];
  ashcomp[0] = _comp1;  ashcomp[1]  = _comp2;  ashcomp[2]  = _comp3;
  ashcomp[3] = _comp4;  ashcomp[4]  = _comp5;  ashcomp[5]  = _comp6;
  ashcomp[6] = _comp7;  ashcomp[7]  = _comp8;  ashcomp[8]  = _comp9;
  ashcomp[9] = _comp10; ashcomp[10] = _comp11; ashcomp[11] = _comp12;

  //////////////////
  // Particle sizes
  //////////////////

  int ndpo = 5;
  double dpo[20], modpo[20], dpo_c[20], modpo_c[20];
  double passed_areas[19] = {0.05, 0.2, 0.5, 0.2, 0.05, 0.0, 0.0, 0.00, 0.00,
			     0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0};

  double pct_thru_50  = _size_50;
  double pct_thru_200 = _size_200;

#ifndef WIN32
  histfit_(dpo, modpo, passed_areas, &ndpo, &pct_thru_50, &pct_thru_200);
  createbins_(dpo_c, modpo_c, passed_areas, &ndpo, &_char_size, &_char_sd);
#else
  HISTFIT(dpo, modpo, passed_areas, &ndpo, &pct_thru_50, &pct_thru_200);
  CREATEBINS(dpo_c, modpo_c, passed_areas, &ndpo, &_char_size, &_char_sd);
#endif
  double dp_mean = 0.0;
  double dp_var  = 0.0;

  for(i=0; i<ndpo; i++)
    dp_mean += dpo[i]*modpo[i];

  for(i=0; i<ndpo; i++) dp_var += (dpo[i] - dp_mean)*(dpo[i] - dp_mean)*modpo[i];

  ///////////////////////////////////////
  // Calculate heat of formation for coal
  ///////////////////////////////////////

  // Basis (kg)

  double basis = 1;
  
  // Heats of formation.
  
  double co2 = -94.05 * 4.184e6;
  double h2o = -68.32 * 4.184e6;
  double so2 = -71 * 4.184e6;
  double hcl = -92.31 / 4.184 * 4.184e6;
  
  // Convert wics from wt%-dry to gmols.
  
  double wicAsh = _ash_ult;
  double wicC   = (wic_c * basis) / ((100 - wicAsh) * 12.010);
  double wicH   = (wic_h * basis) / ((100 - wicAsh) * 1.0079);
  double wicO   = (wic_o * basis) / ((100 - wicAsh) * 15.9994); // not used
  double wicN   = (wic_n * basis) / ((100 - wicAsh) * 14.010); // not used
  double wicS   = (wic_s * basis) / ((100 - wicAsh) * 32.060);
  double wicCL  = (wic_cl * basis) / ((100 - wicAsh) * 35.453);

  //# Convert hhv from BTU/lb (as received) to (daf).

  double hhv_1 = (-1) * _hhv / (1 - (_ash_prox + _proxH2O) / 100);
  double lhv_1 = (-1) * (_hhv - 970.3 * (8.9364 * wic_h/100 * (1-_proxH2O/100) + _proxH2O/100))
      /(1 - (_ash_prox + _proxH2O) / 100);
  hhv_1 = hhv_1/4.2995e-4; // Btu/lb -> J/kg
  lhv_1 = lhv_1/4.2995e-4; // Btu/lb -> J/kg

  // HC0 in J/kg @ 298K

  double hhf_coal = (wicC*co2 + (wicH-wicCL)/2*h2o + wicS*so2 + wicCL*hcl - basis * hhv_1);
    
  ///////////////////////////////////////////
  // Calculate dry, ashfree flowrate of coal
  ///////////////////////////////////////////

  double coal_m_daf[3], omegac[3], omegah[3], omegaash[3];
  for(i=0; i<3; i++){
    omegac[i] = (_coal_pct[i] / 100.0) * (1.0 - _proxH2O/100.0 - _ash_prox/100.0); // for slurry only
    omegaash[i] = (_coal_pct[i]/100.0) * (_ash_prox/100.0); // for slurry only
    omegah[i] = _char_pct[i]/100.0; // for recycled char only
    coal_m_daf[i] = _slur_flow[i] * omegac[i] + _char_flow[i]*omegah[i]; 
  }

  //////////////////////////////////////
  // Calculate flowrate of liquid water
  //////////////////////////////////////

  double omegal[3], h2o_m_liq[3];
  for(i=0; i<3; i++) {
    omegal[i] = (100 - _coal_pct[i]) / 100 + _coal_pct[i] / 100 * _proxH2O/100;
    h2o_m_liq[i] = _slur_flow[i] * omegal[i];
  }
     
  /////////////////////////
  // Stream Initialization
  /////////////////////////

  int nstr = 4;
  if(stage2in) nstr++;

  std::vector<int> istream0;
  std::vector<int> istream1;

  std::vector<double> str_frac1;
  std::vector<double> str_frac2;
  std::vector<double> temp0;

  std::vector<std::string> frac_typ;
  std::vector<std::string> fuel0;

  std::vector<double>                    hform    (nstr);
  std::vector<double>                    str_frac (nstr);
  std::vector<std::vector<double> >      frac     (nstr);
  std::vector<std::vector<std::string> > spc_nam  (nstr);

  std::map<std::string, int>::iterator map_iter;
  
  // stream 1 = OXIDANT

  istream0.push_back(1);
  istream1.push_back(1);
  frac_typ.push_back("mole");
  fuel0.push_back("oxidant");
  
  for(map_iter=ox_in->specie.begin(); map_iter!=ox_in->specie.end(); map_iter++) {
    spc_nam[0].push_back(map_iter->first);
    frac[0].push_back(ox_in->gas_composite.comp_specie[map_iter->second]);
  }

  if(second_stage) {
    str_frac1.push_back(_ox_flow[0] + _ox_flow[1]);
    str_frac2.push_back(_ox_flow[2]);
  }
  else {
    str_frac1.push_back(_ox_flow[0]);
    str_frac2.push_back(0.0);
  }

  temp0.push_back(_ox_temp[0]);
   
  // stream 2  = COAL

  istream0.push_back(2);
  istream1.push_back(2);
  frac_typ.push_back("elem");
  fuel0.push_back("fuel");
  
  spc_nam[1].push_back("C");  frac[1].push_back(wic_c  / (100 - wicAsh));
  spc_nam[1].push_back("H");  frac[1].push_back(wic_h  / (100 - wicAsh));
  spc_nam[1].push_back("O");  frac[1].push_back(wic_o  / (100 - wicAsh));
  spc_nam[1].push_back("N");  frac[1].push_back(wic_n  / (100 - wicAsh));
  spc_nam[1].push_back("S");  frac[1].push_back(wic_s  / (100 - wicAsh));
  spc_nam[1].push_back("CL"); frac[1].push_back(wic_cl / (100 - wicAsh));

  hform[1] = hhf_coal;

  if(second_stage) {
    str_frac1.push_back(coal_m_daf[0] + coal_m_daf[1]);
    str_frac2.push_back(coal_m_daf[2]);
  } else {
    str_frac1.push_back(coal_m_daf[0]);
    str_frac2.push_back(0.0);
  }

  temp0.push_back(_slur_temp[0]);
   
  // stream 3 = LIQUID WATER

  istream0.push_back(3);
  istream1.push_back(3);
  frac_typ.push_back("elem");
  fuel0.push_back("oxidant");
  
  spc_nam[2].push_back("H"); frac[2].push_back(0.1119);
  spc_nam[2].push_back("O"); frac[2].push_back(0.8881);
 
  hform[2] = -1.58800E+07;
  
  if(second_stage) {
    str_frac1.push_back(h2o_m_liq[0] + h2o_m_liq[1]);
    str_frac2.push_back(h2o_m_liq[2]);
  } else {
    str_frac1.push_back(h2o_m_liq[0]);
    str_frac2.push_back(0.0);
  }

  temp0.push_back(_slur_temp[0]);
   
  // stream 4 = STEAM

  istream0.push_back(4);
  istream1.push_back(4);
  frac_typ.push_back("mole");
  fuel0.push_back("oxidant");
  
  spc_nam[3].push_back("H2O"); frac[3].push_back(1.0);

  if(second_stage) {
    str_frac1.push_back(_stm_flow[0] + _stm_flow[1]);
    str_frac2.push_back(_stm_flow[2]);
  } else { 
    str_frac1.push_back(_stm_flow[0]);
    str_frac2.push_back(0.0);
  }

  temp0.push_back(_stm_temp[0]);
      
  // stream 5 = SECOND STAGE GAS

  if(stage2in) {
    istream0.push_back(5); 
    istream1.push_back(5);
    frac_typ.push_back("mole");
    fuel0.push_back("oxidant");
    
    for(map_iter=stage2in->specie.begin(); map_iter!=stage2in->specie.end(); map_iter++) {
      spc_nam[4].push_back(map_iter->first);
      frac[4].push_back(ox_in->gas_composite.comp_specie[map_iter->second]);
    }
    
    if(second_stage) {
      str_frac1.push_back(0.0);
      str_frac2.push_back(_ox_flow[2]);
    } else {
      str_frac1.push_back(0.0);
      str_frac2.push_back(0.0);
    }
    
    temp0.push_back(_ox_temp[2]);
  }
    
  ///////////////////////////
  // Particle Initialization
  ///////////////////////////

  int ni   = 1;
  int ntyp = 0;

  double omegal1[3];
  double tot_part_flow = 0.0;

  if(second_stage) ni = 3;

  for(i=0; i<ni; i++){
    tot_part_flow += _slur_flow[i] * (omegac[i]+omegaash[i]) + _char_flow[i];
    
    if(omegac[i]>0.0) ntyp++;
    if(omegah[i]>0.0 || _char_flow[i]*_ash_in_char>0.0) ntyp++;
    omegal1[i] = omegal[i];
    if(omegal1[i]>0.1){
      omegal1[i] = 0.1;
      //   ntyp++;
    }
  }

  std::vector< std::vector<double> > adev(ntyp);
  std::vector< std::vector<double> > edev(ntyp);
  std::vector< std::vector<double> > ydev(ntyp);
  std::vector< std::vector<double> > ahet(ntyp);
  std::vector< std::vector<double> > nhet(ntyp);
  std::vector< std::vector<double> > ehet(ntyp);
  std::vector< std::vector<double> > nuhet(ntyp);
  std::vector< std::vector<double> > part_dia(ntyp);
  std::vector< std::vector<double> > size_frac(ntyp);

  std::vector< std::vector<std::string> > oxyd(ntyp);

  std::vector<double> part_den;
  std::vector<double> omega_srf;
  std::vector<double> omega_char;
  std::vector<double> omega_liq;
  std::vector<double> omega_ash;
  std::vector<double> type_frac;
  std::vector<double> start_time;

  int it  = -1;
  int it1 = -1;
  int nt;

  double tot_ash_flow = 0.0;

  for(i=0; i<ni; i++){

    nt = 0;
    if(omegac[i]>0.0) nt++;
    if(omegah[i]>0.0) nt++;
    //if(omegal[i]>omegal1[i]) nt++;

    for(j=0; j<nt; j++){
      it1++;

      if(i<2) start_time.push_back(0.0);
      else    start_time.push_back(1.0e25);

      // devolatilization
      adev[it1].push_back(_devol_a1); edev[it1].push_back(_devol_e1), ydev[it1].push_back(_devol_y1);
      adev[it1].push_back(_devol_a2); edev[it1].push_back(_devol_e2), ydev[it1].push_back(_devol_y2);
      
      // oxidation
      ahet[it1].push_back(_oxid_a); nhet[it1].push_back(_oxid_n); ehet[it1].push_back(_oxid_e);
      oxyd[it1].push_back("O2"); nuhet[it1].push_back(2.0);
      
      // CO2 gasification
      ahet[it1].push_back(_co2gas_a); nhet[it1].push_back(_co2gas_n); ehet[it1].push_back(_co2gas_e);
      oxyd[it1].push_back("CO2"); nuhet[it1].push_back(1.0);
      
      // H2O gasification
      ahet[it1].push_back(_h2ogas_a); nhet[it1].push_back(_h2ogas_n); ehet[it1].push_back(_h2ogas_e);
      oxyd[it1].push_back("H2O"); nuhet[it1].push_back(1.0);
    }
    
    if(omegac[i]+omegaash[i]>0.0){
      it++;
      part_den.push_back(1300.0);
      
      type_frac.push_back(_slur_flow[i]*(omegac[i]+omegaash[i])/tot_part_flow);

      for(j=0; j<ndpo; j++){
	part_dia[it].push_back(dpo[j]*1.0e-6);
	size_frac[it].push_back(modpo[j]);
      }   

      omega_srf.push_back((100.0-wicAsh)/100.0);
      omega_char.push_back(0.0);
      omega_liq.push_back(0.0);
      omega_ash.push_back(wicAsh/100.0);

      tot_ash_flow += _slur_flow[i] * _coal_pct[i] / 100.0 * _ash_prox/100.0;
    }

    if(omegah[i]>0.0 || _char_flow[i]*_ash_in_char>0.0){
      it++;
      part_den.push_back(1300.0);
      
      type_frac.push_back(_char_flow[i]/tot_part_flow);

      for(j=0; j<ndpo; j++){
	part_dia[it].push_back(dpo_c[j]*1.0e-6);
	size_frac[it].push_back(modpo_c[j]);
      }   

      omega_srf.push_back(0.0);
      omega_char.push_back(_char_pct[i]/100.0);
      omega_liq.push_back(0.0);
      omega_ash.push_back(1.0 - _char_pct[i]/100.0);

      tot_ash_flow += _char_flow[i]*_ash_in_char;
    }

    /*if(omegal[i]>omegal1[i]){
      it++;
      omegal[i] -= omegal1[i];
      part_den.push_back(1000.0);
      
      type_frac.push_back(_slur_flow[i]*omegal[i]/tot_part_flow);
      for(j=ndpo-1; j<ndpo; j++){
      part_dia[it].push_back(dpo[j]*1.0e-6);
      size_frac[it].push_back(modpo[j]);
      }   
      omega_srf.push_back(0.0);
      omega_char.push_back(0.0);
      omega_liq.push_back(0.9999);
      omega_ash.push_back(0.0001);
      }*/
 
  }
  
  stream stm(*thm, pres0, istream0, frac_typ, fuel0, spc_nam, 
	     frac, hform, istream1, str_frac1, temp0);

  part_kinetics p_kin(type_frac, 
		      adev, edev, ydev, ahet, nhet, ehet, nuhet, oxyd, 
		      part_den, part_dia, size_frac, 
		      omega_srf, omega_char, omega_liq, omega_ash, 
		      _slur_temp[0], *thm, stm);
  
  std::vector<double> y0;
  std::vector<double> yscal;

  double x1 = 0.0;
  double x2;

  p_kin.initialize(x1, y0, yscal);
  sode_rk srk(y0, (double)0.0001);

  double eps  = (double)1.0e-6;
  double h1   = (double)1.0;
  double hmin = (double)1.0e-20;
  
  double mdot     = 0.0;
  double burnout  = 1.0;
  double qht      = 0.0;
  double qht0     = 0.0;
  double qht1     = 0.0;
  double qht2     = 0.0;
  double qht_ash1 = 0.0;
  double qht_ash2 = 0.0;

  bool geom_input = _specify_geom;
  bool do_design  = _design_mode;

  if(!geom_input)  burnout = _burnout_gui / 100;
  if(second_stage) burnout = (coal_m_daf[0] + coal_m_daf[1]) / 
		     (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2]);
  
  double fuel_flow;
  if(second_stage) fuel_flow = coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2];
  else             fuel_flow = coal_m_daf[0];
  
  for(i=0; i<nstr; i++) mdot += str_frac1[i];

  double temp_slag;

  double temp     = 300.0;
  double urf      = 0.2;
  double burnout0 = burnout;

  double loverd    = _LD;
  double diam      = _diameter;
  double leng1     = _length1;
  double leng2     = _length2;
  double vol1      = 0.25 * 3.141592654 * _diameter * _diameter * _length1;

  double wall_area = vol1 / _length1 + 3.141592654 * _diameter * _length1;
  if(!second_stage) wall_area += vol1/_length1;

  std::vector<double> comp;
  std::vector<double> mol;

  double tempold;
  double tmp_w;
  double aa, bb, cc;
  double t3;
  double sigma = 5.67e-8;
  double emis;
  double rwall_1 = 1.0;
  double rwall_2 = 1.0;

  double err_tmp;
  double fun;
  double fprime;

  int iter;
  int nspc  = thm->get_spec_nam().size();
  int niter = 10;
  
  if(geom_input){

    for(iter=0; iter<niter; iter++){

      if(second_stage)
	str_frac1[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2]) * burnout;
      else
	str_frac1[1] = coal_m_daf[0] * burnout;
      
      mdot = 0.0;
      for(i=0; i<nstr; i++) mdot += str_frac1[i];

      stream strm((const thermo)(*thm), pres0, istream0, frac_typ, fuel0, spc_nam, 
		  frac, hform, istream1, str_frac1, temp0);

      tempold = temp;
      
      equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
      
      int is;
      double mwt = 0.0;
      for(is=0; is<nspc; is++) mwt += mol[is]*thm->get_mwt()[is];
 
      double rho = strm.get_pres() / thm->get_rgas() / temp * mwt;
      
      if(second_stage)
	x2 = vol1 * rho / (coal_m_daf[0] + coal_m_daf[1] + h2o_m_liq[0] + h2o_m_liq[1]
			   + _ox_flow[0] + _ox_flow[1]);
      else
	x2 = vol1 * rho / (coal_m_daf[0] + h2o_m_liq[0] + _ox_flow[0]);
      
      for(it=0; it<ntyp; it++)
	if(start_time[it]>0.0)
	  p_kin.get_start_time()[it] = x2;
     
      p_kin.get_res_time1() = x2;
      
      // calculate wall temp and wall heat transfer
      
      emis    = _emis1;
      rwall_1 = _rwall1;
      
      aa = emis * sigma;
      bb = 1.0 / rwall_1;
      cc = aa * temp * temp * temp * temp + bb * _back_temp;
      
      int icnt = 0;
      
      err_tmp = 1.0;
      tmp_w   = _back_temp;
      
      while(fabs(err_tmp)>0.1 && icnt<500){
	icnt++;
	t3      = tmp_w*tmp_w*tmp_w;
	fun     = aa*t3*tmp_w + bb*tmp_w - cc;
	fprime  = 4.0*aa*t3 + bb;
	err_tmp = -fun/fprime;
	tmp_w   += err_tmp;
      }

      temp_slag = tmp_w;
      
      qht  = wall_area * aa  * (temp  * temp * temp * temp - tmp_w * tmp_w * tmp_w * tmp_w);
      qht1 = qht;
      
      qht_ash1 = (p_kin.get_ash_enth() - p_kin.get_ash_enth_init_1())*(fuel_flow + tot_ash_flow);
      
      qht += qht_ash1;
      qht  = 0.9 * qht0 + 0.1 * qht;
      qht0 = qht;
            
      p_kin.get_temp_gas()  = temp;
      p_kin.get_temp_gas1() = temp;
      p_kin.get_temp_gas2() = temp;

      burnout0 = burnout;

      p_kin.reset_burnedout();
      srk.integrate(x1, x2, eps, h1, hmin, yscal, p_kin);
      burnout = p_kin.get_burnout();
      //burnout = urf*burnout + (1.-urf)*burnout0;
      
      if(fabs(temp-tempold)<1.0) break;
    }  // for(iter
  }else{ // if(geom_input
    if(second_stage)
      str_frac1[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2])*burnout;
    else
      str_frac1[1] = coal_m_daf[0] * burnout;
    
    mdot      = 0.0;
    for(i=0; i<nstr; i++) mdot += str_frac1[i];
    //for(i=0; i<nstr; i++) cout << "str_frac " << i << " " << str_frac1[i] << endl;
    stream strm((const thermo)(*thm), pres0, istream0, frac_typ, fuel0, spc_nam, 
		frac, hform, istream1, str_frac1, temp0);
    tempold = temp;
    
    qht = 0.0;
    equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
    double enth_ad = thm->enthalpy_mix(comp,temp);
    double temp_st = 298.0;
    double enth_st = thm->enthalpy_mix(comp,temp_st);
    double heat_loss = _heatloss_gui1 / 100; // gui input
    qht = mdot*(enth_ad - enth_st)*heat_loss;
    equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
    cout << " temp " << temp << endl;
    
    double mwt = 0.0;
    int is;
    for(is=0; is<nspc; is++) mwt += mol[is]*thm->get_mwt()[is];
    temp_slag = temp;
    tmp_w = temp;
    if(do_design){
      x2 = (double)1.0e10;
      p_kin.set_desired_burnout(burnout);
      //cout << "1st Stage res time " << x2 << endl;
      for(it=0; it<ntyp; it++) if(start_time[it]>0.0) p_kin.get_start_time()[it] = x2;
      p_kin.get_res_time1() = x2;
      
      // calculate wall temp and wall heat transfer
      
      p_kin.get_temp_gas() = temp;
      p_kin.get_temp_gas1() = temp;
      p_kin.get_temp_gas2() = temp;
      srk.integrate(x1, x2, eps, h1, hmin, yscal, p_kin);
      
      double rho = strm.get_pres()/thm->get_rgas()/temp*mwt;
      if(second_stage)
	vol1 = x2*(coal_m_daf[0]+coal_m_daf[1]+h2o_m_liq[0]+h2o_m_liq[1]
		   +_ox_flow[0]+_ox_flow[1])/rho;
      else
	vol1 = x2*(coal_m_daf[0]+h2o_m_liq[0]+_ox_flow[0])/rho;
      cout << "RES TIME " << x2 << " rho " << rho << endl;
      if(!second_stage){
	diam = pow(vol1*4.0/3.141592654/loverd,0.33333333);
	leng1 = loverd*diam;
	cout << "diameter " << diam << " 1stage length " << leng1 << endl;
	wall_area = 3.141592654/2.0*diam*diam + 3.141592654*diam*leng1;
	emis = _emis1;
	tmp_w = pow(temp*temp*temp*temp - qht/wall_area/emis/sigma,0.25);
	temp_slag = tmp_w;
	if(qht) rwall_1 = (tmp_w - _back_temp)*wall_area/qht;
	cout << "wall resistance " << rwall_1 << " tmp_w " << tmp_w << endl;
      }
      
    } // if(do_design
    
    qht1 = qht;
  } // if(geom_input
  if(second_stage)
    str_frac1[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2])*burnout;
  else
    str_frac1[1] = coal_m_daf[0] * burnout;
  mdot      = 0.0;
  for(i=0; i<nstr; i++) mdot += str_frac1[i];
  //for(i=0; i<nstr; i++) cout << "str_frac " << i << " " << str_frac1[i] << endl;
  double temp1 = temp;
  
  // second stage
  if(second_stage){
    // stream 1 oxidant
    str_frac[0] = str_frac1[0] + str_frac2[0];
    
    // stream 3 liquid water
    str_frac[2] = str_frac1[2] + str_frac2[2];
    
    // stream 4 steam
    str_frac[3] = str_frac1[3] + str_frac2[3];
    if(geom_input){
      // vol2 is the volume of the second stage
      double vol2 = 0.25*3.141592654*_diameter*_diameter*_length2;
      double wall_area = vol2/_length1 + 3.141592654*_diameter*_length2;
      qht = 0.0;
      qht0 = 0.0;
      double x3;
      burnout = 1.0;
      burnout0 = burnout;
      niter = 10;
      for(iter=0; iter<niter; iter++){
	str_frac[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2]) * burnout;
	mdot      = 0.0;
	for(i=0; i<nstr; i++) mdot += str_frac[i];
	stream strm((const thermo)(*thm), pres0, istream0, frac_typ, fuel0, spc_nam, 
		    frac, hform, istream1, str_frac, temp0);
	tempold = temp;
	equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
	cout << "2nd stage iter " << iter << " temp " << temp << endl;

	double mwt = 0.0;
	int is;
	for(is=0; is<nspc; is++) mwt += mol[is]*thm->get_mwt()[is];
	double rho = strm.get_pres()/thm->get_rgas()/temp*mwt;
	x3 = x2 + vol2*rho/(coal_m_daf[0]+coal_m_daf[1]+coal_m_daf[2]+h2o_m_liq[0]+h2o_m_liq[1]+h2o_m_liq[2]
			    +_ox_flow[0]+_ox_flow[1]+_ox_flow[2]);
	//cout << "2nd Stage res time " << x3-x2 << endl;
	p_kin.get_res_time2() = x3;

	// calculate wall temp and wall heat transfer

	emis = _emis2;
	rwall_2 = _rwall2;

	aa = emis*sigma;
	bb = 1.0/rwall_2;
	cc = aa*temp*temp*temp*temp + bb*_back_temp;
	int icnt=0; err_tmp = 1.0; tmp_w = _back_temp;
	while(fabs(err_tmp)>0.1&&icnt<500){
	  icnt++;
	  t3 = tmp_w*tmp_w*tmp_w;
	  fun = aa*t3*tmp_w + bb*tmp_w - cc;
	  fprime = 4.0*aa*t3 + bb;
	  err_tmp = -fun/fprime;
	  tmp_w += err_tmp;
	}
	qht = wall_area*aa*(temp*temp*temp*temp - tmp_w*tmp_w*tmp_w*tmp_w);
	qht2 = qht;
	qht_ash2 = (p_kin.get_ash_enth() - p_kin.get_ash_enth_init_2())*(fuel_flow+tot_ash_flow);
	qht += qht_ash2;
        qht = 0.9*qht0 + 0.1*qht;
	qht0 = qht;

	p_kin.get_temp_gas() = p_kin.get_temp_gas1();
	p_kin.get_temp_gas2() = temp;
	burnout0 = burnout;
	p_kin.reset_burnedout();
	srk.integrate(x1, x3, eps, h1, hmin, yscal, p_kin);
	burnout = p_kin.get_burnout();
	burnout = urf*burnout + (1.-urf)*burnout0;
	if(fabs(temp-tempold)<1.0) break;
      }
      
      str_frac[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2]) * burnout;
      mdot = 0.0;
      for(i=0; i<nstr; i++) mdot += str_frac[i];
      
    }else{ // if(geom_input
      burnout = _burnout_gui/100.0; // desired burnout gui input
      str_frac[1] = (coal_m_daf[0] + coal_m_daf[1] + coal_m_daf[2])*burnout;
            
      mdot      = 0.0;
      for(i=0; i<nstr; i++) mdot += str_frac[i];
      //for(i=0; i<nstr; i++) cout << "str_frac " << i << " " << str_frac[i] << endl;
      stream strm((const thermo)(*thm), pres0, istream0, frac_typ, fuel0, spc_nam, 
		  frac, hform, istream1, str_frac, temp0);
      tempold = temp;
       
      qht = 0.0;
      equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
      //cout << " temp 1st " << temp << endl;
      double enth_ad = thm->enthalpy_mix(comp,temp);
      double temp_st = 298.0;
      double enth_st = thm->enthalpy_mix(comp,temp_st);
      double heat_loss = _heatloss_gui2 / 100; // gui input
      qht = mdot*(enth_ad - enth_st)*heat_loss;
      //cout << "QHT " << qht << endl;
      equil::equilibrium(*thm, strm, qht, burnout, mdot, temp, comp, mol);
      cout << " temp " << temp << endl;
    
      double mwt = 0.0;
      int is;
      for(is=0; is<nspc; is++) mwt += mol[is]*thm->get_mwt()[is];
      tmp_w = temp;
      if(do_design){
	double x3 = (double)1.0e10;
	p_kin.set_desired_burnout(burnout);
	//cout << "1st Stage res time " << x2 << endl;
	for(it=0; it<ntyp; it++) if(start_time[it]>0.0) p_kin.get_start_time()[it] = x2;
	p_kin.get_res_time1() = x2;
	
	// calculate wall temp and wall heat transfer
      
	p_kin.get_temp_gas() = p_kin.get_temp_gas1();
	p_kin.get_temp_gas2() = temp;
	srk.integrate(x1, x3, eps, h1, hmin, yscal, p_kin);
	
	double rho = strm.get_pres()/thm->get_rgas()/temp*mwt;
	double vol2 = (x3-x2)*(coal_m_daf[0]+coal_m_daf[1]+h2o_m_liq[0]+h2o_m_liq[1]
	       +_ox_flow[0]+_ox_flow[1])/rho; 
	cout << "RES TIME " << x3 << " rho " << rho << endl;
	diam = pow((vol1+vol2)*4.0/3.141592654/loverd,0.33333333);
        leng1 = vol1/3.141592654/diam;
	leng2 = loverd*diam;
        leng2 -= leng1;
	cout << "diameter " << diam << " 1stage length " << leng1 << " 2stage length " 
	     << leng2 << endl;
	// 1stage heat transfer
	wall_area = 3.141592654/4.0*diam*diam + 3.141592654*diam*leng1;
	emis = _emis1;
	tmp_w = pow(temp1*temp1*temp1*temp1 - qht1/wall_area/emis/sigma,0.25);
	if(qht1) rwall_1 = (tmp_w - _back_temp)*wall_area/qht1;
	cout << "1stage wall resistance " << rwall_1 << " tmp_w " << tmp_w << endl;
	// second state heat transfer
	wall_area = 3.141592654/4.0*diam*diam + 3.141592654*diam*leng2;
	emis = _emis2;
	tmp_w = pow(temp*temp*temp*temp - qht/wall_area/emis/sigma,0.25);
	if(qht) rwall_2 = (tmp_w - _back_temp)*wall_area/qht;
	cout << "2stage wall resistance " << rwall_2 << " tmp_w " << tmp_w << endl;
        qht2 = qht;

      } // if(do_design

    } // if(geom_input
  }// if(second_stage
      
   
  double CoeffA, CoeffB, visc;
  // critical ash viscosity temperature
  double ashcomp_red[5] = {ashcomp[0], ashcomp[1], ashcomp[3], ashcomp[4], ashcomp[5]};
  double sum = 0.0;
  for(i=0; i<5; i++) sum += ashcomp_red[i];
  for(i=0; i<5; i++) ashcomp_red[i] *= 100.0/sum;
  double alpha = ashcomp_red[0]/ashcomp_red[1], beta = 0.0;
  for(i=2; i<5; i++) beta += ashcomp_red[i];
  double tcr = 3452.0 - 519.2*alpha + 74.5*alpha*alpha - 67.8*beta + 0.86*beta*beta;
   
  slagvis(temp_slag, ashcomp, CoeffA, CoeffB, visc);

   // calculate cold gas efficiency

   double mwt = thm->mweight(mol);
   const std::map<std::string, int >& nam_spec = thm->get_nam_spec();
   std::map<std::string, int >::const_iterator CO, H2O, CO2, CH4, H, N, O, OH,
	   C2H2, HCN, NH3, NO, CS, H2S, COS, H2;
   
   double f33 = 0.0, h43 = 0.0, h42 = 0.0;
   CO = nam_spec.find("CO");
   if(CO!=nam_spec.end()){
	   f33 += -26.416*mol[(*CO).second];
	   h42 += -94.052*mol[(*CO).second];
	   h43 += -94.052*mol[(*CO).second];
   }
   H2O = nam_spec.find("H2O");
   if(H2O!=nam_spec.end()){
	   f33 += -57.798*mol[(*H2O).second];
	   h42 += -57.798*mol[(*H2O).second];
	   h43 += -68.317*mol[(*H2O).second];
   }
   CO2 = nam_spec.find("CO2");
   if(CO2!=nam_spec.end()){
	   f33 += -94.052*mol[(*CO2).second];
	   h42 += -94.052*mol[(*CO2).second];
	   h43 += -94.052*mol[(*CO2).second];
   }
   CH4 = nam_spec.find("CH4");
   if(CH4!=nam_spec.end()){
	   f33 += -17.889*mol[(*CH4).second];
	   h42 += (-94.052-2*57.798)*mol[(*CH4).second];
	   h43 += (-94.052-2*68.317)*mol[(*CH4).second];
   }
   H = nam_spec.find("H");
   if(H!=nam_spec.end()){
	   f33 += +52.1530*mol[(*H).second];
	   h42 += -57.798/2.*mol[(*H).second];
	   h43 += -68.317/2.*mol[(*H).second];
   }
   N = nam_spec.find("N");
   if(N!=nam_spec.end()) f33 += +113.0821*mol[(*N).second];
   O = nam_spec.find("O");
   if(O!=nam_spec.end()) f33 += +59.6108*mol[(*O).second];
   OH = nam_spec.find("OH");
   if(OH!=nam_spec.end()){
	   f33 += +9.3270*mol[(*OH).second];
	   h42 += -57.798/2.*mol[(*OH).second];
	   h43 += -68.317/2.*mol[(*OH).second];
   }
   C2H2 = nam_spec.find("C2H2");
   if(C2H2!=nam_spec.end()){
	   f33 += +54.194*mol[(*C2H2).second];
	   h42 += (-94.052*2.-57.798)*mol[(*C2H2).second];
	   h43 += (-94.052*2.-68.317)*mol[(*C2H2).second];
   }
   HCN = nam_spec.find("HCN");
   if(HCN!=nam_spec.end()){
	   f33 += +31.10*mol[(*HCN).second];
	   h42 += (-94.052-57.798/2.)*mol[(*HCN).second];
	   h43 += (-94.052-68.317/2.)*mol[(*HCN).second];
   }
   NH3 = nam_spec.find("NH3");
   if(NH3!=nam_spec.end()){
	   f33 += -10.96*mol[(*NH3).second];
	   h42 += -57.798*3./2.*mol[(*NH3).second];
	   h43 += -68.317*3./2.*mol[(*NH3).second];
   }
   NO = nam_spec.find("NO");
   if(NO!=nam_spec.end()) f33 += +21.6008*mol[(*NO).second];
   CS = nam_spec.find("C(GR)");
   if(CS!=nam_spec.end()){
    // f33 += +171.45*mol[(*CS).second];
    // h42 += -94.052*mol[(*CS).second];
    // h43 += (-94.052-68.317)*mol[(*CS).second];
   }
   H2 = nam_spec.find("H2");
   if(H2!=nam_spec.end()) {
     h42 += -57.798*mol[(*H2).second];
     h43 += -68.317*mol[(*H2).second];
   }
   H2S = nam_spec.find("H2S");
   if(H2S!=nam_spec.end()) {
     //f33 += -4.77*mol[(*H2S).second];
     //h42 += -57.798*mol[(*H2S).second];
     //h43 += -68.317*mol[(*H2S).second];
   }
   COS = nam_spec.find("COS");
   if(COS!=nam_spec.end()) {
     //f33 += -33.0793*mol[(*COS).second];
     //h42 += -94.052*mol[(*COS).second];
     //h43 += -94.052*mol[(*COS).second];
   }

   double geffi = -100.*(f33-h43)*4.184e6/mwt*mdot/fuel_flow/hhv_1;
   double geffi_lhv = -100.*(f33-h42)*4.184e6/mwt*mdot/fuel_flow/lhv_1;
   //cout << "numerator " << (f33-h42)*4.184e6/mwt*mdot << endl;
   //cout << "sg lhv " << (f33-h42)*4.184e6/mwt << endl;
   //cout << "sl hhv " << (f33-h43)*4.184e6/mwt << endl;
   //cout << "denominator " << -fuel_flow*lhv_1 << endl;
   //cout << "mdot " << mdot << endl;
   //cout << "fuel_flow " << fuel_flow << endl;

  //# Fill in gas out specie map and specie composition
  gas_out->specie.clear(); 
  gas_out->gas_composite.comp_specie.clear(); 
  gas_out->gas_composite.comp_specie.resize(nspc);
  for(i=0; i<nspc; i++) {
    gas_out->specie[thm->get_spec_nam()[i]] = i;
    gas_out->gas_composite.comp_specie[i] = mol[i];
  }

  //# HHV - WHAT GOES HERE
  gas_out->hh0.clear();
  gas_out->hh0.push_back(0.0); //# hho
  gas_out->hh0.push_back(0.0); //# hao
  gas_out->hh0.push_back(0.0); //# hwo
  gas_out->hh0.push_back(0.0); //# hco

  //# Clear out gas_cell (profile) data
  gas_out->gas_cell.clear();

  //# Gas composite vectors
  gas_out->gas_composite.icell.clear();
  gas_out->gas_composite.velocity.clear();
  gas_out->gas_composite.node_location.clear();
  for(i=0; i<3; i++) {
    gas_out->gas_composite.icell.push_back(0);
    gas_out->gas_composite.velocity.push_back(0);
    gas_out->gas_composite.node_location.push_back(0);
  }

  //# Gas composite scalars
  gas_out->gas_composite.area = 0;
  gas_out->gas_composite.eff = 0;
  gas_out->gas_composite.eta = 0;
  gas_out->gas_composite.chi = 0;
  gas_out->gas_composite.T = temp;
  gas_out->gas_composite.P = pres0;

  gas_out->gas_composite.M = 0;
  if(second_stage) {
    for(i=0; i<3; i++)
      gas_out->gas_composite.M += coal_m_daf[i]*burnout + h2o_m_liq[i] + _stm_flow[i] + _ox_flow[i];
  } else {
    gas_out->gas_composite.M += coal_m_daf[0]*burnout + h2o_m_liq[0] + _stm_flow[0] + _ox_flow[0];
  }
  
  gas_out->pressure_drop = 0;

  //# Aiolos
  gas_out->gas_composite.tar = 0;
  gas_out->gas_composite.soot = 0;
 
  //# Baghouse
  gas_out->CoalCal = 0;
  gas_out->AshCal = 0;
  gas_out->AshpH = 7;

  //# Particle
  if(geom_input){
    gas_out->gas_composite.mean_size = p_kin.get_dp_mean();
    gas_out->gas_composite.size_variance = p_kin.get_dp_var();
  }else{
    gas_out->gas_composite.mean_size = dp_mean*1.0e-6;
    gas_out->gas_composite.size_variance = dp_var*1.0e-12;
  }
  gas_out->gas_composite.T_particle = temp;
  gas_out->gas_composite.M_particle = fuel_flow*(1.0 - burnout)
  //         daf coal and daf recycled char^   
            + tot_ash_flow*(1.0-_slag_eff/100.0);
  //       ash^
  //cout << "fuel_flow " << fuel_flow << endl;
  //cout << "burnout " << burnout << endl;
  //cout << "tot_ash_flow " << tot_ash_flow << endl;
  //cout << "slag_eff " << _slag_eff << endl;
 
  //# Particle composition
  gas_out->particle.clear();
  gas_out->gas_composite.comp_particle.clear();
  gas_out->gas_composite.comp_particle.resize(4);

  gas_out->particle["ASH"]   = 0; gas_out->gas_composite.comp_particle[0] = 
					 tot_ash_flow*(1.0-_slag_eff/100.0)/gas_out->gas_composite.M_particle;

  gas_out->particle["CHAR"]  = 1; gas_out->gas_composite.comp_particle[1] =
					 fuel_flow*(1.0 - burnout)/gas_out->gas_composite.M_particle;
  
  gas_out->particle["COAL"]  = 2; gas_out->gas_composite.comp_particle[2] = 0;
  gas_out->particle["WATER"] = 3; gas_out->gas_composite.comp_particle[3] = 0;

  //# WIC composition
  gas_out->wics.clear();
  gas_out->comp_wics.clear();
  gas_out->wics["C"] = 0;  gas_out->comp_wics.push_back(wic_c / (100 - wicAsh));
  gas_out->wics["H"] = 1;  gas_out->comp_wics.push_back(wic_h / (100 - wicAsh));
  gas_out->wics["O"] = 2;  gas_out->comp_wics.push_back(wic_o / (100 - wicAsh));
  gas_out->wics["N"] = 3;  gas_out->comp_wics.push_back(wic_n / (100 - wicAsh));
  gas_out->wics["S"] = 3;  gas_out->comp_wics.push_back(wic_s / (100 - wicAsh));
  gas_out->wics["CL"] = 3; gas_out->comp_wics.push_back(wic_cl / (100 - wicAsh));

  double hhv_daf = _hhv / (1 - (_ash_prox + _proxH2O) / 100);
  
  _thermal_input = fuel_flow*hhv_daf*.00232587;

  summaries->insert_summary_val("Cold gas efficiency UNITS:%(HHV) FORMAT:12.2f", geffi);
  summaries->insert_summary_val("Cold gas efficiency UNITS:%(LHV) FORMAT:12.2f", geffi_lhv);
  summaries->insert_summary_val("Burnout UNITS:% FORMAT:12.2f", burnout*100);
  summaries->insert_summary_val("Output Temperature UNITS:K FORMAT:12.2f", temp);
  summaries->insert_summary_val("Thermal Output UNITS:MW FORMAT:12.2f", 
			       (f33-h43)*4.184/mwt*mdot);
  summaries->insert_summary_val("Thermal Input UNITS:MW FORMAT:12.2f", _thermal_input);
  if(do_design)
    summaries->insert_summary_val("Calculated Diameter UNITS:m FORMAT:12.4f",diam);
  if(!second_stage){
    if(do_design) {
      summaries->insert_summary_val("Calculated Length UNITS:m FORMAT:12.4f",leng1);
      summaries->insert_summary_val("Calculated Wall Thermal Resistance UNITS:m^2-K/W FORMAT:12.4f",rwall_1);
    }

    summaries->insert_summary_val("Wall Heat Transfer UNITS:W FORMAT:12.2f",qht1);
    //if(geom_input) summaries->insert_summary_val("Ash Heat Transfer UNITS:W FORMAT:12.2f",qht_ash1);
  }else{
    if(do_design) {
      summaries->insert_summary_val("Calculated 1st Stage Length UNITS:m FORMAT:12.4f",leng1);
      summaries->insert_summary_val("Calculated 1st Stage Wall Thermal Resistance UNITS:m^2-K/W FORMAT:12.4f",rwall_1);
      summaries->insert_summary_val("Calculated 2nd Stage Length UNITS:m FORMAT:12.4f",leng2);
      summaries->insert_summary_val("Calculated 2nd Stage Wall Thermal Resistance UNITS:m^2-K/W FORMAT:12.4f",rwall_2);
    }
    summaries->insert_summary_val("1st Stage Wall Heat Transfer UNITS:W FORMAT:12.2f",qht1);
    //if(geom_input) summaries->insert_summary_val("1st Stage Ash Heat Transfer UNITS:W FORMAT:12.2f",qht_ash1);
    summaries->insert_summary_val("2nd Stage Wall Heat Transfer UNITS:W FORMAT:12.2f",qht2);
    //if(geom_input) summaries->insert_summary_val("2nd Stage Ash Heat Transfer UNITS:W FORMAT:12.2f",qht_ash2);
  } 
  summaries->insert_summary_val("HHV UNITS:BTU/lb FORMAT:12.2f", (f33-h43)*3.96832/mwt/0.0022046226);
  summaries->insert_summary_val("LHV UNITS:BTU/lb FORMAT:12.2f", (f33-h42)*3.96832/mwt/0.0022046226);
  summaries->insert_summary_val("Slag Surface Temperature UNITS:K FORMAT:12.2f", temp_slag);
  summaries->insert_summary_val("Slag Viscosity UNITS:Pa-s FORMAT:12.2f", visc);
  summaries->insert_summary_val("Slag Critical Viscosity Temperature UNITS:K FORMAT:12.2f", tcr);
  summaries->insert_summary_val("Slag Flow UNITS:kg/sec FORMAT:12.2f", tot_ash_flow*_slag_eff/100.0);
}

////////////////////////////////////////////////////////////////////////////////////

void Gasifier0D::print_inputs ()
{
  int i;

  for(i=0; i<3; i++) {
    cout << "O2 Temp [" << i << "]   " << _ox_temp[i] << endl;
    cout << "O2 Flow [" << i << "]   " << _ox_flow[i] << endl;
    cout << "Stm Temp [" << i << "]  " << _stm_temp[i] << endl;
    cout << "Stm Flow [" << i << "]  " << _stm_flow[i] << endl;
    cout << "Slur Temp [" << i << "] " << _slur_temp[i] << endl;
    cout << "Slur Flow [" << i << "] " << _slur_flow[i] << endl;
    cout << "Coal Pct [" << i << "]  " << _coal_pct[i] << endl;
    cout << "Char Pct [" << i << "]  " << _char_pct[i] << endl;
    cout << "Char Flow [" << i << "] " << _char_flow[i] << endl;
  }
  
  cout << "char_size      " << _char_size << endl;
  cout << "char_sd       " << _char_sd << endl;
  cout << "ash_in_char   " << _ash_in_char << endl;

  cout << "wic_C         " << _wic_C << endl;
  cout << "wic_H         " << _wic_H << endl;
  cout << "wic_O         " << _wic_O << endl;
  cout << "wic_N         " << _wic_N << endl;
  cout << "wic_S         " << _wic_S << endl;
  cout << "wic_CL        " << _wic_CL << endl;
  cout << "ash_ult       " << _ash_ult << endl;
  cout << "ash_prox      " << _ash_prox << endl;
  cout << "proxH2O       " << _proxH2O << endl;
  cout << "proxVM        " << _proxVM << endl;
  cout << "proxFC        " << _proxFC << endl;
  cout << "hhv           " << _hhv << endl;

  cout << "comp1         " << _comp1 << endl;
  cout << "comp2         " << _comp2 << endl;
  cout << "comp3         " << _comp3 << endl;
  cout << "comp4         " << _comp4 << endl;
  cout << "comp5         " << _comp5 << endl;
  cout << "comp6         " << _comp6 << endl;
  cout << "comp7         " << _comp7 << endl;
  cout << "comp8         " << _comp8 << endl;
  cout << "comp9         " << _comp9 << endl;
  cout << "comp10        " << _comp10 << endl;
  cout << "comp11        " << _comp11 << endl;
  cout << "comp12        " << _comp12 << endl;

  cout << "devol_a1      " << _devol_a1 << endl;
  cout << "devol_a2      " << _devol_a2 << endl;
  cout << "devol_e1      " << _devol_e1 << endl;

  cout << "devol_e2      " << _devol_e2 << endl;
  cout << "devol_y1      " << _devol_y1 << endl;
  cout << "devol_y2      " << _devol_y2 << endl;

  cout << "oxid_a        " << _oxid_a << endl;
  cout << "oxid_n        " << _oxid_n << endl;
  cout << "oxid_e        " << _oxid_e << endl;

  cout << "co2gas_a      " << _co2gas_a << endl;
  cout << "co2gas_n      " << _co2gas_n << endl;
  cout << "co2gas_e      " << _co2gas_e << endl;

  cout << "h2ogas_a      " << _h2ogas_a << endl;
  cout << "h2ogas_n      " << _h2ogas_n << endl;
  cout << "h2ogas_e      " << _h2ogas_e << endl;

  cout << "size_50       " << _size_50 << endl;
  cout << "size_200      " << _size_200 << endl;

  cout << "stage         " << _stage << endl;

  cout << "press_drop    " << _press_drop << endl;

  cout << "LD            " << _LD << endl;
  cout << "diameter      " << _diameter << endl;
  cout << "length1       " << _length1 << endl;
  cout << "length2       " << _length2 << endl;

  cout << "rwall1        " << _rwall1 << endl;
  cout << "rwall2        " << _rwall2 << endl;
  cout << "emis1         " << _emis1 << endl;
  cout << "emis2         " << _emis2 << endl;
  
  cout << "back_temp     " << _back_temp << endl;
  cout << "slag_eff      " << _slag_eff << endl;

  cout << "heatloss_gui1 " << _heatloss_gui1 << endl;
  cout << "heatloss_gui2 " << _heatloss_gui2 << endl;
  cout << "burnout_gui   " << _burnout_gui << endl;

  cout << "specify_geom  " <<  _specify_geom << endl;
  cout << "design_mode   " <<  _design_mode << endl;

}

////////////////////////////////////////////////////////////////////////////////////

//c-------------------------------------------------------------------------------
//      subroutine slagvis(ashcomp,CoeffA,CoeffB)
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void Gasifier0D::slagvis (double temp, double* ashcomp, double& CoeffA, double& CoeffB, double& visc)
{
  //c    This program reads slag composition and an array of Weynman coefficients,
  //c    Call a subroutine to calculate viscosity coefficients and returns to the 
  //c    main program viscosity of slag at a given temperature. 
  //c        Temi M. Linjewile and Connie Senior/ - 23 May 2002
  //c-------------------------------------------------------------------------------
  //	implicit double precision (a-h,o-z)
  //	dimension ASHCOMP(12),Coeff1450(12,6),Coeff1500(12,6)
  int flag; // integer flag 
   
   
   
   
  //cout << "Ash Composition" << endl; //write (6,*) 'Ash Composition'
  //int i;
  //for(i=0; i<12; i++){ //do i=1,12
  //  cout << ashcomp[i] << endl; //write (6,*) ASHCOMP(I)
  //} // enddo
   
  //call viscosity(Ashcomp, coeff1450, coeff1500, temp, visc, flag,
  //&               CoeffA, CoeffB)
  viscosity(ashcomp, temp, visc, flag, CoeffA, CoeffB);
  
  return;

  //c     A conditional statement will be written to characterize the flag
  //c     if the temperature appears to be out of the range. Note that the viscosity 
  //c     calculation is only valid in the temperature range 1350 - 1550 C.
  if(flag<10){ // if(flag.lt.10) then
    cout << "ash viscosity model:" << endl; //write(6,*) 'ash viscosity model:'
    if(flag==1){ // if(flag.eq.1) then
      cout << "Model 3 is used" << endl; // write(6,*)'Model 3 is used'
    }else if(flag==2){ // elseif(flag.eq.2) then
      cout << "Model 5 is used" << endl; // write(6,*)'Model 5 is used'
    }else if(flag==3){ // elseif(flag.eq.3) then
      cout << "Model 6 is used" << endl; // write(6,*)'Model 6 is used'
    }else if(flag==4){ // elseif(flag.eq.4) then
      cout << "Model 8 is used" << endl; // write(6,*)'Model 8 is used'
    }else if(flag==5){ // elseif(flag.eq.5) then
      cout << "Model 9 is used" << endl; // write(6,*)'Model 9 is used'
    }else if(flag==6){ // elseif(flag.eq.6) then
      cout << "Model 11 is used" << endl; //write(6,*)'Model 11 is used'
    } // endif
      
    cout << endl; // write(6,*)
    cout << "The calculated coefficients A and B are:" << endl; //write(6,*) 'The calculated coefficients A and B are:'
    cout << CoeffA << " " << CoeffB << endl; //write(6,*)CoeffA,CoeffB
    cout << endl; // write(6,*)
    cout << "Slag Viscosity at " << temp << " K is " << visc << endl; // write (6,*) 'Slag Viscosity at', Temp,'is:',visc
      
  }else{
    if (flag==10){ // if ( flag.eq.10) then
      cout << "FeO composition > 15 %" << endl; // write(6,*) 'FeO composition > 15 %'
    }else if(flag==11){ // elseif (flag.eq.11) then
      cout << "Temperature is out of range" << endl; // write (6,*) 'Temperature is out of range'
    } // endif
    cout << "The model selection is out of range" << endl; // write(6,*)'The model selection is out of range!'
  } //endif
}

//c---Subroutine to Determine Weynmann constants and computing slag viscosity
//Subroutine Viscosity(Ashcomp,coeff1450,coeff1500,temp,visc,flag,
//&                     CoeffA,CoeffB)
//\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/\/

void Gasifier0D::viscosity (double* ashcomp, double& temp, double& visc, int& flag,
			     double& CoeffA, double& CoeffB)
{
  /*implicit real*8 (a-h,o-z)
    dimension ashcomp(12),coeff1450(12,6),coeff1500(12,6)
    dimension A1(12),A2(12)
    integer flag*/
  double a1[12], a2[12];
   
  //c    --------------------------------------------------------
  //c     Polynomial Coefficients at 1450 C
  double coeff1450[12][6] = {
    { /*DATA (COEFF1450( 1,J),J=1, 6) */
      -7.42027747E+03, -1.07889480E+03,  2.58020770E+04,
      -1.04201550E+03,  1.24505030E+05,  1.88515310E+05},
    { /*DATA (COEFF1450( 2,J),J=1, 6) */
      2.13386420E+04,  3.69255670E+03, -7.64251840E+04,
      2.05339590E+03, -4.03551170E+05, -5.66484640E+05},
    { /*DATA (COEFF1450( 3,J),J=1, 6) */
      -1.53950730E+05, -3.08439500E+03,  5.63279640E+04,
      -8.76749290E+02,  3.24128780E+05,  4.22723860E+05},
    { /*DATA (COEFF1450( 4,J),J=1, 6) */
      4.13381300E+04,  7.46139700E+03, -1.27827570E+05,
      4.87559690E+03, -5.53999610E+05, -8.66167830E+05},
    { /*DATA (COEFF1450( 5,J),J=1, 6) */
      -1.19059690E+05, -2.50087190E+04,  3.77432480E+05,
      -8.75780950E+03,  1.79907180E+06,  2.60422950E+06},
    { /*DATA (COEFF1450( 6,J),J=1, 6) */
      8.58225180E+04,  2.05922620E+04, -2.77330780E+05,
      2.96473750E+03, -1.44787870E+06, -1.94406860E+06},
    { /*DATA (COEFF1450( 7,J),J=1, 6) */
      -7.62380290E+04, -1.63369650E+04,  2.09403720E+05,
      -7.29130600E+03,  8.20981730E+05,  1.32480440E+06},
    { /*DATA (COEFF1450( 8,J),J=1, 6) */
      2.19392670E+05,  5.41384450E+04, -6.16193030E+05,
      1.13305200E+04, -2.67130520E+06, -3.98543360E+06},
    { /*DATA (COEFF1450( 9,J),J=1, 6) */
      -1.58012490E+05, -4.42584420E+04,  4.51253620E+05,
      -2.10416150E+03,  2.15421840E+06,  2.97629480E+06},
    { /*DATA (COEFF1450(10,J),J=1, 6) */
      4.65186070E+04,  1.15253460E+04, -1.13417370E+05,
      3.45785730E+03, -4.05226900E+05, -6.74539440E+05},
    { /*DATA (COEFF1450(11,J),J=1, 6) */
      -1.33696550E+05, -3.78982950E+04,  3.32533720E+05,
      -4.09227230E+03,  1.32122870E+06,  2.03047640E+06},
    { /*DATA (COEFF1450(12,J),J=1, 6) */
      9.61918950E+04,  3.08663940E+04, -2.42625530E+05,
      -6.60092530E+02, -1.06769810E+06, -1.51695800E+06}};
  //c
  //c     Polynomial Coefficients at 1500 C
  double coeff1500[12][6] =
  {{ /*DATA (COEFF1500( 1,J),J=1, 6) */
    -7.81732260E+03, -1.21671260E+03,  2.34984390E+04,
    -3.67026580E+02,  1.22277030E+05,  1.78849520E+05},
   { /*DATA (COEFF1500( 2,J),J=1, 6) */
     2.24141940E+04,  3.85090490E+03, -6.82805490E+04,
     8.02909100E+02, -3.93359920E+05, -5.34119960E+05},
   { /*DATA (COEFF1500( 3,J),J=1, 6) */
     -1.60712980E+04, -3.00521460E+03,  4.90633230E+04,
     -4.44139110E+02,  3.13512220E+05,  3.96182930E+05},
   { /*DATA (COEFF1500( 4,J),J=1, 6) */
     4.36155100E+04,  7.94440820E+03, -1.15387680E+05,
     2.03293750E+03, -5.46022280E+05, -8.20605460E+05},
   { /*DATA (COEFF1500( 5,J),J=1, 6) */
     -1.24887170E+05, -2.49951250E+04,  3.33359690E+05,
     -4.49688070E+03,  1.75903660E+06,  2.45006340E+06},
   { /*DATA (COEFF1500( 6,J),J=1, 6) */
     8.94158990E+04,  1.94401080E+04, -2.37929030E+05,
     2.52047810E+03, -1.40412150E+06, -1.81660980E+06},
   { /*DATA (COEFF1500( 7,J),J=1, 6) */
     -8.03967070E+04, -1.67345590E+04,  1.87367240E+05,
     -3.55678410E+03,  8.12125740E+05,  1.25253170E+06},
   { /*DATA (COEFF1500( 8,J),J=1, 6) */
     2.29890830E+05,  5.25425260E+04, -5.37914970E+05,
     7.85773640E+03, -2.62017770E+06, -3.73851590E+06},
   { /*DATA (COEFF1500( 9,J),J=1, 6) */
     -1.64371190E+05, -4.08809120E+04,  3.81052600E+05,
     -4.43300980E+03,  2.09480970E+06,  2.77062880E+06},
   { /*DATA (COEFF1500(10,J),J=1, 6) */
     4.90399090E+04,  1.14758290E+04, -1.00581000E+05,
     1.98863240E+03, -4.02374930E+05, -6.36000180E+05},
   { /*DATA (COEFF1500(11,J),J=1, 6) */
     -1.39987150E+05, -3.59921270E+04,  2.86786930E+05,
     -4.30581200E+03,  1.30022480E+06,  1.89765460E+06},
   { /*DATA (COEFF1500(12,J),J=1, 6) */
     9.99421320E+04,  2.80590470E+04, -2.01444000E+05,
     2.41680810E+03, -1.04121160E+06, -1.40559070E+06}};
     
  //c    
  //c    ------------------------------------------------------
  //c     Convert Fe2O3 to FeO
  ashcomp[3] *= 143.7/159.7; //Ashcomp(4) = Ashcomp(4)*(143.7/159.7)
  double AshModelSelector = ashcomp[3]; // Ashmodelselector = Ashcomp(4)
  //c     Normalize SiO2, Al2O3, CaO, and FeO to 100%.
  //c     Ashcomp1 is SiO2, Ashcomp2 is Al2O3, Ashcomp4 is FeO, Ashcomp5 is CaO.
  double AshCompSum = ashcomp[0] + ashcomp[1] + ashcomp[3] + ashcomp[4]; //Ashcompsum = Ashcomp(1) + Ashcomp(2) + Ashcomp(4) + Ashcomp(5)
  ashcomp[0] /= AshCompSum/100.0; //  Ashcomp(1) = (ashcomp(1)/Ashcompsum)*100.0
  ashcomp[1] /= AshCompSum/100.0; //  Ashcomp(2) = (ashcomp(2)/Ashcompsum)*100.0
  ashcomp[3] /= AshCompSum/100.0; //  Ashcomp(4) = (ashcomp(4)/Ashcompsum)*100.0
  ashcomp[4] /= AshCompSum/100.0; //  Ashcomp(5) = (ashcomp(5)/Ashcompsum)*100.0
         
  //c     Calculate moles of normalized ash constituents
  //c     Molecular weights: SiO2 = 60.09, Al2O3 = 101.96, Cao = 56.08,FeO = 71.85
  ashcomp[0] /= 60.09; //  Ashcomp(1) = ashcomp(1)/60.09
  ashcomp[1] /= 101.96; //  Ashcomp(2) = ashcomp(2)/101.96
  ashcomp[3] /= 71.85; // Ashcomp(4) = ashcomp(4)/71.85
  ashcomp[4] /= 56.08; // Ashcomp(5) = ashcomp(5)/56.08
  AshCompSum = ashcomp[0] + ashcomp[1] + ashcomp[3] + ashcomp[4]; // Ashcompsum = ashcomp(1) + ashcomp(2) + Ashcomp(4) + Ashcomp(5)
         
         //c     Calculation of ash constituent normalized mole fractions
  double xms = ashcomp[0]/AshCompSum; // xms = ashcomp(1)/ashcompsum
  double xma = ashcomp[1]/AshCompSum; // xma = ashcomp(2)/ashcompsum
  double xmf = ashcomp[3]/AshCompSum; // xmf = ashcomp(4)/ashcompsum
  double xmc = ashcomp[4]/AshCompSum; // xmc = ashcomp(5)/ashcompsum
         
         //c     Calculation of X and Y parameters for the viscosiy equation
  double x = xms/(xms + xma + xmc + xmf); // x = xms/(xms + xma + xmc + xmf)
  double y = (xmc + xmf)/(xma + xmc + xmf); // y = (xmc + xmf)/(xma + xmc + xmf)
         
         
         //c     Choose which model to use
  int j = (int)(AshModelSelector/2.5);
  flag = j+1;
  if(j<6){
    int i;
    for(i=0; i<12; i++){
      a1[i] = coeff1450[i][j];
      a2[i] = coeff1500[i][j];
    }
  }
  /*if(AshModelSelector<=2.5){ // IF (Ashmodelselector.LE.2.5) THE
    //c       (select Model 3) 
    for(i=0; i<12; i++){ // DO I = 1, 12
    a1[i] = coeff1450[i][0]; // A1(i) = coeff1450(i,1)
    a2[i] = coeff1500[i][0]; // A2(i) = coeff1500(i,1)
    } // enddo
    flag = 1; // flag = 1
         
    }else if(AshModelSelector>2.5&&AshModelSelector<=5.0){ // ELSEIF (Ashmodelselector.gt.2.5.and.Ashmodelselector.LE.5.0) THEN 
    //c      (Select Model 5)
    for(i=0; i<12; i++){ // DO I = 1, 12
    a1[i] = coeff1450[i][1]; // A1(i) = coeff1450(i,2)
    a2[i] = coeff1500[i][1]; // A2(i) = coeff1500(i,2)
    } // enddo
    flat = 2; //flag = 2
           
    }else if(AshModelSelector>5.0&&AshModelSelector<=7.5){ // ELSEIF (Ashmodelselector.gt.5.0.and.Ashmodelselector.LE.7.5) THEN 
    //c      (Select Model 6)
    for(i=0; i<12; i++){ // DO I = 1, 12
    a1[i] = coeff1450[i][2]; // A1(i) = coeff1450(i,3)
    A2(i) = coeff1500(i,3)
    enddo
    flag = 3
             
    ELSEIF(Ashmodelselector.gt.7.5.and.Ashmodelselector.LE.10.0) THEN 
    c      (Select Model 8)
    DO I = 1, 12
    A1(i) = coeff1450(i,4)
    A2(i) = coeff1500(i,4)
    enddo
    flag = 4
               
    ELSEIF (Ashmodelselector.gt.10.and.Ashmodelselector.LE.12.5) THEN 
    c      (Select Model 9)
    DO I = 1, 12
    A1(i) = coeff1450(i,5)
    A2(i) = coeff1500(i,5)
    enddo
    flag = 5
                 
    ELSEIF(Ashmodelselector.gt.12.5.and.Ashmodelselector.LE.15.0)THEN 
    c      (Select Model 11)
    DO I = 1, 12
    A1(i) = coeff1450(i,6)
    A2(i) = coeff1500(i,6)
    enddo
    flag = 6
                   
    ENDIF*/
         
  if(AshModelSelector>15.0){ //IF (Ashmodelselector.GT.15.0) flag = 10  !STOP 'Error: FeO Composition > 15%'
    flag = 10;
    cout << "Error: FeO Composition > 15%" << endl;
  }
         
  //c     Calculate viscosities at 1450 and 1500C using the Weynman Coefficients selected above.
         
  double eta1450 = a1[0]+a1[1]*y+a1[2]*y*y +a1[3]*x+a1[4]*x*y+a1[5]*x*y*y
    //eta1450 = A1(1)+A1(2)*y+A1(3)*y**2+A1(4)*x+A1(5)*x*y+A1(6)*x*y**2
    +a1[6]*x*x +a1[7]*x*x*y +a1[8]*x*x*y*y  +a1[9]*x*x*x
    //& +A1(7)*x**2+A1(8)*x**2*y+A1(9)*x**2*y**2+A1(10)*x**3
    +a1[10]*x*x*x*y+a1[11]*x*x*x*y*y;
  //& +A1(11)*x**3*y+A1(12)*x**3*y**2
         
  double eta1500 = a2[0]+a2[1]*y+a2[2]*y*y+a2[3]*x+a2[4]*x*y+a2[5]*x*y*y
    //eta1500 = A2(1)+A2(2)*y+A2(3)*y**2+A2(4)*x+A2(5)*x*y+A2(6)*x*y**2
    +a2[6]*x*x +a2[7]*x*x*y +a2[8]*x*x*y*y  +a2[9]*x*x*x
    //& +A2(7)*x**2+A2(8)*x**2*y+A2(9)*x**2*y**2+A2(10)*x**3
    +a2[10]*x*x*x*y+a2[11]*x*x*x*y*y;
  //& +A2(11)*x**3*y+A2(12)*x**3*y**2
         
         
  //c     Simultaneous Linear Equations
         
  double T1 = 1450.0+273.0;
  double T2 = 1500.0+273.0;
  double c1 = eta1450-log(T1); //c1 = eta1450-DLOG(T1)
  double c2 = eta1500-log(T2); //c2 = eta1500-DLOG(T2)
  double b1 = 1./(T1);
  double b2 = 1./(T2);
         
  //c     Calculation of Viscosity Coefficients
  double Alna = c1 - (b1*((c2-c1)/(b2-b1)));
  CoeffA = exp(Alna); // DEXP(Alna)
  CoeffB = (c2-c1)/(b2-b1);
         
  //c    Calculation of Slag Viscosity, Pa.s
         
  //if(temp<1623.0||temp>1823.0) { // IF (Temp.LT.1623.0.OR.Temp.GT.1823.) THEN
  //flag = 11;
  // cout << "Temperature is out of range" << endl; // Write (4,*) 'Temperature is out of range'
  //}else{ //ELSE
  double const1 = CoeffA*temp;
  double const2 = CoeffB/(temp);
         
         
  //cout << const1 << " " << const2 << endl; // write(*,*)const1,const2
  visc = const1*exp(const2);
  //} // ENDIF
  //c
  //return
  //end
}


