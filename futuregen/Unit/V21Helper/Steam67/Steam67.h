/*  
    "T H E   1 9 6 7   A S M E   S T E A M   T A B L E S"
    
    Michael Lynn McGuire         7/2/85  --- ??/??/????
    
    Taken from:       "Formulations and Iterative Procedures for
    the Calculation of Properties of Steam" by
    R. B. McClintock (General Electric Co.)
    and G. J. Silvestri (Westinghouse Electric),
    The American Society of Mechanical
    Engineers, 1968.
    
    "Some Improved Steam Property Calculation
    Procedures", McClintock and Silvestri,
    Journal of Engineering for Power, April 1970. 

    ----------------------------------------------------------

    Michael Deloy Maguire      April 2002
    Reaction Engineering Intl.
    Converted to C++ class
*/

#ifndef REISTEAM67_H
#define REISTEAM67_H

#include <iostream>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

using namespace std;

class Steam67 {

 private:
  
  //  constants for steam properties 
  const double ALPHA0;
  const double ALPHA1;
  const double PCA;
  const double VCA;
  const double TCA;
  const double TZA;
  const double PVO1O;
  const double PVOT;
  const double I1;
  const double T1;
  const double TC;
  const double P1;
  const double PMIN;
  const double PMAX;
  const double P3MIN;
  const double V3MIN;
  const double V3MAX;
  const double TMIN;
  const double TMAX;
  const double T1MAX;
  const double T2MIN;
  const double T3MIN;
  const double T3MAX;
  const double HMIN;
  const double HMAX;
  const double SMIN;
  const double SMAX;
  const double XMIN;
  const double XMAX;
  const double VMIN;
  const double VMAX;
  const double AL0;
  const double AL1;
  const double AL2;
  const double S4MAX;
  const double H4MAX;
  
  // variables for building error messages
  int VPTX3_ALREADY_SET_0;
  int VPTX3_ALREADY_SET_100;
  int VPTX3_CALL;
  
  /*  
      The steam tables will set the variable major_error to TRUE when there
      is/was a problem.  A variable named major_error_type is also set with a
      code:
      
      major_error_type       problem
      ----------------       -------------------------------------------
      0                      no error
      1                      not enough variables were input
      2                      the input variables were out of range with
      each other
  */
  int MAJOR_ERROR;
  int MAJOR_ERROR_TYPE;
  
  // funtion prototypes for steam properties 
  double psl (double *temperature, double *dpdt);
  double p23t (double *temperature);
  double tsl (double *pressure);
  double shvpt1 (double *pressure, double *temperature, int option);
  double shvpt2 (double *pressure, double *temperature, int option);
  double shpvt3 (double *volume, double *temperature, int option);
  double vptx3 (double *pressure, double *temperature, double *quality);
  double shvptx (double *pressure, double *temperature, double *quality, int option);
  double x_pt (double *pressure, double *temperature);
  double xpshv (double *pressure, double *shv, int option);
  double xtshv (double *temperature, double *shv, int option);
  double tpshvx (double *tp, double *shv, double *quality, int option_shv, int option_tp);
  double phsd (double *h, double *s);
  double phsw (double *h, double *s);
  double phs (double *h, double *s);
  double vistv (double *temperature, double density);
  double critical_velocity_of_tpqhs_satp(double *temperature, double *pressure,
					 double *quality, double *enthalpy,
					 double *entropy, double *saturation_pressure);

 public:

  Steam67();
  ~Steam67();

  /*
    action VALUE      program work done
    ------------      -------------------------------------------------
    0                 calculate balance around temperature, pressure,
                                steam quality, enthalpy, entropy,
		                specific weight, saturation pressure,
                                saturation temperature, degrees superheat,
                                degrees subcooling
    1                 action 0 plus viscosity
    2                 action 1 plus critical velocity
  */
  int calculate (double *temperature, double *pressure, double *quality,
		 double *weight, double *enthalpy, double *entropy,
		 double *saturation_temperature,
		 double *saturation_pressure, double *degrees_superheat,
		 double *degrees_subcooling, double *viscosity,
		 double *critical_velocity, int action);
  
};

#endif



