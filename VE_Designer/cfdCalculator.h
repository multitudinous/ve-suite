#ifndef CFD_CALCULATOR_H
#define CFD_CALCULATOR_H

#include <unistd.h>
#include <sys/types.h>
#include <sys/dir.h>

#include <fstream>
#include <cstdlib>
#include <iostream>
#include <cstdio>
#include <cmath>
#include <cstring>
#include <errno.h>

#include "cfdVeReadParam.h"
#include "cfdVeReadParamFactory.h"

//for single cfd case

#define Pi 3.1415926
#define Start_x 0.0
#define Start_y 0.0
#define Start_z 0.0
#define End_x   15.24
#define End_y   6.096
#define End_z	6.096
#define Radius_elbow 1.3
#define Diameter_pipe 0.65
#define Inlet_length  2.85
#define Max_elbow_angle 180.0
#define Min_elbow_angle 0.0

class cfdCalculator
{

   public:
      cfdCalculator(float *parameters);
      ~cfdCalculator(){};

      void makeSystemCallFile();
      void makeGambitJournalFile();
      void makeFluentJournalFile();
      void makeInteractiveFluentJournalFile();
      void runSystemCall();

      

   public:
      double step_L;
      std::ofstream SystemCallFile;
      std::ofstream WholeFluentInputFile;
      std::ofstream WholeJoulFile;
      char* JoulFileName ;
	   char* MeshFileName;
      char* DBSFileName ;
	   char* FluentDataFileName ;
	   char* FluentCasFileName ;
	   char* FluentDPMCasFileName ;
	   char* FluentDPMDataFileName ;
	   char* transferstring ;


   private:  
      double x1,y1; /*start point*/
      double x2,y2; /*end point*/
      double R_elbow,R_pipe;
      double L1;/*the length of the first pipe (before the first elbow)*/
      double L2;/*the length of the second pipe (between the two elbows)*/
      double L3;/*the length of the third pipe (after the second elbows)*/
      double L_before_orifice,L_orifice,L_after_orifice;
      double R_major,R_minor;
      double angle; /*the angle of the elbow*/	
      double angle_rad;	
      double Orifice_angle,Orifice_angle_rad;
      double A,B; /*middle variables in the formular to calculate L2 and L3*/	
      double transfer_x,transfer_y;

   private:
      bool interactiveGA;
      bool interactivedisplay;
      cfdVeReadParam* param_file;


};

#endif
