#ifndef CoNvErTeR
#define CoNvErTeR
 
#include <string.h>
#include <iostream>
#include <math.h>
#include "mfixDataHeaders.h"


  extern double P_REF, P_SCALE, C_E, C_F, PHI, PHI_W;
  extern int DIM_IC, IM_BC, DIM_C, DIM_IS;
  extern int NEXT_RECA;
  extern double version_number;
  extern int IMIN1, JMIN1, KMIN1, IMAX1, JMAX1, KMAX1, IJMAX2, DIM_BC;
  extern double DT, XMIN, XLENGTH, YLENGTH, ZLENGTH;

  extern char (*c_name)[20];
  extern double *c, *dx, *dy, *dz, *d_p, *ro_s, *MW_g, *MW_s;
  extern int *nmax;
  extern char units[16], run_type[16];
  extern double ep_star, ro_go, mu_go, mw_avg;


int LoadMFIX(const char *projectName, int timestep, resHead *rsH, spHead *spH, mfixData *mfD);
int ReadResFile(char *filename, resHead *resInfo);	//returns 1 if file couldn't be opened
int ReadSPTimesteps(char *filename); //returns the number of timesteps in an sp file
int ReadSPFile(char *filename, int timestep, resHead *resInfo, spHead *spInfo, mfixData *data); //returns 1 if file couldn't be opened

void res_cleanup(resHead *resInfo);
void sp_cleanup(spHead *spInfo);


#endif

