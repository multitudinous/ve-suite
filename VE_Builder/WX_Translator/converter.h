/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *   - National Energy Technology Laboratory, www.netl.doe.gov
 *   - West Virginia Virtual Environments Laboratory, wvvel.csee.wvu.edu
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: converter.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

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

