/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: converter.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

//	MFIX RES and SP* file reader
//	written by Trubie Turner (flexei@hotmail.com)
//	last modified 02/25/2004

#include "converter.h"
//#include "cgnslib.h"

double P_REF = 0.0, P_SCALE = 1.0, C_E =1.0, C_F = 0.0, PHI = 0.0, PHI_W = 0.0;
int DIM_IC = 5, IM_BC = 5, DIM_C = 5, DIM_IS = 5, DIM_USR = 5;
int NEXT_RECA, TOL_RESID_Scalar, DIM_SCALAR;
double version_number;
int IMIN1, JMIN1, KMIN1, IMAX1, JMAX1, KMAX1, JMAX2, KMAX2, IJMAX2, DIM_BC;
double DT, XMIN, XLENGTH, YLENGTH, ZLENGTH;

char (*c_name)[20];
double *c, *d_p, *ro_s, *MW_g, *MW_s;
char units[16], run_type[16];
double ep_star, ro_go, mu_go, mw_avg;

void sp_cleanup(spHead* spH) {

	delete [] spH->runName;
	spH->runName = NULL;
                
	delete [] spH->version;
	spH->version = NULL;
}


void byte_swap(char *buf, int size)
{
	char temp[32];
	int j = size-1;

	//temp = new char [size];

	for (int i = 0; i <size; i++)
	{
		temp[i] = buf[j];
		j--;
	}

	memcpy(buf, temp, size);
}

int populate_array(FILE *MFIXRESfile, double *darray, int size)
{
    int next_reca_inc =1, record_index = 0;
	for (int i = 0; i <size; i++)
	{
		fread(&darray[i], sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&darray[i], sizeof(double));

		if(record_index <63)
		{
			record_index += 1;
		}
		else
		{
			record_index = 0;
			next_reca_inc +=1;
		}
	}
	return next_reca_inc;
}

int populate_arrayi(FILE *MFIXRESfile, int *iarray, int size)
{
    int next_reca_inc =1, record_index = 0;
	for (int i = 0; i <size; i++)
	{
		fread(&iarray[i], sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&iarray[i], sizeof(int));
		if(record_index <127)
		{
			record_index += 1;
		}
		else
		{
			record_index = 0;
			next_reca_inc +=1;
		}
	}
	return next_reca_inc;
}

int populate_dummy_array(FILE *MFIXRESfile, int size) //double *darray, int size)
{
    int next_reca_inc =1, record_index = 0;
	for (int i = 0; i <size; i++)
	{
	//	fread(&darray[i], sizeof(double), 1, MFIXRESfile);
	//	byte_swap((char *)&darray[i], sizeof(double));

		if(record_index <63)
		{
			record_index += 1;
		}
		else
		{
			record_index = 0;
			next_reca_inc +=1;
		}
	}
	return next_reca_inc;
}

int populate_dummy_arrayi(FILE *MFIXRESfile, int size)//int *iarray, int size)
{
    int next_reca_inc =1, record_index = 0;
	for (int i = 0; i <size; i++)
	{
	//	fread(&iarray[i], sizeof(int), 1, MFIXRESfile);
	//	byte_swap((char *)&iarray[i], sizeof(int));
		if(record_index <127)
		{
			record_index += 1;
		}
		else
		{
			record_index = 0;
			next_reca_inc +=1;
		}
	}
	return next_reca_inc;
}

int LoadMFIX(const char *projectName, int timestep, resHead *rsH, spHead *spH, mfixData *mfD )
{

  char *filename;
  char extension[5];

 
  strncpy(extension, ".RES\0", 5);
  filename = (char *) malloc(sizeof(projectName));
  strncpy(filename, projectName, strlen(projectName));
  strncpy(filename+strlen(projectName), extension, 5);


  ReadResFile(filename, rsH);
 
 	strncpy(extension, ".SP1\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP2\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP3\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP4\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP5\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);
 
	strncpy(extension, ".SP6\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP7\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP8\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SP9\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

	strncpy(extension, ".SPA\0", 5);
	strncpy(filename+strlen(projectName), extension, 5);
	ReadSPFile(filename, timestep, rsH, spH, mfD);

  return 0;
  
}

int ReadResFile(char *filename, resHead *resInfo)
{
  FILE *MFIXRESfile;
  int next_reca_inc = 0;
  
  int byte_offset = 0;
  int temp_int = 10;
  int n_spx = 0;
  //float n_spx = 0;

  if((MFIXRESfile = fopen(filename, "rb")) == NULL)
  {
	  cout << "Cannot open MFIX RES file!" << endl;
	  getchar();
	  return 1;
  }
  else
  {
	  cout << "MFIX RES file opened" << endl;
  }

  resInfo->version = new char[512];
  fseek(MFIXRESfile, 0x0000, SEEK_SET); //record 1
  fread(resInfo->version, sizeof(char), 512, MFIXRESfile);
 
  byte_offset+= 512;
  fseek(MFIXRESfile, byte_offset, SEEK_SET); //record 2

  resInfo->runName = new char[60];
  fread(resInfo->runName, sizeof(char), 60, MFIXRESfile);
  fread(&resInfo->idMonth, sizeof(int), 1, MFIXRESfile);
  fread(&resInfo->idDay, sizeof(int), 1, MFIXRESfile); 
  fread(&resInfo->idYear, sizeof(int), 1, MFIXRESfile);
  fread(&resInfo->idHour, sizeof(int), 1, MFIXRESfile);
  fread(&resInfo->idMinute, sizeof(int), 1, MFIXRESfile);
  fread(&resInfo->idSecond, sizeof(int), 1, MFIXRESfile);

  byte_swap((char *)&resInfo->idMonth, sizeof(int));
  byte_swap((char *)&resInfo->idDay, sizeof(int));
  byte_swap((char *)&resInfo->idYear, sizeof(int));
  byte_swap((char *)&resInfo->idHour, sizeof(int));
  byte_swap((char *)&resInfo->idMinute, sizeof(int));
  byte_swap((char *)&resInfo->idSecond, sizeof(int));


 
  byte_offset+= 512;
   fseek(MFIXRESfile, byte_offset, SEEK_SET);//record 3

  fread(&NEXT_RECA, sizeof(int), 1, MFIXRESfile);
  byte_swap((char *)&NEXT_RECA, sizeof(int));

  byte_offset+= 512;
  fseek(MFIXRESfile, byte_offset, SEEK_SET); //record 4

  version_number = strtod(resInfo->version+7, (char **)resInfo->version+11);

//	if(strncmp(version, "RES = 01.00", 11) == 0)
	if(version_number == 01.00)
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);

		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));
		
	}
	else if((version_number == 01.01) || (version_number == 01.02))
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_BC, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);

		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DIM_IC, sizeof(int));
		byte_swap((char *)&DIM_BC, sizeof(int));
		byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));

	}
	else if(version_number == 01.03)
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_BC, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XMIN, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);

		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DIM_IC, sizeof(int));
		byte_swap((char *)&DIM_BC, sizeof(int));
		byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XMIN, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));

	}
	else if(version_number == 01.04)
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_BC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_C, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XMIN, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);

		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DIM_IC, sizeof(int));
		byte_swap((char *)&DIM_BC, sizeof(int));
		byte_swap((char *)&DIM_C, sizeof(int));
		byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XMIN, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));

	}
	else if(version_number == 01.05)
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_BC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_C, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IS, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XMIN, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);

		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DIM_IC, sizeof(int));
		byte_swap((char *)&DIM_BC, sizeof(int));
		byte_swap((char *)&DIM_C, sizeof(int));
		byte_swap((char *)&DIM_IS, sizeof(int));
		byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XMIN, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));

	}
	else
	{
		fread(&IMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&JMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&KMIN1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax, sizeof(int), 1, MFIXRESfile);
		fread(&IMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&JMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&KMAX1, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->iMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->jMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->kMax2, sizeof(int), 1, MFIXRESfile);
		fread(&IJMAX2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->ijkMax2, sizeof(int), 1, MFIXRESfile);
		fread(&resInfo->mMax, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_BC, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_C, sizeof(int), 1, MFIXRESfile);
		fread(&DIM_IS, sizeof(int), 1, MFIXRESfile);
		fread(&DT, sizeof(double), 1, MFIXRESfile);
		fread(&XMIN, sizeof(double), 1, MFIXRESfile);
		fread(&XLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&YLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&ZLENGTH, sizeof(double), 1, MFIXRESfile);
		fread(&C_E, sizeof(double), 1, MFIXRESfile);
		fread(&C_F, sizeof(double), 1, MFIXRESfile);
		fread(&PHI, sizeof(double), 1, MFIXRESfile);
		fread(&PHI_W, sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&IMIN1, sizeof(int));
		byte_swap((char *)&JMIN1, sizeof(int));
		byte_swap((char *)&KMIN1, sizeof(int));
		byte_swap((char *)&resInfo->iMax, sizeof(int));
		byte_swap((char *)&resInfo->jMax, sizeof(int));
		byte_swap((char *)&resInfo->kMax, sizeof(int));
		byte_swap((char *)&IMAX1, sizeof(int));
		byte_swap((char *)&JMAX1, sizeof(int));
		byte_swap((char *)&KMAX1, sizeof(int));
		byte_swap((char *)&resInfo->iMax2, sizeof(int));
		byte_swap((char *)&resInfo->jMax2, sizeof(int));
		byte_swap((char *)&resInfo->kMax2, sizeof(int));
		byte_swap((char *)&IJMAX2, sizeof(int));
		byte_swap((char *)&resInfo->ijkMax2, sizeof(int));
		byte_swap((char *)&resInfo->mMax, sizeof(int));
		byte_swap((char *)&DIM_IC, sizeof(int));
		byte_swap((char *)&DIM_BC, sizeof(int));
		byte_swap((char *)&DIM_C, sizeof(int));
		byte_swap((char *)&DIM_IS, sizeof(int));
		//byte_swap((char *)&DT, sizeof(double));
		byte_swap((char *)&XMIN, sizeof(double));
		byte_swap((char *)&XLENGTH, sizeof(double));
		byte_swap((char *)&YLENGTH, sizeof(double));
		byte_swap((char *)&ZLENGTH, sizeof(double));
		byte_swap((char *)&C_E, sizeof(double));
		byte_swap((char *)&C_F, sizeof(double));
		byte_swap((char *)&PHI, sizeof(double));
		byte_swap((char *)&PHI_W, sizeof(double));
	}
	
//	byte_offset = NEXT_RECA * 512;
	byte_offset += 512;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	if (version_number >= 1.04)
	{
	//	int record_index = 0;
//		c = new double [DIM_C];
//		c_name = new char [DIM_C][20];
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_C);

		byte_offset += 512 * next_reca_inc;
        fseek(MFIXRESfile, byte_offset, SEEK_SET);

		for (int i = 1; i <= DIM_C; i++)
		{
			//fseek(MFIXRESfile, byte_offset, SEEK_SET);	
			//fread(&c_name[i-1], sizeof(char), 20, MFIXRESfile);
			byte_offset += 512;
		}
		
	}
	fseek(MFIXRESfile, byte_offset, SEEK_SET);
	resInfo->nMax = new int [resInfo->mMax + 1];

	for (int i = 0; i <resInfo->mMax + 1; i++)
	{
		fread(&resInfo->nMax[i], sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nMax[i], sizeof(int));
	}

	byte_offset += 512;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->dx = new double [resInfo->iMax2];

	next_reca_inc = populate_array(MFIXRESfile, resInfo->dx, resInfo->iMax2);

	byte_offset += 512 * next_reca_inc;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->dy = new double [resInfo->jMax2];

	next_reca_inc = populate_array(MFIXRESfile, resInfo->dy, resInfo->jMax2);

	byte_offset += 512 * next_reca_inc;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->dz = new double [resInfo->kMax2];

	next_reca_inc = populate_array(MFIXRESfile, resInfo->dz, resInfo->kMax2);

	byte_offset += 512 * next_reca_inc;
    fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->description = new char [60];
	resInfo->coordinates = new char [16];

	fread(resInfo->runName, sizeof(char), 60, MFIXRESfile);
	fread(resInfo->description, sizeof(char), 60, MFIXRESfile);
	fread(units, sizeof(char), 16, MFIXRESfile);
	fread(run_type, sizeof(char), 16, MFIXRESfile);
	fread(resInfo->coordinates, sizeof(char), 16, MFIXRESfile);


	byte_offset +=512 ;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	d_p = new double [resInfo->mMax];
	ro_s = new double [resInfo->mMax];

	if (version_number == 1.00 || version_number == 1.01)
	{
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&d_p[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&ro_s[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		fread(&ep_star, sizeof(double), 1, MFIXRESfile);
		fread(&mu_go, sizeof(double), 1, MFIXRESfile);
		fread(&mw_avg, sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&ep_star, sizeof(double));
		byte_swap((char *)&mu_go, sizeof(double));
		byte_swap((char *)&mw_avg, sizeof(double));
	}
	else if(version_number == 1.02)
	{
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&d_p[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&ro_s[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		fread(&ep_star, sizeof(double), 1, MFIXRESfile);
		fread(&ro_go, sizeof(double), 1, MFIXRESfile);
		fread(&mu_go, sizeof(double), 1, MFIXRESfile);
		fread(&mw_avg, sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&ep_star, sizeof(double));
		byte_swap((char *)&ro_go, sizeof(double));
		byte_swap((char *)&mu_go, sizeof(double));
		byte_swap((char *)&mw_avg, sizeof(double));
	}
	else if(version_number == 1.03)
	{
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&d_p[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&ro_s[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		fread(&ep_star, sizeof(double), 1, MFIXRESfile);
		fread(&ro_go, sizeof(double), 1, MFIXRESfile);
		fread(&mu_go, sizeof(double), 1, MFIXRESfile);
		fread(&mw_avg, sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&ep_star, sizeof(double));
		byte_swap((char *)&ro_go, sizeof(double));
		byte_swap((char *)&mu_go, sizeof(double));
		byte_swap((char *)&mw_avg, sizeof(double));
	}
	else if(version_number >= 1.04)
	{
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&d_p[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		for (int i = 0; i <resInfo->mMax; i++)
		{
			fread(&ro_s[i], sizeof(double), 1, MFIXRESfile);
			byte_swap((char *)&d_p[i], sizeof(double));
		}
		fread(&ep_star, sizeof(double), 1, MFIXRESfile);
		fread(&ro_go, sizeof(double), 1, MFIXRESfile);
		fread(&mu_go, sizeof(double), 1, MFIXRESfile);
		fread(&mw_avg, sizeof(double), 1, MFIXRESfile);
		byte_swap((char *)&ep_star, sizeof(double));
		byte_swap((char *)&ro_go, sizeof(double));
		byte_swap((char *)&mu_go, sizeof(double));
		byte_swap((char *)&mw_avg, sizeof(double));	
	}
	

	byte_offset += 512;
//	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	//BEGIN THE SKIPPING STUFF MESS!!!//
	//old stuff with dummy int & double at bottom//

	
	if(version_number >= 1.04)
	{

		next_reca_inc = populate_dummy_array(MFIXRESfile, resInfo->nMax[0]);
		byte_offset += 512*next_reca_inc;
	
		byte_offset += 512*resInfo->mMax;
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
	}


	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_arrayi(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*3;

	if(version_number < 1.15)
	{
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
		byte_offset += 512*next_reca_inc;

			next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
			byte_offset += 512*next_reca_inc;
	}

	if(version_number >=1.04)
	{	
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
		byte_offset += 512*next_reca_inc*resInfo->nMax[0];
	}

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*3;


	for(int i = 1; i <=resInfo->mMax; i++)
	{
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
		byte_offset += 512*next_reca_inc*4;
		
		if(version_number >= 1.15)
		{
			next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
			byte_offset += 512*next_reca_inc;
		}
		if(version_number >=1.04)
		{
				for(int j = 1; j<= resInfo->nMax[i]; j++)
				{
					next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
					byte_offset += 512*next_reca_inc;
				}		
		}
	}

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_arrayi(MFIXRESfile,DIM_BC);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*3;


	if(version_number <1.15)
	{
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
		byte_offset += 512*next_reca_inc;

			next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
			byte_offset += 512*next_reca_inc;
	}
	if(version_number >=1.04)
	{	
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
		byte_offset += 512*next_reca_inc*resInfo->nMax[0];
	}

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*7;
	
	for(int i = 1; i <= resInfo->mMax; i++)
	{
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
		byte_offset += 512*next_reca_inc*3;

		if(version_number >= 1.04)
		{
			next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
			byte_offset += 512*next_reca_inc;

			if(version_number >= 1.15)
			{

				next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
				byte_offset += 512*next_reca_inc;

			}
			for(int j = 1; j<= resInfo->nMax[i]; j++)
			{
					next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
					byte_offset += 512*next_reca_inc;
			}
		}
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
		byte_offset += 512*next_reca_inc*2;
	}
	if(version_number == 1.0)
		byte_offset += 512*10;
	else
		byte_offset += 512*(DIM_BC);

	//END THE SKIPPING STUFF MESS TO GET FLAG VALUE//

	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->flag = new int [resInfo->ijkMax2];
	next_reca_inc = populate_arrayi(MFIXRESfile, resInfo->flag, resInfo->ijkMax2);

	byte_offset += 512*next_reca_inc;

/*	for(int i =0; i< resInfo->ijkMax2; i ++)
	{
		printf("%d ", resInfo->flag[i]);
	}*/

	//BACK TO SKIPPING STUFF TO TRY FOR THE NSCALAR AND NRR VALUES//
	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IS);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_arrayi(MFIXRESfile, DIM_IS);
	byte_offset += 512*next_reca_inc*6;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IS);
	byte_offset += 512*next_reca_inc*2;

	for(int i = 0; i <resInfo->mMax; i++)
	{
		next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IS);
		byte_offset += 512*next_reca_inc;
	}
	for(int i = 0; i < DIM_IS; i++)
	{
		byte_offset += 512;
	}
	byte_offset +=512;

	//version 1.09//
	if (version_number >= 1.5)
	{
		byte_offset +=512;
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&n_spx, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&n_spx, sizeof(int));
	}
	else
		n_spx = 9;

	byte_offset+=512;
	byte_offset += 512*n_spx;
	byte_offset += 512*(resInfo->mMax+1);

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_USR);
	byte_offset += 512*next_reca_inc*7;

	byte_offset += 512*DIM_USR;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*2;

	byte_offset += 512*DIM_IC;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*6;

	//version 1.1
	byte_offset += 512;
	//version 1.11
	byte_offset += 512;
	//version 1.12
	byte_offset += 1024;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*4;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_BC);
	byte_offset += 512*next_reca_inc*4*resInfo->mMax;

	byte_offset += 512;
	//version 1.14
	byte_offset += 512;
	//version 1.15
	byte_offset += 512;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*2;

	next_reca_inc = populate_dummy_array(MFIXRESfile, DIM_IC);
	byte_offset += 512*next_reca_inc*2*resInfo->mMax;

	//version 1.2
	byte_offset += 512;

	if (version_number >= 1.3 && version_number < 1.5)
	{
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&resInfo->nscalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nscalar, sizeof(int));
		resInfo->nrr = 0;
	}
	else if (version_number >= 1.5)
	{
		fseek(MFIXRESfile, byte_offset, SEEK_SET); 
		fread(&resInfo->nscalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nscalar, sizeof(int));
		fread(&TOL_RESID_Scalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&TOL_RESID_Scalar, sizeof(int));
		fread(&DIM_SCALAR, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&DIM_SCALAR, sizeof(int));

		byte_offset += 512;	
	
		next_reca_inc = populate_dummy_arrayi(MFIXRESfile, DIM_SCALAR);
		byte_offset += 512*next_reca_inc;
	
		
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&resInfo->nrr, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nrr, sizeof(int));

	}
	else
	{
		resInfo->nscalar = 0;
		resInfo->nrr = 0;
	}


	free(c);
	free(c_name);
    fclose(MFIXRESfile);

   return 0;
}

int ReadSPTimesteps(char *filename)
{
	FILE *MFIXfile;
	int byte_offset = 0;
	int next_rec, num_rec, timesteps;
	int nstep = 0;


  if((MFIXfile = fopen(filename, "rb")) ==NULL)
  {
	  cout << "Cannot open MFIX SP file!" << endl;
	  return 1;
  }
  else
  {
	  cout << "MFIX SP file opened" << endl;
  }
  
  byte_offset += 1024;
  fseek(MFIXfile, byte_offset, SEEK_SET); //record 3
  
  
  fread(&next_rec, sizeof(int), 1, MFIXfile);
  byte_swap((char *)&next_rec, sizeof(int));
  fread(&num_rec, sizeof(int), 1, MFIXfile);
  byte_swap((char *)&num_rec, sizeof(int));


  timesteps = (next_rec - 4)/num_rec;

  fclose(MFIXfile);

  return timesteps-1;
}


int ReadSPFile(char *filename, int timestep, resHead *resInfo, spHead *spInfo, mfixData *data)
{
	FILE *MFIXfile;
	int byte_offset = 0, timestep_offset = 0;
	int timesteps;
	int float_count = 0, blocks_per_timestep = 0;



  if((MFIXfile = fopen(filename, "rb")) ==NULL)
  {
	  cout << "Cannot open MFIX SP file!" << endl;
	  return 1;
  }
  else
  {
	  cout << "MFIX SP file opened" << endl;
  }

   fseek(MFIXfile, 0x0000, SEEK_SET); //record 1

   spInfo->version = new char [512];
  
  fread(spInfo->version, sizeof(char), 512, MFIXfile);


  byte_offset+= 512;
  fseek(MFIXfile, byte_offset, SEEK_SET); //record 2

  spInfo->runName = new char [60];

  fread(spInfo->runName, sizeof(char), 60, MFIXfile);

  
  byte_offset += 512;
  fseek(MFIXfile, byte_offset, SEEK_SET); //record 3
  
  
  fread(&spInfo->nextRec, sizeof(int), 1, MFIXfile);
  byte_swap((char *)&spInfo->nextRec, sizeof(int));
  fread(&spInfo->numRec, sizeof(int), 1, MFIXfile);
  byte_swap((char *)&spInfo->numRec, sizeof(int));


  byte_offset += 512;
  fseek(MFIXfile, byte_offset, SEEK_SET);

  timesteps = (spInfo->nextRec - 4)/spInfo->numRec;
	
  if(resInfo->ijkMax2 % 128 == 0)
  {
	  blocks_per_timestep = resInfo->ijkMax2 / 128;
  }
  else
  {
	  blocks_per_timestep = (resInfo->ijkMax2 / 128) + 1;
  }
 

 
   if(memcmp(filename+(strlen(filename)-4), ".SP1", 4) == 0)
   {

		data->sp1Array1 = new float [resInfo->ijkMax2];

		timestep_offset = byte_offset + (timestep * (blocks_per_timestep +1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		
		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);


		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp1Array1[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp1Array1[i], sizeof(float));
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP2", 4) == 0)
   {

		data->sp2Array1 = new float [resInfo->ijkMax2];
		data->sp2Array2 = new float [resInfo->ijkMax2];

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*2) +1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		
		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp2Array1[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp2Array1[i], sizeof(float));
		}

		timestep_offset += blocks_per_timestep * 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp2Array2[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp2Array2[i], sizeof(float));
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP3", 4) == 0)
   {

		data->sp3Array1 = new float [resInfo->ijkMax2];
		data->sp3Array2 = new float [resInfo->ijkMax2];
		data->sp3Array3 = new float [resInfo->ijkMax2];

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*3) +1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		
		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp3Array1[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp3Array1[i], sizeof(float));
		}

		timestep_offset += blocks_per_timestep * 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp3Array2[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp3Array2[i], sizeof(float));
		}

		timestep_offset += blocks_per_timestep * 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for(int i = 0; i <resInfo->ijkMax2; i++)
		{
		  fread(&data->sp3Array3[i], sizeof(float), 1, MFIXfile);
		  byte_swap((char *)&data->sp3Array3[i], sizeof(float));
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP4", 4) == 0)
   {

		data->sp4Array1 = new float *[resInfo->mMax];
		data->sp4Array1[0]= new float[resInfo->mMax * resInfo->ijkMax2];
		for (int i=1;i<resInfo->mMax ;i++) {
			data->sp4Array1[i]=data->sp4Array1[i-1]+resInfo->ijkMax2;
		}

		data->sp4Array2= new float *[resInfo->mMax];
		data->sp4Array2[0]= new float[resInfo->mMax * resInfo->ijkMax2];
		for (int i=1;i<resInfo->mMax ;i++) {
			data->sp4Array2[i]=data->sp4Array2[i-1]+resInfo->ijkMax2;
		}

		data->sp4Array3= new float *[resInfo->mMax];
		data->sp4Array3[0]= new float[resInfo->mMax * resInfo->ijkMax2];
		for (int i=1;i<resInfo->mMax ;i++) {
			data->sp4Array3[i]=data->sp4Array3[i-1]+resInfo->ijkMax2;
		}

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*3*resInfo->mMax )+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		

		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for (int j = 0; j <resInfo->mMax; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp4Array1[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp4Array1[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);

			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp4Array2[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp4Array2[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);

			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp4Array3[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp4Array3[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP5", 4) == 0)
   {

		data->sp5Array1 = new float *[resInfo->mMax];
		data->sp5Array1[0]= new float[resInfo->mMax * resInfo->ijkMax2];
		for (int i=1;i<resInfo->mMax ;i++) {
			data->sp5Array1[i]=data->sp5Array1[i-1]+resInfo->ijkMax2;
		}

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*resInfo->mMax )+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		


		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for (int j = 0; j <resInfo->mMax; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp5Array1[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp5Array1[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP6", 4) == 0)
   {
		if (version_number > 1.15)
		{
			data->sp6Array1 = new float [resInfo->ijkMax2];
		
			data->sp6Array2 = new float *[resInfo->mMax];
			data->sp6Array2[0]= new float[resInfo->mMax * resInfo->ijkMax2];
			for (int i=1;i<resInfo->mMax ;i++) {
				data->sp6Array2[i]=data->sp6Array2[i-1]+resInfo->ijkMax2;
			}

			timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*(1+resInfo->mMax) )+1) * 512);
			fseek(MFIXfile, timestep_offset, SEEK_SET); 

			fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
			fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
			byte_swap((char *)&spInfo->timeNow, sizeof(float));
			byte_swap((char *)&spInfo->nStep, sizeof(int));

			


			timestep_offset += 512;
			fseek(MFIXfile, timestep_offset, SEEK_SET);

			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
				  fread(&data->sp6Array1[i], sizeof(float), 1, MFIXfile);
				  byte_swap((char *)&data->sp6Array1[i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
			fseek(MFIXfile, timestep_offset, SEEK_SET);

			for (int j = 0; j <resInfo->mMax; j++)
			{
				for(int i = 0; i <resInfo->ijkMax2; i++)
				{
				  fread(&data->sp6Array2[j][i], sizeof(float), 1, MFIXfile);
				  byte_swap((char *)&data->sp6Array2[j][i], sizeof(float));
				}

				timestep_offset += blocks_per_timestep * 512;
				fseek(MFIXfile, timestep_offset, SEEK_SET);
			}
		}
		else if(version_number <= 1.15)
		{
			data->sp6Array1 = new float [resInfo->ijkMax2];
			data->sp6Array2b = new float [resInfo->ijkMax2];
		
			timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*2)+1) * 512);
			fseek(MFIXfile, timestep_offset, SEEK_SET); 

			fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
			fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
			byte_swap((char *)&spInfo->timeNow, sizeof(float));
			byte_swap((char *)&spInfo->nStep, sizeof(int));

		

			timestep_offset += 512;
			fseek(MFIXfile, timestep_offset, SEEK_SET);

			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
				  fread(&data->sp6Array1[i], sizeof(float), 1, MFIXfile);
				  byte_swap((char *)&data->sp6Array1[i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
			fseek(MFIXfile, timestep_offset, SEEK_SET);

			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
				  fread(&data->sp6Array2b[i], sizeof(float), 1, MFIXfile);
				  byte_swap((char *)&data->sp6Array2b[i], sizeof(float));
			}

		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP7", 4) == 0)
   {

		data->sp7Array1 = new float *[resInfo->nMax[0]];
		data->sp7Array1[0]= new float[resInfo->nMax[0] * resInfo->ijkMax2];
		for (int i=1;i<resInfo->nMax[0] ;i++) {
			data->sp7Array1[i]=data->sp7Array1[i-1]+resInfo->ijkMax2;
		}
		
		data->sp7Array2 = new float **[resInfo->mMax];

		for(int i = 0; i <resInfo->mMax; i++)
		{
			data->sp7Array2[i] = new float *[resInfo->nMax[i+1]];
			data->sp7Array2[i][0]= new float[resInfo->nMax[i+1] * resInfo->ijkMax2];
			for (int j=1;j<resInfo->nMax[i+1] ;j++) {
				data->sp7Array2[i][j]=data->sp7Array2[i][j-1]+resInfo->ijkMax2;
			}
		}


		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*resInfo->nMax[0]*resInfo->nMax[1]*resInfo->nMax[2])+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		
		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);


		for(int j = 0; j <resInfo->nMax[0]; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
				fread(&data->sp7Array1[j][i], sizeof(float), 1, MFIXfile);
				byte_swap((char *)&data->sp7Array1[j][i], sizeof(float));
			}
			timestep_offset += blocks_per_timestep * 512;
			fseek(MFIXfile, timestep_offset, SEEK_SET);
		}

		for(int z = 0; z <resInfo->mMax; z++)
		{
			for(int j = 0; j <resInfo->nMax[z+1]; j++)
			{
				for(int i = 0; i <resInfo->ijkMax2; i++)
				{
					fread(&data->sp7Array2[z][j][i], sizeof(float), 1, MFIXfile);
					byte_swap((char *)&data->sp7Array2[z][j][i], sizeof(float));
				}
				timestep_offset += blocks_per_timestep * 512;
				fseek(MFIXfile, timestep_offset, SEEK_SET);
			}
		}

   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP8", 4) == 0)
   {

		data->sp8Array1 = new float *[resInfo->mMax];
		data->sp8Array1[0]= new float[resInfo->mMax * resInfo->ijkMax2];
		for (int i=1;i<resInfo->mMax ;i++) {
			data->sp8Array1[i]=data->sp8Array1[i-1]+resInfo->ijkMax2;
		}

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*resInfo->mMax )+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));


		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for (int j = 0; j <resInfo->mMax; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp8Array1[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp8Array1[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SP9", 4) == 0)
   {

		data->sp9Array1= new float *[resInfo->nscalar];
		data->sp9Array1[0]= new float[resInfo->nscalar * resInfo->ijkMax2];
		for (int i=1;i<resInfo->nscalar ;i++) {
			data->sp9Array1[i]=data->sp9Array1[i-1]+resInfo->ijkMax2;
		}

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*resInfo->nscalar )+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		


		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for (int j = 0; j <resInfo->nscalar; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->sp9Array1[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->sp9Array1[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);
		}
   }
   else if(memcmp(filename+(strlen(filename)-4), ".SPA", 4) == 0)
   {

		data->spaArray1= new float *[resInfo->nrr];
		data->spaArray1[0]= new float[resInfo->nrr * resInfo->ijkMax2];
		for (int i=1;i<resInfo->nrr ;i++) {
			data->spaArray1[i]=data->spaArray1[i-1]+resInfo->ijkMax2;
		}

		timestep_offset = byte_offset + (timestep * ((blocks_per_timestep*resInfo->nrr )+1) * 512);
		fseek(MFIXfile, timestep_offset, SEEK_SET); 

		fread(&spInfo->timeNow, sizeof(float), 1, MFIXfile);
		fread(&spInfo->nStep, sizeof(int), 1, MFIXfile);
		byte_swap((char *)&spInfo->timeNow, sizeof(float));
		byte_swap((char *)&spInfo->nStep, sizeof(int));

		

		timestep_offset += 512;
		fseek(MFIXfile, timestep_offset, SEEK_SET);

		for (int j = 0; j <resInfo->nrr; j++)
		{
			for(int i = 0; i <resInfo->ijkMax2; i++)
			{
			  fread(&data->spaArray1[j][i], sizeof(float), 1, MFIXfile);
			  byte_swap((char *)&data->spaArray1[j][i], sizeof(float));
			}

			timestep_offset += blocks_per_timestep * 512;
            fseek(MFIXfile, timestep_offset, SEEK_SET);
		}
   }
  fclose(MFIXfile);

  return 0;
}


/* COPY OF INEFFICEINT BLANK MEMORY WRITING SKIP CODE

  	if(version_number >= 1.04)
	{
		blank_double = new double [nmax[0]];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, nmax[0]);
		byte_offset += 512*next_reca_inc;
		free(blank_double);
		byte_offset += 512*resInfo->mMax;
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
	}

	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*6;
	free(blank_double);
	blank_int = new int [DIM_IC];
	next_reca_inc = populate_arrayi(MFIXRESfile, blank_int, DIM_IC);
	byte_offset += 512*next_reca_inc*6;
	free(blank_int);
	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*3;
	free(blank_double);

	if(version_number < 1.15)
	{
		blank_double = new double [DIM_IC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
		byte_offset += 512*next_reca_inc;
		free(blank_double);

			blank_double = new double [DIM_IC];
			next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
			byte_offset += 512*next_reca_inc;
			free(blank_double);
	}

	if(version_number >=1.04)
	{	
		blank_double = new double [DIM_IC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
		byte_offset += 512*next_reca_inc*nmax[0];
		free(blank_double);
	}

	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*3;
	free(blank_double);

	for(int i = 1; i <=resInfo->mMax; i++)
	{
		blank_double = new double [DIM_IC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
		byte_offset += 512*next_reca_inc*4;
		free(blank_double);
		
		if(version_number >= 1.15)
		{
			blank_double = new double [DIM_IC];
			next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
			byte_offset += 512*next_reca_inc;
			free(blank_double);
		}
		if(version_number >=1.04)
		{
				for(int j = 1; j<= nmax[i]; j++)
				{
					blank_double = new double [DIM_IC];
					next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
					byte_offset += 512*next_reca_inc;
					free(blank_double);
				}		
		}
	}

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*6;
	free(blank_double);

	blank_int = new int [DIM_BC];
	next_reca_inc = populate_arrayi(MFIXRESfile, blank_int, DIM_BC);
	byte_offset += 512*next_reca_inc*6;
	free(blank_int);

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*3;
	free(blank_double);

	if(version_number <1.15)
	{
		blank_double = new double [DIM_BC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
		byte_offset += 512*next_reca_inc;
		free(blank_double);

			blank_double = new double [DIM_BC];
			next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
			byte_offset += 512*next_reca_inc;
			free(blank_double);
	}
	if(version_number >=1.04)
	{	
		blank_double = new double [DIM_BC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
		byte_offset += 512*next_reca_inc*nmax[0];
		free(blank_double);
	}

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*7;
	free(blank_double);

	
	for(int i = 1; i <= resInfo->mMax; i++)
	{
		blank_double = new double [DIM_BC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
		byte_offset += 512*next_reca_inc*3;
		free(blank_double);

		if(version_number >= 1.04)
		{
			blank_double = new double [DIM_BC];
			next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
			byte_offset += 512*next_reca_inc;
			free(blank_double);
			if(version_number >= 1.15)
			{
				blank_double = new double [DIM_BC];
				next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
				byte_offset += 512*next_reca_inc;
				free(blank_double);
			}
			for(int j = 1; j<= nmax[i]; j++)
			{
					blank_double = new double [DIM_BC];
					next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
					byte_offset += 512*next_reca_inc;
					free(blank_double);
			}
		}
		blank_double = new double [DIM_BC];
		next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
		byte_offset += 512*next_reca_inc*2;
		free(blank_double);
	}
	if(version_number == 1.0)
		byte_offset += 512*10;
	else
		byte_offset += 512*(DIM_BC);

	//END THE SKIPPING STUFF MESS TO GET FLAG VALUE//

	fseek(MFIXRESfile, byte_offset, SEEK_SET);

	resInfo->flag = new int [resInfo->ijkMax2];
	next_reca_inc = populate_arrayi(MFIXRESfile, resInfo->flag, resInfo->ijkMax2);

	byte_offset += 512*next_reca_inc;

	for(int i =0; i< resInfo->ijkMax2; i ++)
	{
		printf("%d ", resInfo->flag[i]);
	}

	//BACK TO SKIPPING STUFF TO TRY FOR THE NSCALAR AND NRR VALUES//
	blank_double = new double [DIM_IS];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IS);
	byte_offset += 512*next_reca_inc*6;
	free(blank_double);

	blank_int = new int [DIM_IS];
	next_reca_inc = populate_arrayi(MFIXRESfile, blank_int, DIM_IS);
	byte_offset += 512*next_reca_inc*6;
	free(blank_int);

	blank_double = new double [DIM_IS];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IS);
	byte_offset += 512*next_reca_inc*2;
	free(blank_double);

	blank_double = new double [DIM_IS];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IS);
	byte_offset += 512*next_reca_inc*resInfo->mMax;
	free(blank_double); 

	byte_offset += 512*(DIM_IS);

	byte_offset +=512;

	//version 1.09//
	byte_offset +=512;
	fseek(MFIXRESfile, byte_offset, SEEK_SET);
	fread(&n_spx, sizeof(int), 1, MFIXRESfile);
	byte_swap((char *)&n_spx, sizeof(int));

	byte_offset+=512;
	byte_offset += 512*n_spx;
	byte_offset += 512*(resInfo->mMax+1);

	blank_double = new double [DIM_USR];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_USR);
	byte_offset += 512*next_reca_inc*7;
	free(blank_double); 

	byte_offset += 512*DIM_USR;

	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*2;
	free(blank_double); 

	byte_offset += 512*DIM_IC;

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*6;
	free(blank_double); 

	//version 1.1
	byte_offset += 512;
	//version 1.11
	byte_offset += 512;
	//version 1.12
	byte_offset += 1024;

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*4;
	free(blank_double); 

	blank_double = new double [DIM_BC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_BC);
	byte_offset += 512*next_reca_inc*4*resInfo->mMax;
	free(blank_double); 

	byte_offset += 512;
	//version 1.14
	byte_offset += 512;
	//version 1.15
	byte_offset += 512;

	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*2;
	free(blank_double); 

	blank_double = new double [DIM_IC];
	next_reca_inc = populate_array(MFIXRESfile, blank_double, DIM_IC);
	byte_offset += 512*next_reca_inc*2*resInfo->mMax;
	free(blank_double); 

	//version 1.2
	byte_offset += 512;

	if (version_number >= 1.3 && version_number < 1.5)
	{
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&resInfo->nscalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nscalar, sizeof(int));
		resInfo->nrr = 0;
	}
	else if (version_number >= 1.5)
	{
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&resInfo->nscalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nscalar, sizeof(int));
		fread(&TOL_RESID_Scalar, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&TOL_RESID_Scalar, sizeof(int));
		fread(&DIM_SCALAR, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&DIM_SCALAR, sizeof(int));

		byte_offset += 512;	
		blank_int = new int [DIM_SCALAR];
		next_reca_inc = populate_arrayi(MFIXRESfile, blank_int, DIM_SCALAR);
		byte_offset += 512*next_reca_inc;
		free(blank_int); 

		
		fseek(MFIXRESfile, byte_offset, SEEK_SET);
		fread(&resInfo->nrr, sizeof(int), 1, MFIXRESfile);
		byte_swap((char *)&resInfo->nrr, sizeof(int));

	}
	else
	{
		resInfo->nscalar = 0;
		resInfo->nrr = 0;
	}
	*/

