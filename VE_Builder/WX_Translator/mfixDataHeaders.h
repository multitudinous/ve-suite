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
 * File:          $RCSfile: mfixDataHeaders.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

// Data structures for the mfix data and sp and restart files.

#ifndef MFIXDATAHEADERS_H
#define MFIXDATAHEADERS_H

#include <iostream>		
using namespace std;

struct spHead {	// SP* File Headers
	char		*version;
	char		*runName;
	int		nextRec;
	int		numRec;
	float		timeNow;
	int		nStep;
};

struct resHead { // Restart File Headers

	char			*version;
	char			*runName;
	int			idMonth;	//added by trubie.. i don't read these out of SP
	int			idDay;		//added by trubie.. i don't read these out of SP
	int			idYear;		//added by trubie.. i don't read these out of SP
	int			idHour;		//added by trubie.. i don't read these out of SP
	int			idMinute;	//added by trubie.. i don't read these out of SP
	int			idSecond;	//added by trubie.. i don't read these out of SP
	int			iMax;
	int			jMax;
	int			kMax;
	int			iMax2;
	int			jMax2;
	int			kMax2;
	int			ijkMax2;
	int			mMax;
	int			nscalar;	//number of scalars
	int			nrr;		//number of reaction rates
	int			*nMax;
	char			*coordinates;
	char			*description;
	double			*dx;
	double			*dy;
	double			*dz;
	int			*flag;
	int			*varSelectFlag;
	int			numVars;
};

struct mfixData { // Raw MFIX data
	float		*sp1Array1;

	float		*sp2Array1;
	float		*sp2Array2;

	float		*sp3Array1;
	float		*sp3Array2;
	float		*sp3Array3;

	float		**sp4Array1;
	float		**sp4Array2;
	float		**sp4Array3;

	float		**sp5Array1;

	float		*sp6Array1;
	float		**sp6Array2;
	float		*sp6Array2b;	// used instead of sp6Array2 if version <= 1.15

	float		**sp7Array1;
	float		***sp7Array2;

	float		**sp8Array1;

	float		**sp9Array1;

	float		**spaArray1;
};

#endif

