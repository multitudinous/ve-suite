// mfixDataHeaders.h
// Data structures for the mfix data and sp and restart files.
// Last revision: 01-26-04
// Version: 0.8

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

