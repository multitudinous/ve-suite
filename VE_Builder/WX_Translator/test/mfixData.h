// mfixDataHeaders.h
// Data structures for the mfix sp and restart files.
// Last revision: 01-20-04
// Version: 0.2

#ifndef MFIXDATAHEADERS_H	
#define MFIXDATAHEADERS_H	

#include <iostream>		
#include <vector>
#include <string>
using namespace std;

struct spHead {	// SP* File Headers
	string	version;
	string	runName;
	int		idMonth;
	int		idDay;
	int		idYear;
	int		idHour;
	int		idMinute;
	int		idSecond;
	int		nextRec;
	int		numRec;
	float	timeNow;
	int		nStep;
};

struct resHead { // Restart File Headers
	int iMax;
	int jMax;
	int kMax;
	int iMax2;
	int jMax2;
	int kMax2;
	int mMax;
	vector<int> nMax;
	string coordinates;
	vector<double> dx;
	vector<double> dy;
	vector<double> dz;
};

#endif