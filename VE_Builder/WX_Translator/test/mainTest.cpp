// mainTest.cpp
// Test app for UnstructuredGridWriter.cpp
// Inputs:
// Outputs:
// Last revision: 01-20-04
// Version: 0.1

#include <iostream>
#include <string>

#include "UnstructuredGridWriter.h"
#include "mfixDataHeaders.h"
#include "converter.h"

int main() {
	resHead resH;
	spHead spH;
	mfixData mfD;
	char *filename;

	// Load MFIX data
	filename= "sgsr1.RES";
	ReadResFile(filename, &resH);

	// Print the headers
	//cout << " -=The SP Header=- " << endl;
	//cout << " nextRec: " << spH.nextRec << endl;
	//cout << " numRec: " << spH.numRec << endl;
	//cout << " timeNow: " << spH.timeNow << endl;
	//cout << " nStep: " << spH.nStep << endl << endl;

	/*cout << " -=The RES Header=- " << endl;
	cout << " version: " << resH.version << endl;
	cout << " runName: " << resH.runName << endl;
	cout << " idMonth: " << resH.idMonth << endl;
	cout << " idYear: " << resH.idYear << endl;
	cout << " idMinute: " << resH.idMinute << endl;
	cout << " idSecond: " << resH.idSecond << endl;
	cout << " iMax: " << resH.iMax << endl;
	cout << " jMax: " << resH.jMax << endl;
	cout << " kMax: " << resH.kMax << endl;
	cout << " iMax2: " << resH.iMax2 << endl;
	cout << " jMax2: " << resH.jMax2 << endl;
	cout << " kMax2: " << resH.kMax2 << endl;
	cout << " ijkMax2: " << resH.ijkMax2 << endl;
	cout << " mMax: " << resH.mMax << endl;

	for (int i=0; i<resH.mMax; i++) {
		cout << " nMax: " << resH.nMax[i] << endl;
	}

	cout << " dx: ";
	for (int i=0; i<resH.iMax2; i++)
		cout << resH.dx[i] << " ";
	cout << endl;

	cout << " dy: ";
	for (int i=0; i<resH.jMax2; i++)
		cout << resH.dx[i] << " ";
	cout << endl;

	cout << " dz: ";
	for (int i=0; i<resH.kMax2; i++)
		cout << resH.dx[i] << " ";
	cout << endl;

	cout << " coordinates: " << resH.coordinates << endl; */

	resH.numVars = 12;

	resH.varSelectFlag = new int[resH.numVars];
	resH.varSelectFlag[0]=0;
	resH.varSelectFlag[1]=0;
	resH.varSelectFlag[2]=0;
	resH.varSelectFlag[3]=0;
	resH.varSelectFlag[4]=0;
	resH.varSelectFlag[5]=0;
	resH.varSelectFlag[6]=0;
	resH.varSelectFlag[7]=0;
	resH.varSelectFlag[8]=1; //dunno yet
	resH.varSelectFlag[9]=0;
	resH.varSelectFlag[10]=0;
	resH.varSelectFlag[11]=0;



	// Call UnstructuredGridWriter.cpp
//for (int i=0; i<=100; i++)
	UnstructuredGridWriter("sgsr1", 26, 63, 1, &resH, &spH, &mfD);

	return 0;
}

