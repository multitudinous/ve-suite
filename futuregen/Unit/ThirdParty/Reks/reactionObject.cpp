#include "reactionObject.h"

using namespace std;

reactionObject::reactionObject() {

	fReversible = false;
	ford = -1;
	covSp = "";
	fStick = false;
	fCov   = false;
	fFord  = false;
	iStick = -1;

}

void reactionObject::outputData() {
	cout << "\n\n";
	cout << "\nfReversible: " << fReversible;
	cout << "\nfStick  " << fStick;
	cout << "\nfCov: " << fCov;
	cout << "\nfFord: " << fFord << endl;
	int i = 0;
		cout << "\nProduct Coefficients: ";
	for(i=0; i<rxnReacCoeffs.size(); i++) 
		cout << rxnReacCoeffs[i] << ' ' ;
	cout << "\nProduct Coefficients: ";
	for(i=0; i<rxnReacCoeffs.size(); i++)
		cout << rxnProdCoeffs[i] << ' ';
	cout << "\nRate Parameters:       " << rateParameters[0] << ' ' << rateParameters[1] << ' ' 
		 << rateParameters[2];
	if(fStick)
		cout << "\nGas Sp Ind for fStick: " << iStick;
	if(fCov) 
		cout << "\nCov Params: " << covSp << ' ' 
								 << covParams[0] << ' ' 
		                         << covParams[1] << ' '
								 << covParams[2] << ' ';
	if(fFord)
		cout << "\nSpecified FORD " << ford;
  
}

