#ifndef REACTIONOBJECT_H
#define REACTIONOBJECT_H

#include <vector>
#include <map>
#include <utility>
#include <iostream>
#include <string>

class reactionObject {

 public:
	std::vector<double> rxnReacCoeffs;         // vector of size species list of coefficients
	std::vector<double> rxnProdCoeffs;         // vector of size species list of coefficients
	std::vector<double> rateParameters;        // reaction line Arrhenius parameters ko B Ea/R
	double  ford;
	int iStick;                                // index of gas species for sticking rxns
	std::string covSp;
	std::vector<double> covParams;
	bool	fReversible;                          // true if reaction is reversible
	bool	fStick;
	bool	fCov;
	bool	fFord;

	reactionObject();
	void  outputData();

};


#endif
