#pragma warning(disable: 4786)
#include "surf_parser.h"

using namespace std;


///////////////////////////////////////////////////////////////////////////////////

surf_parser::surf_parser() : thermoReader()  {
	nGasSp = 53;
	nSurfSp = 10; //7;
}

///////////////////////////////////////////////////////////////////////////////////

bool surf_parser::parse_surf_mech(string mechFileName) {

	ifstream mechFile(mechFileName.c_str());
	if(!mechFile) {
		cerr << "\nError opening surface mechanism file";
		return false;
	}

	string line, s1;
	stringstream ss1;
	ss1.clear();

	// read species (surface and gas species participating in surface reactions

	getNextLine(mechFile, line);
	if(line.find("SITE") != string::npos) {
		for(;;) {
			getNextLine(mechFile, line);
			if(line.find("END") != string::npos || mechFile.eof())
				break;
			ss1.str(line);
			for(;;) {
				ss1 >> s1;
				if(ss1.fail()) {
					ss1.clear();
					break;
				}
				spNames.push_back(s1);
				speciesMap[spNames[spNames.size()-1]] = spNames.size()-1;
			}
		}
	}

	// read thermodynamic data
	
	mechFile.close();
	mechFile.clear();
	
	if( !thermoReader.readThermoDb(mechFileName, spNames) ) {
		cerr << "\nError reading thermo data for surface mech";
		return false;
	}
	thermoReader.input.close();
	//thermoReader.dump();

	// read reactions data

	mechFile.open(mechFileName.c_str());
	if(!mechFile) {
		cerr << "\nError opening surface mechanism file";
		return false;
	}
	
	for(;;) {
		getNextLine(mechFile, line);
		if(line.find("REACTIONS") != string::npos)
			break;
	}
	
	for(;;) {
		getNextLine(mechFile, line);
		if(line.find("END") != string::npos || mechFile.eof())
			break;
		if(line.find("=") != string::npos) {
			rxnsData.push_back(reactionObject());
			rxnsData[rxnsData.size()-1].rxnReacCoeffs.resize(spNames.size(), 0.0);
			rxnsData[rxnsData.size()-1].rxnProdCoeffs.resize(spNames.size(), 0.0);
			if(line.find(">") == string::npos)
				rxnsData[rxnsData.size()-1].fReversible = true;
			processReactionLine(line);
		}
		else
			processAuxilaryLine(line);
		//rxnsData[rxnsData.size()-1].outputData();
	}

	// read lj parameters

	vector<double> d1(3,0);
	vector<string> tempNames;
	vector<vector<double> > tempLjs;
	for(;;) {
		ss1.clear();
		getNextLine(mechFile, line);
		if(line.find("END") != string::npos || mechFile.eof())
			break;
		ss1.str(line);
		ss1 >> s1 >> d1[0] >> d1[1] >> d1[2];
		if(ss1.fail()) {
			cerr << "\nError Reading L.J. Parameters";
			return false;
		}
		tempNames.push_back(s1);
		tempLjs.push_back(d1);
	}
	 //reorder parameters
	if(tempNames.size() != nGasSp) {
		cerr << "\nError, wrong number of lj sets";
		return false;
	}
	int i,j;
	for(i=0; i<nGasSp; i++)
		for(j=0; j<tempNames.size(); j++)
			if(tempNames[j] == spNames[i]) {
				s_lj.push_back(tempLjs[j][0]);
				ek_lj.push_back(tempLjs[j][1]);
				delta_lj.push_back(tempLjs[j][2]);
				break;
			}
	if(s_lj.size() != tempNames.size()) {
		cerr << "\nError, reordering lj parameters";
		return false;
	}

    return true;

}

///////////////////////////////////////////////////////////////////////////////////

void surf_parser::getNextLine(ifstream& input, string& line) {
  string::size_type i;
  int j;
  while (!input.eof()) {
    getline(input, line);
    for(j=0; j<line.size(); j++)         // make all uppercase
      if(isalpha(line[j]))
		line[j] = toupper(line[j]);
    i = line.find_first_not_of("\n\r\t\b\f\v ");
    if(i == string::npos)                // skip blank lines
      continue;
    if(line[i] != '!')                   // skip full comment lines
      break;
  }
  i = line.find('!');                    // eliminate trailing comment
  if(i != string::npos)
    line = line.substr(0, i);
  for(i=0; i<line.size(); i++)           // get rid of /'s
	  if(line[i] == '/')
		  line[i] = ' ';
}	

//////////////////////////////////////////////////////////////////////////////////////

void surf_parser::processReactionLine(string s1) {
  // called by readReactions Data
  // recieves the reaction line, assigns reaction variables
  // returns exit condition (t or f) and an error message

  stringstream ss1;
  string::size_type pos;
  string s2;
  vector<string> strBin;
  double d1, d2, d3;
  int i, j;
  int irxn = rxnsData.size()-1;
  
  // get Arrhenius coefficients
  ss1.clear();
  ss1.str(s1);
  for(;;) {
    ss1 >> s2;
    if(ss1.fail()) break;
    strBin.push_back(s2);
  }
  ss1.clear();
  s2 = strBin[strBin.size()-1] + " " + strBin[strBin.size()-2] + " " + strBin[strBin.size()-3];
  ss1.str(s2);
  ss1 >> d1 >> d2 >> d3;
  rxnsData[irxn].rateParameters.push_back(d3);              // prexponential
  rxnsData[irxn].rateParameters.push_back(d2);              // power of T
  rxnsData[irxn].rateParameters.push_back(d1/8.31451);      // Ea/R activation energy (Ea assumed J/mol)

  // get the reaction portion
  s2 = "";
  for(i=0; i < strBin.size()-3; i++)
    s2 += strBin[i] + " ";                // s2 is now just the reaction
  
  // operate on the reaction

  for(;;) {                               // delete ws and < or >
    pos = s2.find_first_of("\n\r\t\b\f\v<> ");
    if(pos == string::npos) break;
    s2.erase(pos,1);
  }
  
  pos = s2.find('+');                    // delimit the reaction 
  while(pos != string::npos) {
    s2[pos] = ' ';
    pos = s2.find('+');
  }
  pos = s2.find('=');                    // the '=' IS present 
  s2.replace(pos,1," ");
  s2.insert(pos+1, "= ");

  vector<string> rxnSpecies;             // list of species in current reaction
  vector<double> rxnCoeffs;              // list of coefficients in curr rxn
  int iFirstProduct = 0;                 // position in local rxnSpecies vector
  ss1.clear();
  ss1.str(s2);
  for(;;) {                   
    ss1 >> s1;
    if(ss1.fail()) break;
    if(s1 == "=")
      iFirstProduct = rxnSpecies.size();
    else {
      rxnSpecies.push_back("");
      rxnSpecies[rxnSpecies.size()-1] = s1;         // species is aA, (coeffSpecies)
    }
  }
  for(i=0; i<rxnSpecies.size(); i++) {              // split aA into a and A
    int jmax = rxnSpecies[i].size();
    for(j=0; j < jmax; j++) {
      if(isalpha(rxnSpecies[i][j])) {
		s1 = rxnSpecies[i].substr(0, j);
		rxnSpecies[i] = rxnSpecies[i].substr(j, rxnSpecies[i].size()-j);
		break;
      }
    }
    if(s1 != "") {
      ss1.clear();   ss1.str(s1);   ss1 >> d1;
    }
    else 
      d1 = 1.0;
    rxnCoeffs.push_back(d1);
  }

  // assign coefficients
  map<string, int>::iterator itMap;
  for(i=0; i<rxnSpecies.size(); i++) {               // loop over current rxn species
	itMap = speciesMap.find(rxnSpecies[i]);          //^ these sps are known through flags (hv ignored)
      if(i >= iFirstProduct)
		rxnsData[irxn].rxnProdCoeffs[itMap->second] += rxnCoeffs[i];   // += allows sp+sp
      else
		rxnsData[irxn].rxnReacCoeffs[itMap->second] += rxnCoeffs[i];
  }


}

////////////////////////////////////////////////////////////////////////////////////////////
void surf_parser::processAuxilaryLine(string s1) {
	stringstream ss1;
	ss1.str(s1);
	string s2, s3;
	double d1, d2, d3;
	int irxn = rxnsData.size()-1;
	ss1 >> s2;
	if(s2 == "DUPLICATE") {
		ss1 >> s2;
		if(ss1.fail())
			return;
	}
	if(s2 == "FORD") {
		ss1 >> s3;
		rxnsData[irxn].fFord = true;      // assumes PT
		ss1 >> d1;
		rxnsData[irxn].ford = d1;
	}
	else if(s2 == "STICK")  {
		int i;
		for(i=0; i<spNames.size(); i++) 
			if(rxnsData[irxn].rxnReacCoeffs[i] != 0.0 && spNames[i].find("(S)") == string::npos) {
				 rxnsData[irxn].iStick = i;              // the second half is a hack (assumes surf sp has (S) in name)
				 break;
			}
		rxnsData[irxn].fStick = true;
		
	}
	else {                               // cov
		rxnsData[irxn].fCov = true;
		ss1 >> s2;
		rxnsData[irxn].covSp = s2;
		ss1 >> d1 >> d2 >> d3;
		rxnsData[irxn].covParams.push_back(d1);rxnsData[irxn].covParams.push_back(d2);
		rxnsData[irxn].covParams.push_back(d3);
	}

	// change units of k from cm, mole, s to m, mol, s.
	// only change for arrenius rates, not sticking coefficients
	
	int i;
	if(!rxnsData[irxn].fStick) {
		double nuGas = 0.0; 
		double nuSurf = 0.0;
		for(i=0; i<rxnsData[irxn].rxnReacCoeffs.size(); i++) {
			if(spNames[i].find("(S)") != string::npos)
				nuSurf += rxnsData[irxn].rxnReacCoeffs[i];
			else
				nuGas += rxnsData[irxn].rxnReacCoeffs[i];
		}
		rxnsData[irxn].rateParameters[0] /= ( pow(100.0, 3.0*nuGas + 2.0*nuSurf - 2.0) );
	}
}
