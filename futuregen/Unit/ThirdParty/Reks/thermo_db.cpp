#pragma warning(disable: 4786)
// By David O. Lignell, 2003 
// Chemkin style thermo db parser


#include "thermo_db.h"


using namespace std;


////////////////////////////////////////////////////////////////////////////////

bool thermo_db::readThermoDb(string thermo_db_file, vector<string> selectedSpeciesNames) {
  
  string        line, s1;                         // temporary variables...
  stringstream  ss1;
  int           p1;
  int           i,j,k;
  double        d1;
  vector<int>   formula;
  vector<double> temps(3,0.0); 
  vector<double> coef(14,0.0);
  bool           fEof = false;                    // end of file flag?
  bool           fRepeat = false;                 // flag repeated species 
  vector<string> progressiveSpList;              

  cout << endl << endl << "Parsing Thermo DB";

  selectedSpecies = selectedSpeciesNames;
  fSelectedSpecies = true;
  coeffs.resize(selectedSpeciesNames.size());
  atFormula.resize(selectedSpeciesNames.size());
  tempRanges.resize(selectedSpeciesNames.size());
  phase.resize(selectedSpeciesNames.size());
  input.open(thermo_db_file.c_str());
  if(!input) {
    cerr << "\nError opening thermo db file: " << thermo_db_file;
    exit(0);
  }

  for(;;) {                                       // search for THERMO
    getNextLine(input, line);
    if(input.eof()) {
      cerr << "\n\nERROR, Thermo_db no data read\n" << endl; 
      return false;
    }
    ss1.clear();
    ss1.str(line);
    ss1 >> s1;
    if(s1 == "THERMO" || s1 == "THER") 
      break;
  }

  getNextLine(input, line);
  if(input.eof()) {
    cerr << "\n\nERROR, Thermo_db no data read\n" << endl; 
    return false; 
  }
  ss1.clear();
  ss1.str(line);
  globalTempRange.resize(3,298.15);
  ss1 >> globalTempRange[0] >> globalTempRange[2] >> globalTempRange[1];
  if(ss1.fail()) {
    cerr << "\n\nERROR Problem reading global Tlow, Tmid, Thigh\n" << endl;
    return false;
  }
  
  // READ IN INDIVIDUAL SPECIES THERMO DATA

  for(;;) {                            // read species set. 4 lines each

    ss1.clear();
    fRepeat = false;

    string            d_spNames;       // temporary vars to hold data
    vector<double>    d_coeffs;
    vector<double>    d_tempRanges;
    char              d_phase;

    // GET FIRST SPECIES LINE

    getNextLine(input, line);                 
	if(line.find("END") != string::npos) {   // hacked in for surf mech reader 
		spNames = selectedSpecies;
		cout << "\nThermo DB Read\n";
		return true;                         // (thermo data is in mech file, not standalone)
	}
    if(input.eof() && !fSelectedSpecies) 
      if(spNames.size()==0) {
		cerr << "\nERROR, No thermo db info read\n" << endl;
		return false;
      }
      else {
		cout << endl << endl << "Thermo DB Read\n";
		return true;
      }
    else if(input.eof()) {
      if(spNames.size() != selectedSpecies.size()) {
		cerr << "\nERROR, not all of input species list were read";
		return false;
      }
      else {
		spNames = selectedSpecies;               // get the right ordering
		cout << endl << endl << "Thermo DB Read\n";
		return true;
      }
    }

    // GET SPECIES NAMES

    ss1.clear();
    ss1.str(line.substr(0,18));
    ss1 >> s1;
    if(isdigit(s1[0])) { 
      cerr << "\n\nERROR, INVALID SPECIES NAME: " << s1 << endl << endl;
      return false;
    }
    
    // CHECK FOR REPEATED SPECIES

    for(k=0; k<spNames.size(); k++) 
      if(s1 == spNames[k]) {                      // species is a repeat
	fRepeat = true;
	cout << "\nSpecies " << s1 << " occurs more than once\n";
      }
    d_spNames = s1;   
    if(line.size() < 73) {
      cerr << "\nERROR, species line size too small for " << spNames[spNames.size()-1] << endl;
      return false;
    }
    
    // GET SPECIES FORMULA                      

    formula.assign(formula.size(),0);
    bool fReadOne = false;
    for(i=0; i<4; i++) {
      ss1.clear();
      s1 = line.substr(24+5*i,5);
      if(s1.find_first_not_of("\n\r\t\b\f\v ")==string::npos)
	if(i==3 && !fReadOne) {
	  cerr << "\nERROR, no elements for species " << d_spNames << endl;
	  return false;
	}
      else
	continue;
      ss1.str(s1);
      ss1 >> s1 >> p1;                         // el name, moles     
      if(!ss1.fail()) {
	if(p1 < 0) {
	  cerr << "\nERROR: Negative element in species " << d_spNames << endl;
	  return false;
	}
	if(s1.size() > 2){ 
	  cerr << "\nERROR: ELEMENT SIZE CAN BE ONLY 1 OR 2 CHARACTERS LONG\n"; 
	  return false;
	}
	fReadOne = true;
	if(!knownElement(s1)) {
	  cerr << "\nError: Element not in known list: " << s1;
	  return false;
	}
	assignFormula(s1, p1, formula);
      } 
      else if(isalpha(s1[0])) {
	cerr << "\nERROR: INVALID ELEMENT DATA FOR " << d_spNames << endl;
	return false;
      }
    }
    
    // GET SPECIES PHASE

    d_phase = line[44]; 
    //if(line[44] == ' ' || !(line[44]=='G'||line[44]=='L'||line[44]=='S')){
    //  cerr << "\nERROR, species phase for " << d_spNames << endl;
    //  return false;
    //}
    
    // GET TEMPERATURE RANGES   
    
    ss1.clear();
    for(i=0; i<3; i++){
      s1 = (line.substr(45+10*i,10));
      if(s1.find_first_not_of("\n\t\r\b ")==string::npos) {   // use default if not present
	temps[i] = globalTempRange[i];
	ss1.clear();
	continue;
      }
      ss1.str(s1);
      ss1 >> d1;
      if(ss1.fail()) {
	cerr << "\nERROR READING TEMP RANGE FOR SPECIES " << d_spNames << endl;
	return false; 
      }
      temps[i] = d1;
      ss1.clear();
    }
    
    // GET COEFFICIENTS

    coef.clear();
    for(i=0; i<3; i++) {         // three lines of input
      k = (i==2) ? 4 : 5;        // number of coefficients on the line
      getNextLine(input, line);
      if(input.eof()) {cout << "\n\nThermo_db no Coefficients \n" << endl; return false; }
      if(line.size() < 75 && k == 5 || line.size() < 60 && k == 4) {
	cerr << "\nERROR Coeffs line size too small for " << d_spNames << endl;
	return false;
      }
      for(j=0; j<k; j++) {
	ss1.str(line.substr(0+j*15, 15));
	ss1 >> d1;
	if(ss1.fail()) {
	  cerr << "\nERROR READING COEFFS FOR SPECIES " << d_spNames << endl;
	  return false;
	}
	coef.push_back(d1);
	ss1.clear();
      }
	}
    
    // Should we include the data: if fSelectedNames then check if curr sp is in selectedSpecies
    
    bool fIncludeSpecies = true;
    if(fSelectedSpecies) {
      fIncludeSpecies = false;
      for(i=0; i<selectedSpecies.size(); i++) 
		if(d_spNames == selectedSpecies[i]) {
			fIncludeSpecies = true;
			break;
		}
    }
    if(!fSelectedSpecies && !fRepeat) {
      coeffs.push_back(coef);
      tempRanges.push_back(temps);
      atFormula.push_back(formula);
      spNames.push_back(d_spNames);
      phase.push_back(d_phase);
    }
    else if(fIncludeSpecies == true && !fRepeat) {
      coeffs[i]     = coef;
      tempRanges[i] = temps;
      atFormula[i]  = formula;
      spNames.push_back(d_spNames);
      phase[i]      = d_phase;
    }
          
  }   // end loop through file    
  return true;                          // not reached
}

////////////////////////////////////////////////////////////////////////////////

void thermo_db::assignFormula(string s1, int p1, vector<int> &formula) {
  bool fNewEl = true;
  int i=0;
  map<string,int>::iterator mit;
  for(i=0; i<elNames.size(); i++)
    if(s1 == elNames[i]) {
      fNewEl = false;
      break;
    }
  if(fNewEl) {
    elNames.push_back(s1);
    elMap[s1] = elNames.size()-1;
    formula.push_back(0);
  }
  mit = elMap.find(s1);
  formula[mit->second] = p1;
  
}
////////////////////////////////////////////////////////////////////////////////

void thermo_db::getNextLine(ifstream &input, string &line) {
  // GET NEXT NON COMMENT, NON BLANK LINE
  string::size_type i;
  int j;
  while(!input.eof()) {
    getline(input, line);
    for(j=0; j<line.size(); j++)         // Make all uppercase
      if(isalpha(line[j]))
	line[j] = toupper(line[j]);
    i = line.find_first_not_of("\n\r\t\b\f\v ");
    if(i == string::npos)
      continue;
    if(line[i]!='!')
      break;
  }
  i = line.find('!');                    // eliminate trailing comment
  if(i != string::npos)
    line = line.substr(0, i);
}
////////////////////////////////////////////////////////////////////////////////
  
bool thermo_db::knownElement(const string& s1) {
  int i;
  for(i=0; i<CHART_LEN; i++) 
    if(s1 == periodic_chart_name[i])
      return true;
  return false;
}
////////////////////////////////////////////////////////////////////////////////

void thermo_db::dump() {
  int i = 0, j=0;
  cout << "\n\nRESULTS\n\n";
  for(i=0;i<spNames.size(); i++)
    cout << spNames[i] << endl;
  cout << endl;
  for(i=0;i<elNames.size(); i++)
    cout << elNames[i] << endl;
  cout << endl;
  for(i=0; i < atFormula.size(); i++) {
    cout << "\nspecies: " << spNames[i] << endl;
    for(j=0; j < atFormula[i].size(); j++)
      cout << "\t" << atFormula[i][j] << ' ';
  }
  cout << endl;
  cout << "\nphases: ";
  for(i=0; i < phase.size(); i++)
    cout << phase[i] << ' ';
  cout << "\n";
  
  for(i=0; i < coeffs.size(); i++) {
    cout << "\nnspeces: " << spNames[i] << endl;
    for(j=0; j < 5; j++)
      cout << coeffs[i][j] << ' ';
    cout << endl;
    for(j=5; j<10;j++)
      cout << coeffs[i][j] << ' ';
    cout << endl;
    for(j=10;j<coeffs[i].size();j++)
      cout << coeffs[i][j] << ' ';
    cout << endl;
  }
  cout << endl;
  
  for(i=0; i < tempRanges.size(); i++) {
    cout << endl << spNames[i] << endl;
    for(j=0; j < 3; j++)
      cout << tempRanges[i][j] << ' ';
    cout << endl;
  }
}
