// By David O. Lignell, 2003
// Chemkin style thermo db parser

#ifndef THERMO_DB_H
#define THERMO_DB_H

#include <string>
#include <fstream>
#include <iostream>
#include <vector>
#include <map>
#include <sstream>
#include <cstdlib>

#include "GlobalConst.h"

class thermo_db {

 public:

  std::ifstream input;                            // kinetic mechanism input file

  std::vector<std::string>            spNames;
  std::vector<std::string>            elNames;
  std::vector<std::vector<double> >   coeffs;
  std::vector<std::vector<int> >      atFormula;   // species, formula (in order of elNames, but
  std::vector<std::vector<double> >   tempRanges;  //    formula size may be less than elNames size
  std::vector<double>                 globalTempRange;
  std::vector<char>                   phase;
  std::map<std::string, int>          elMap;

  thermo_db() {} 

  bool readThermoDb(std::string thermo_db_file, 
	  std::vector<std::string> selectedSpeciesNames);
  void dump();
  
 private:
   
  // Internal members
  
  bool                      fSelectedSpecies;
  std::vector<std::string>  selectedSpecies;         // only used for fSelectedSpecies

  void getNextLine(std::ifstream &input, std::string &line);
  void assignFormula(std::string s1, int p1, std::vector<int> &formula);
  bool knownElement(const std::string& s1);           
};

#endif


// note: code doesn't check for isotopes
// does not allow unknown elements
