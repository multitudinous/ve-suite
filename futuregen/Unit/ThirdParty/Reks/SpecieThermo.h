// SpecieThermo.h: interface for the SpecieThermo class.
//
// Copyright 2002  Reaction Engineering International
// by Yang

//////////////////////////////////////////////////////////////////////

#ifndef SPECTHERMO_H
#define SPECTHERMO_H

#pragma warning(disable : 4786)
#include <string>
#include <fstream>
#include "StringParseUtil.h"
#include "ElementReader.h"

//this class holds information read from the thermo file
class REKS_specie_thermo{
public:
	string m_spec_name;
	string m_date;
	
	vector <AtomicFormula> m_atom_form;
	char m_phase;
	REAL m_low_temp;
	REAL m_high_temp;
	REAL m_common_temp;
	vector<AtomicFormula> m_atom_form_ext;
	REAL m_upper_temp_a[7];
	REAL m_lower_temp_a[7];
	REAL spec_wgt;
	REAL delta_H;
	//the index of the specie in the vector of the m_specs
	int index;
	//flag of if this is really setted
	bool set;

#ifdef REKS_BANFF
	REAL m_LJparams[4];
#endif
	 
	//This data member contain the list of reactions number ,
	//which is the index number of the reaction vector containing this spec
	//this list is built up through parsing reaction process
	//So at the rate computation stage, the species's rate can be computed
	//directly from this list instead of searching through all the reactions
	vector<int> related_reactions;
    
	//REC specie thermo constructor. Just user the initialize the LJparams;
	REKS_specie_thermo(){
#ifdef REKS_BANFF
	m_LJparams[0] = -1.0e+50;
#endif
	}

	bool operator==(const REKS_specie_thermo& spec_therm) const 
	{
		int i;
		if (m_spec_name!=spec_therm.m_spec_name)
		  {
		    //cout<<m_spec_name<<"                "<<spec_therm.m_spec_name<<endl;
		    return false;
		  }
		else
		//	for (i=0; i<m_atom_form.size(); i++)
		//	if ((m_atom_form[i].num!=spec_therm.m_atom_form[i].num)
		//		||(m_atom_form[i].symbol[0]!=spec_therm.m_atom_form[i].symbol[0])
		//		||(m_atom_form[i].symbol[1]!=spec_therm.m_atom_form[i].symbol[1]))
		//		return false;
		//if (m_phase!=spec_therm.m_phase)
		//	return false;
		return true;
	};
	
	//check if the elem is in the elements list
	bool check_elem(REKS_element_list elements);
	
	//check if the spec is already in the simple specie reader's list
	//this is because you don't need the specie not in the mechanism file's specie list
	bool check_spec(vector<string> specs);

	//get the spec_wgt of the specie from the elements list
	REAL get_spec_wgt(REKS_element_list elements);

	//add the reaction corresponding to the spec to the specie's reaction list
	void add_reaction(int rxn);

} ;

//A global function to get the pointer to the specie_thermo object when the name of the specie is known
REKS_specie_thermo * get_specp(string spec_name, vector<REKS_specie_thermo *> speclist);
int get_spec_index(string spec_name, vector<REKS_specie_thermo *> speclist);
#endif 


