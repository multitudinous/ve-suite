// ThermoReader.cpp: implementation of the ThermoReader class.
//
//////////////////////////////////////////////////////////////////////

#include "SpecieThermo.h"

//Class REKS_specie_thermo
bool REKS_specie_thermo::check_elem(REKS_element_list elements)
{
	char elem[3]={'\0', '\0', '\0'};
	string aelem;
	int i, j;
	bool found;
	REAL temp;
	REAL result=0;

	for (i=0; i<m_atom_form.size(); i++)
	{
		elem[0]= m_atom_form[i].symbol[0];
		elem[1]= m_atom_form[i].symbol[1];
		aelem=strtok(elem," ");
		temp = m_atom_form[i].num;
		found = false;
		for (j=0; j<elements.size(); j++)
			if (aelem==elements[j]->m_name)
			{
				found = true;
				break;
			};
		if (!found)
		{
			cerr<<"Error! Specie: "<<m_spec_name<<endl; 
			cerr<<"Element: "<<aelem<<" NOT FOUND in Elements list!"<<endl;
			return false;
		}
		result +=temp*elements[j]->m_atomic_weight;
	}
	
	spec_wgt=result;
	return true;
}

bool REKS_specie_thermo::check_spec(vector<string> specs)
{
	int i;
	for (i=0; i<specs.size(); i++)
		if (m_spec_name==specs[i])
			return true;
	return false;
}

REAL REKS_specie_thermo::get_spec_wgt(REKS_element_list elements)
{
  char elem[3]={'\0', '\0', '\0'};
  string aelem;
  REAL temp;
  REAL result=0;
  int i, j;
  bool found;
  
  for (i=0; i<m_atom_form.size(); i++)
    {
      elem[0]= m_atom_form[i].symbol[0];
      elem[1]= m_atom_form[i].symbol[1];
      aelem=strtok(elem," ");
      temp = m_atom_form[i].num;
      found = false;
      for (j=0; j<elements.size(); j++)
	if (aelem==elements[j]->m_name)
	  {
	    found = true;
	    break;
	  };
      if (!found)
	{
	  cerr<<"Error! Specie: "<<m_spec_name<<endl; 
	  cerr<<"Element: "<<aelem<<" NOT FOUND in Elements list!"<<endl;
	  return -1;
	}
      result +=temp*elements[j]->m_atomic_weight;
    }
  
  spec_wgt = result;
  return result;
}

void REKS_specie_thermo::add_reaction(int rxn)
{
  int i;

  for (i=0; i<related_reactions.size(); i++)
    if (related_reactions[i]==rxn)
      return;
  related_reactions.push_back(rxn);
}

///A global function

REKS_specie_thermo * get_specp(string spec_name, vector<REKS_specie_thermo *> speclist)
{
  int i;
  
  for (i=0; i<speclist.size(); i++)
    {
      if (((speclist[i]->m_spec_name)==spec_name)
	  &&(speclist[i]->set==true))
	return speclist[i];
    }
  return NULL;
}

int get_spec_index(string spec_name, vector<REKS_specie_thermo *> speclist)
{
  int i;
  
  for (i=0; i<speclist.size(); i++)
    {
      if (((speclist[i]->m_spec_name)==spec_name)
	  &&(speclist[i]->set==true))
	return i;
    }
  return -1;
}
