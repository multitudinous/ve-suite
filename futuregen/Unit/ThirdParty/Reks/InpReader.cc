// Copyright 2002  Reaction Engineering International
// by Yang

#include "InpReader.h"

REKS_inp_reader::REKS_inp_reader()
{
	is_spec_tau = true;
	rtol =  RTOL;
	atol = ATOL;
	deltT = 1.0E-3;
	rxn_time = 1.0;
        temp = 1400.0;
}

REKS_inp_reader::~REKS_inp_reader()
{
   int i;
   for(i=0; i<fuel.size(); i++) delete fuel[i];
   for(i=0; i<oxid.size(); i++) delete oxid[i];
}

int REKS_inp_reader::is_inp_keyw(string oword)
{ 
  char temp[256];
  int i;
  string word;
  
  strcpy(temp,oword.c_str());
  for (i=0; i<strlen(temp); i++)
    temp[i]=(char)toupper(temp[i]);
  temp[i]='\0';
  word = string(temp);
  
  if (match(word, "TEMP"))
    return 1; //gas temperature
  else if (match(word, "PRES"))
    return 2; //gas pressure
  else if (match(word, "TIME"))
    return 3; //reaction time
  else if (match(word, "DELT"))
    return 4; //output time slice
  else if (match(word, "REAC"))
    return 5; //reactants
  else if (match(word, "END"))
    return 6; //symbol for ending
  else if (match(word, "CONP"))
	return 7;
  else if (match(word, "CONT"))
	return 8;
  else if (match(word, "CONV"))
	return 9;
  else if (match(word, "TTIM"))
	return 10;
  else if (match(word, "FUEL"))
	return 11;
  else if (match(word, "OXID"))
	return 12;
  else if (match(word, "EQUI"))
	return 13;
  else if (match(word, "ATOL"))
	return 14;
  else if (match(word, "RTOL"))
	return 15;
  else if (match(word, "CUSTOM_TTIM"))
	return 16;
  else if (match(word, "LINEAR_TTIM"))
	return 17;
  else if (match(word, "TGIV"))
	return 18;
  else if (match(word, "ENGR"))
	return 19;
  else if (match(word, "TINL"))
	return 20;
  else if (match(word, "QLOSS"))
	return 21;
  else if (match(word, "TAU"))
	return 22;
  else if (match(word, "VOL"))
	return 23;
  else if (match(word, "FLRT"))
	return 24;
  else if (match(word, "LENGTH"))			// dol
    return 25;
  else if (match(word, "AREA"))				// dol
    return 26;
  else if (match(word, "CONST_DIST"))        // dol
    return 27;                       
   else if (match(word, "CONSP_DIST"))        // dol
    return 28;    
   else if (match(word, "T_SPEC_TIME"))
    return 29;
  else if (match(word, "TTSPECS_FILE"))
    return 30;
   else
    return 0; //not a key word
}

int REKS_inp_reader::parse_inp(std::ifstream & inp, vector<REKS_specie_thermo *> speclist, int &line_number)
{
  vector<string> toks;
  char buffer[BUFFER_MAX+1];
  int temp;
  REKS_specie_thermo *temp2;
  REKS_usr_specie * cur_sp;
  psr_spec * cur_psrsp;
  bool end_of_file;
  int i;

//some initialization code here to init the member variables
  psr_preset = 0;
  case_flag = -1;
  equi = 1.0;
  atol = ATOL;
  rtol = RTOL;
  
  the_gas.specs.clear();

  //YANG, Attemp to speed things up by matching the order of the thrm_specie and the usr_specie
  //Before the gas system read any species from the input file

  for (i=0; i<speclist.size(); i++)
    {
      cur_sp = new REKS_usr_specie(speclist[i]);
      cur_sp->set_mole_fraction(0.0);
      cur_sp->mygas = &the_gas;
      the_gas.specs.push_back(*cur_sp);
      delete cur_sp;
    }

  do 
    {
      end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
      line_number++;
      ignore_comments(buffer);
      if (get_token4(buffer, toks)>0)
	switch(is_inp_keyw(toks[0]))
	{
	case 1:
          temp = atof(toks[1].c_str());
	  the_gas.set_temperature(atof(toks[1].c_str()));
	  break;
	case 2:
	  the_gas.set_pressure(atof(toks[1].c_str())*patom);
	  break;
	case 3:
		rxn_time=atof(toks[1].c_str());
		break;
	case 4:
		deltT = atof(toks[1].c_str());
	    break;
	case 5:
	  if (psr_preset==2)
	  {
		cerr<<"Error! line number: "<<line_number<<endl; 
		cerr<<"The REAC keyword can't be used with FUEL/OXID set up case"<<endl;
		break;
	  }
	  psr_preset = 1;
	  temp = get_spec_index(toks[1],speclist);
	  if (temp==-1)
	    {
	      cerr<<"Error! Input file line number: "<<line_number<<endl;
	      cerr<<"Reactant: "<<toks[1]<<" Not found in the thermo database!"<<endl;
	      return -1;
	    }
	  
	  the_gas.specs[temp].set_mole_fraction(atof(toks[2].c_str()));
	  break;
	case 6:
	  goto psr_premix;
	  break;
	case 7:
	  case_flag = 0;
	  break;
	case 8:
	  case_flag = 1;
	  break;
	case 9:
	  case_flag = 2;
	  break;
	case 10:
	  case_flag = 3;
	  break;
	case 11:
	  if (psr_preset==1)
	  {
		cerr<<"Error! line number: "<<line_number<<endl; 
		cerr<<"The FUEL keyword can't be used with REAC set up case"<<endl;
		break;
	  }
	  psr_preset = 2;
	  temp2 = get_specp(toks[1],speclist);
	  if (temp2==NULL)
	    {
	      cerr<<"Error! Input file line number: "<<line_number<<endl;
	      cerr<<"Reactant: "<<toks[1]<<" Not found in the thermo database!"<<endl;
	      return -1;
	    }
	  cur_psrsp = new psr_spec();
	  cur_psrsp->mol_fraction = atof(toks[2].c_str());
	  cur_psrsp->specie = temp2;
	  cur_psrsp->calc_valence();
	  fuel.push_back(cur_psrsp);
	  break;
	case 12:
	  if (psr_preset==1)
	  {
		cerr<<"Error! line number: "<<line_number<<endl; 
		cerr<<"The OXID keyword can't be used with REAC set up case"<<endl;
		break;
	  }
	  psr_preset = 2;
	  temp2 = get_specp(toks[1],speclist);
	  if (temp2==NULL)
	    {
	      cerr<<"Error! Input file line number: "<<line_number<<endl;
	      cerr<<"Reactant: "<<toks[1]<<" Not found in the thermo database!"<<endl;
	      return -1;
	    }
	  cur_psrsp = new psr_spec();
	  cur_psrsp->mol_fraction = atof(toks[2].c_str());
	  cur_psrsp->specie = temp2;
	  cur_psrsp->calc_valence();
	  oxid.push_back(cur_psrsp);
	  break;
	case 13:
	  if (psr_preset==1)
	  {
		cerr<<"Error! line number: "<<line_number<<endl; 
		cerr<<"The EQUI keyword can't be used with REAC set up case"<<endl;
		break;
	  }
	  psr_preset = 2;
	  equi = atof(toks[1].c_str());
	  break;
	case 14:
	  atol = atof(toks[1].c_str());
	  break;
	case 15:
	  rtol = atof(toks[1].c_str());
	  break;
	case 16:
	  ttime_type = 1;
	  time_temp_profile_fname = toks[1];
	  break;
	case 17:
	  ttime_type = 0;
	  start_temp = atof(toks[1].c_str());
	  to_ttim = atof(toks[2].c_str());
	  slope_ttim = atof(toks[3].c_str());
	  break;
	case 18:
	  case_flag = 4;
	  break;
	case 19:
	  case_flag = 5;
	  break;
	case 20:
	  tinl = atof(toks[1].c_str());
	  break;
	case 21:
	  qloss = atof(toks[1].c_str());
	  break;
	case 22:
	  tau = atof(toks[1].c_str());
	  is_spec_tau = true;
	  break;
	case 23:
	  volume = atof(toks[1].c_str());
	  break;
	case 24:
	  is_spec_tau = false;
	  mass_flow_rate = atof(toks[1].c_str());
	  break;
	case 25:
	  rxr_length = atof(toks[1].c_str());
	  break;
	case 26:
	  rxr_Acs = atof(toks[1].c_str());
	  break;
	case 27:
	  case_flag = 6;
	  break;
	case 28:
	  case_flag = 7;
	  break;
	case 29:
	  case_flag = 9;
	  break;
	case 30:
	  for (i=1; i<toks.size()-1; i++)
	    radical_specs.push_back(toks[i]);
	  time_temp_profile_fname=toks[i];
	  break;
	  
	default:
	  return -1;
	}
      else
	;
    } while(!end_of_file);

psr_premix:
	if (psr_preset==2)
		premix(speclist);

	return 0;
}

void REKS_inp_reader::dump()
{
  int i;
  for (i=0; i<the_gas.specs.size(); i++)
    cout<<the_gas.specs[i].spec->m_spec_name<<"    "<<the_gas.specs[i].mole_fraction<<"    "<<the_gas.specs[i].spec_wgt<<endl;
  
}

void REKS_inp_reader::premix(vector<REKS_specie_thermo *> speclist)
{
	int i;
  
	REAL mmwF = 0.0;
	REAL mmwO = 0.0;
	REAL total_pos_valence_F = 0.0;
	REAL total_neg_valence_F = 0.0;
	REAL total_pos_valence_O = 0.0;
	REAL total_neg_valence_O = 0.0;
	REAL R;  
	REAL total_mass_F = 0.0;
	REAL total_mass_O = 0.0;
	REKS_usr_specie * cur_sp;
	int temp;

//calculate the mean molacule weight for both fuel and oxid
	for (i=0; i < fuel.size(); i++)
		mmwF+=(fuel[i]->mol_fraction*fuel[i]->specie->spec_wgt);
	for (i=0; i< oxid.size(); i++)
		mmwO+=(oxid[i]->mol_fraction*oxid[i]->specie->spec_wgt);
	
//calculate the mass fraction for both fuel and oxid and calc the positive and negtive valence
	
	for (i=0; i < fuel.size(); i++)
	{
		fuel[i]->mas_fraction = fuel[i]->mol_fraction*fuel[i]->specie->spec_wgt/mmwF;
		total_pos_valence_F += fuel[i]->mol_fraction*fuel[i]->positive_valence;
		total_neg_valence_F += fuel[i]->mol_fraction*fuel[i]->negtive_valence;
		total_mass_F += fuel[i]->mas_fraction;
	}
	for (i=0; i < oxid.size(); i++)
	{
		oxid[i]->mas_fraction = oxid[i]->mol_fraction*oxid[i]->specie->spec_wgt/mmwF;
		total_pos_valence_O += oxid[i]->mol_fraction*oxid[i]->positive_valence;
		total_neg_valence_O += oxid[i]->mol_fraction*oxid[i]->negtive_valence;
	}
	
	R = -(total_pos_valence_F-total_neg_valence_F)/(total_pos_valence_O-total_neg_valence_O);
	
	for (i=0; i<oxid.size(); i++)
	{
		oxid[i]->mas_fraction = oxid[i]->mas_fraction * R / equi;
		total_mass_O += oxid[i]->mas_fraction;
	}
	
	total_mass_F += total_mass_O;

	for (i=0; i<oxid.size(); i++)
	{	
		temp=get_spec_index(oxid[i]->specie->m_spec_name, speclist);
		the_gas.specs[temp].set_mass_fraction(oxid[i]->mas_fraction / total_mass_F);		
	}

	for (i=0; i<fuel.size(); i++)
	{	
		temp=get_spec_index(fuel[i]->specie->m_spec_name, speclist);
	  	the_gas.specs[temp].set_mass_fraction(fuel[i]->mas_fraction / total_mass_F);
	}

	the_gas.mean_mole_wgt_by_massf();
	for (i=0; i<the_gas.specs.size(); i++)
		the_gas.specs[i].massf2molef_by_MMW();
}

void psr_spec::calc_valence()
{
	char elem[3]={'\0', '\0', '\0'};
	string aelem;
	int i, j;
	bool found;
	REAL temp;
	
	negtive_valence=0;
	positive_valence=0;

	for (i=0; i<specie->m_atom_form.size(); i++)
	{
		elem[0]= specie->m_atom_form[i].symbol[0];
		elem[1]= specie->m_atom_form[i].symbol[1];
		aelem=strtok(elem," ");
		temp = specie->m_atom_form[i].num;
		found = false;
		for (j=0; j<VALENCE_LEN; j++)
			if (aelem==elem_valence[j])
			{
				found = true;
				break;
			};
		if (!found)
		{
			cerr<<"Error! Specie: "<<specie->m_spec_name<<endl; 
			cerr<<"Element: "<<aelem<<" DO NOT have a valence value!"<<endl;
			return ;
		}
		if (valence[j]>=0)
			positive_valence +=temp*valence[j];
		else
			negtive_valence += temp*(-valence[j]);
	}
	
	return ;
}
