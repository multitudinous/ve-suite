// Reaction.cpp: implementation of the REKS_reaction class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#include "Reaction.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////
#define RXNTOL 0.001

REKS_reaction::REKS_reaction()
{
	int i;
	reactants.clear();
	products.clear();
	arrhe_params.A=0;
	arrhe_params.beta=0;
	arrhe_params.E=0;
	is_three_body = false;
	is_third_spec = false;
	third_specs.clear();
	thirdbodies.clear();
	is_fall_of = false;
	is_chem_act = false;
	fall_of.A = 0;
	fall_of.beta = 0;
	fall_of.E = 0;
	chem_act.A = 0;
	chem_act.beta = 0;
	chem_act.E = 0;
	is_Lindemann = false;
	is_Troe = false;
	troe_params.a=0;
	troe_params.T3star=-0;
	troe_params.Tstar=0;
	troe_params.T2star=-10E+10; //invalid value
	is_SRI = false;
	sri_params.a =0; sri_params.b=0; sri_params.c=0; sri_params.d=1; //default value
	sri_params.e=0; //default value
	is_LandauTeller = 0;
	lt_params.Bi=0;
	lt_params.Ci=0;
	if_spec_Eq79 = false;
	for (i=0; i<9; i++)
		eq79_bn[i] = 0;
	if_spec_Eq80 = false;
	for (i=0; i<4; i++)
		eq80_bn[i]=0;
	if_spec_HV = false;
	wave_length = 0;
	if_spec_temp_dep = false;
	temp_dep_specs.clear();
	if_spec_energy_loss = false;
	e_loss =0;
	is_MOME = false;
	is_XSMI = false;
	if_spec_reverse = false;
	reverse_params.A=0; reverse_params.beta=0; reverse_params.E=0;
	if_spec_order_forward = false;
	forward_order.clear();
	if_spec_order_reverse = false;
	reverse_order.clear();
	if_spec_rxn_units = false;
	units.clear();
	is_duplicate = false;
	duplicate = -1;
	//Rxn_number = get_Rxn_number(); Don't use the Arbit number, use the index number
	type = 0;
	reactant_effs.clear();
	product_effs.clear();
	mygas = NULL;
	Fstoichiometric=false;
	for_rate=0;
	rev_rate=0;
}

REKS_reaction::REKS_reaction(REKS_reaction* dup)
{
	int i;
	
	reaction_flag=dup->reaction_flag;
	reactants = dup->reactants;
	products = dup->products;
	arrhe_params.A = (dup->arrhe_params).A;
	arrhe_params.beta= (dup->arrhe_params).beta;
	arrhe_params.E= (dup->arrhe_params).E;
	is_three_body = dup->is_three_body;
	is_third_spec = dup->is_third_spec;
	third_specs = dup->third_specs;
	thirdbodies = dup->thirdbodies;
	is_fall_of = dup->is_fall_of;
	is_chem_act = dup->is_chem_act;
	fall_of.A = (dup->fall_of).A;
	fall_of.beta = (dup->fall_of).beta;
	fall_of.E = (dup->fall_of).E;
	chem_act.A = (dup->chem_act).A;
	chem_act.beta = (dup->chem_act).beta;
	chem_act.E = (dup->chem_act).E;
	is_Lindemann = dup->is_Lindemann;
	is_Troe = dup->is_Troe;
	troe_params.a=(dup->troe_params).a;
	troe_params.T3star=(dup->troe_params).T3star;
	troe_params.Tstar=(dup->troe_params).Tstar;
	troe_params.T2star=(dup->troe_params).T2star; //invalid value
	is_SRI = dup->is_SRI;
	sri_params.a =(dup->sri_params).a; sri_params.b=(dup->sri_params).b; sri_params.c=(dup->sri_params).c; sri_params.d=(dup->sri_params).d; //default value
	sri_params.e=(dup->sri_params).e; //default value
	is_LandauTeller = dup->is_LandauTeller;
	lt_params.Bi=(dup->lt_params).Bi;
	lt_params.Ci=(dup->lt_params).Ci;
	if_spec_Eq79 = dup->if_spec_Eq79;
	for (i=0; i<9; i++)
		eq79_bn[i] = dup->eq79_bn[i];
	if_spec_Eq80 = dup->if_spec_Eq80;
	for (i=0; i<4; i++)
		eq80_bn[i]=dup->eq80_bn[i];
	if_spec_HV = dup->if_spec_HV;
	wave_length = dup->wave_length;
	if_spec_temp_dep = dup->if_spec_temp_dep;
	temp_dep_specs = dup->temp_dep_specs;
	if_spec_energy_loss = dup->if_spec_energy_loss;
	e_loss = dup->e_loss;
	is_MOME = dup->is_MOME;
	is_XSMI = dup->is_XSMI;
	if_spec_reverse = dup->if_spec_reverse;
	reverse_params.A = (dup->reverse_params).A; 
	reverse_params.beta=(dup->reverse_params).beta; 
	reverse_params.E=(dup->reverse_params).E;
	if_spec_order_forward = dup->if_spec_order_forward;
	forward_order = dup->forward_order;
	if_spec_order_reverse = dup->if_spec_order_reverse;
	reverse_order = dup->reverse_order;
	if_spec_rxn_units = dup->if_spec_rxn_units;
	units = dup->units;
	is_duplicate = dup->is_duplicate;
	duplicate = dup->duplicate;
	rxn_number = dup->rxn_number; //Don't use the Arbit number, use the index number
	type = dup->type;
	reactant_effs=dup->reactant_effs;
	product_effs=dup->product_effs;
	mygas = NULL;
	Fstoichiometric=false;
	for_rate=0;
	rev_rate=0;
}

REKS_reaction::~REKS_reaction()
{

}

bool REKS_reaction::operator==(const REKS_reaction& rxn) const
{
	int i;
	for (i=0; i<this->reactants.size(); i++)
		if (this->reactants[i]!=rxn.reactants[i])
			return false;
	for (i=0; i<this->products.size(); i++)
		if (this->products[i]!=rxn.products[i])
			return false;
	return true;
}

bool REKS_reaction::is_legal_unit(string word)
{
   if (match(word, "MOLE(CULE)"))
      return true; 
    else if (match(word, "CAL"))
      return true; 
    else if (match(word, "KCAL"))
      return true; 
    else if (match(word, "JOUL"))
      return true; 
    else if (match(word, "KJOUL"))
      return true; 
	else if (match(word, "KELV(IN)"))
      return true; 
	else if (match(word, "EVOL(TS)"))
      return true; 
    else 
      return false; //not a key word
}

void REKS_reaction::dump()
{
	int i;
	cout<<"========================================"<<endl;	
	cout<<"Rxn number: "<<rxn_number<<endl;
	cout<<"========================================"<<endl;	
	cout<<"isDuplicate: "<<is_duplicate<<endl;
	cout<<"duplicate number: "<<duplicate<<endl;

	cout<<"Reactants:"<<endl;
	REKS_specie_thermo * debugt;
	for (i=0; i<reactants.size(); i++)
	{
		debugt=reactants[i];
		cout<<reactants[i]->m_spec_name<<"   ";
	}
	cout<<endl;
	cout<<"Products:"<<endl;
	for (i=0; i<products.size(); i++)
		cout<<products[i]->m_spec_name<<"   ";
	cout<<endl;
	cout<<"arrhe_params"<<endl;
	cout<<arrhe_params.A<<"   "<<arrhe_params.beta<<"   "<<arrhe_params.E<<endl;
	
	cout<<"isThreeBody: "<<is_three_body<<endl;
	cout<<"third body: "<<endl;
	for (map<string,REAL>::iterator j= thirdbodies.begin(); j != thirdbodies.end(); j++)
		cout<<(*j).first<<"   "<<(*j).second<<endl;
	
	
	cout<<"isFallof: "<<is_fall_of<<endl;
	cout<<"isChemAct: "<<is_chem_act<<endl;
	
	cout<<"Fall of :"<<endl;
	cout<<fall_of.A <<"   " <<fall_of.beta <<"   "<<fall_of.E<<endl;
	cout<<"Chem Act :"<<endl;
	cout<<chem_act.A <<"   " <<chem_act.beta <<"   "<<chem_act.E<<endl;

	cout<<"isLindemann: "<<is_Lindemann<<endl;
	
	cout<<"isTroe: "<<is_Troe<<endl;
	cout<<"Troe params: "<<endl;
	cout<<troe_params.a<<"   "<<troe_params.T3star<<"   "<<troe_params.Tstar<<"   "<<troe_params.T2star<<endl;
	
	cout<<"isSRI: "<<is_SRI<<endl;
	cout<<sri_params.a<<"   ";
	cout<<sri_params.b<<"   ";
	cout<<sri_params.c<<"   ";
	cout<<sri_params.d<<"   ";
	cout<<sri_params.e<<endl;
	cout<<"isLandauTeller: "<<is_LandauTeller<<endl;
	cout<<lt_params.Bi<<"   "<<lt_params.Ci<<"   "<<endl;
	
	cout<<"if specify EQ79 "<<if_spec_Eq79<<endl;
	for (i=0; i<9; i++)
		cout<<eq79_bn[i]<<"   ";
	cout<<endl;
	
	cout<<"if specify EQ80 "<<if_spec_Eq80<<endl;
	for (i=0; i<4; i++)
		cout<<eq80_bn[i]<<"   ";
	cout<<endl;
	
	cout<<"if_HV: "<<if_spec_HV<<endl;
	cout<<"wave_length: "<<	wave_length <<endl;

	cout<< "if_SpecTempDep: "<<if_spec_temp_dep<<endl;
	for (i=0; i<temp_dep_specs.size(); i++)
		cout<<temp_dep_specs[i]<<"   ";
	cout<<endl;
	
	cout<<"if_SpecEnergyLoss: "<<if_spec_energy_loss<<endl;
	cout<< e_loss <<endl;
	cout<<"MOME: "<<is_MOME<<endl;
	cout<<"XSMI: "<<is_XSMI<<endl;
	cout<<"ifSepcReverse: "<<if_spec_reverse<<endl;
	cout<<reverse_params.A<<"   "<<reverse_params.beta<<"   "<<reverse_params.E<<endl;
	
	cout<<"ifSpecOrderForward: "<<if_spec_order_forward<<endl;
	for (i=0; i<forward_order.size(); i++)
		cout<<(forward_order[i].spec)->m_spec_name<<"   "<<forward_order[i].vki1<<"   "<<forward_order[i].vki2<<endl;
	
	cout<<"ifSpecOrderReverse: "<<if_spec_order_reverse<<endl;
	for (i=0; i<reverse_order.size(); i++)
		cout<<(reverse_order[i].spec)->m_spec_name<<"   "<<reverse_order[i].vki1<<"   "<<reverse_order[i].vki2<<endl;

	cout<<"ifSpecRxnUnits: "<<if_spec_rxn_units<<endl;
	for (i=0; i<units.size(); i++)
		cout<<units[i]<<"   ";
	cout<<"========================================"<<endl;	
	cout<<"========================================"<<endl;	
	cout<<endl;

}

bool REKS_reaction::balance_check()
{
	int i,j;
	map<string,REAL>left;
	map<string,REAL>right;
	map<string,REAL>::iterator the_iter;
	map<string,REAL>::iterator tempi;

	char elem[3]={'\0', '\0', '\0'};
	REAL anum;
	string aelem;
	left.clear();
	right.clear();
	for (i=0; i< reactants.size(); i++)
	{
		for (j=0;  j<(reactants[i]->m_atom_form).size(); j++)
		{
			elem[0]=(reactants[i]->m_atom_form)[j].symbol[0];
			elem[1]=(reactants[i]->m_atom_form)[j].symbol[1];
			aelem=strtok(elem," ");
			anum=(reactants[i]->m_atom_form[j]).num * reactant_effs[i];
			the_iter=left.find(aelem);
			if (the_iter==left.end())
				left.insert(pair<string,REAL>(aelem,anum));
			else
				the_iter->second +=anum;
		}
	}
	
	for (i=0; i< products.size(); i++)
	{
		for (j=0;  j<(products[i]->m_atom_form).size(); j++)
		{
			elem[0]=(products[i]->m_atom_form)[j].symbol[0];
			elem[1]=(products[i]->m_atom_form)[j].symbol[1];
			aelem=strtok(elem," ");
			anum=(products[i]->m_atom_form[j]).num*product_effs[i];
			the_iter=right.find(aelem);
			if (the_iter==right.end())
				right.insert(pair<string,REAL>(aelem,anum));
			else
				the_iter->second +=anum;
		}
	}
	
	int k = left.size();
	for (tempi=left.begin(); tempi!=left.end(); tempi++)
	{
		aelem = tempi->first;
		the_iter=right.find(aelem);
		if (the_iter==right.end())
		{
			cerr<<"Error! Reaction not balance at element: "<<aelem<<endl;
			cerr<<"No such element at the right side of the reaction."<<endl;
			return false;
		}
		if (fabs(tempi->second-the_iter->second)>RXNTOL)
		{
			cerr<<"Error! Reaction not balance at element: "<<aelem<<endl;
			cerr<<"Left  side: "<<aelem<<"   "<<tempi->second<<endl;
			cerr<<"Right side: "<<aelem<<"   "<<the_iter->second<<endl;
			return false;
		}
	}

	for (tempi=right.begin(); tempi!=right.end(); tempi++)
	{
		aelem = tempi->first;
		the_iter=left.find(aelem);
		if (the_iter==left.end())
		{
			cerr<<"Error! Reaction not balance at element: "<<aelem<<endl;
			cerr<<"No such element at the left side of the reaction."<<endl;
			return false;
		}
		if (fabs(tempi->second-the_iter->second)>RXNTOL)
		{
			cerr<<"Error! Reaction not balance at element: "<<aelem<<endl;
			cerr<<"Left  side: "<<aelem<<"   "<<the_iter->second<<endl;
			cerr<<"Right side: "<<aelem<<"   "<<tempi->second<<endl;
			return false;
		}
	}

	return true;
}

void REKS_reaction::stoichiometric()
{
  int i;
  map<REKS_specie_thermo *,REAL>::iterator tempi;

  if (Fstoichiometric)
	  return;

  REKS_specs.clear();
  REKS_specs_v1.clear();
  REKS_specs_v2.clear();
  for (i=0; i<reactants.size(); i++)
    {
      REKS_specs.insert(pair<REKS_specie_thermo *,REAL>(reactants[i],-reactant_effs[i]));
      REKS_specs_v1.insert(pair<REKS_specie_thermo *,REAL>(reactants[i],reactant_effs[i]));  
      REKS_specs_v2.insert(pair<REKS_specie_thermo *,REAL>(reactants[i],0));
    }

  for (i=0; i<products.size(); i++)
    {
      tempi = REKS_specs.end();
      tempi=REKS_specs.find(products[i]);
      if (tempi!=REKS_specs.end())
	  tempi->second+=product_effs[i];
      else
		REKS_specs.insert(pair<REKS_specie_thermo *,REAL>(products[i], product_effs[i]));
      tempi = REKS_specs_v1.end();
      tempi=REKS_specs_v1.find(products[i]);
      if (tempi==REKS_specs_v1.end())
		REKS_specs_v1.insert(pair<REKS_specie_thermo *,REAL>(products[i],0));
      tempi = REKS_specs_v2.end();
      tempi =REKS_specs_v2.find(products[i]);
      if (tempi!=REKS_specs_v2.end())
		tempi->second+=product_effs[i];
	  else
		REKS_specs_v2.insert(pair<REKS_specie_thermo *,REAL>(products[i],product_effs[i]));
    }

  order.clear();

  for (i=0; i<forward_order.size(); i++)
	order.push_back(forward_order[i]);
  
  bool spec_found;
  TSpec_order_param cur_spec_order;
  
  for (i=0; i<reverse_order.size(); i++)
  {
	spec_found = false;
	for (int j=0; j<order.size(); j++)
	{
		if (order[j].spec==reverse_order[i].spec)
		{
			order[j].vki2=reverse_order[i].vki2;
			spec_found = true;
		}
	}
	if (!spec_found)
	{
		cur_spec_order.spec=reverse_order[i].spec;
		cur_spec_order.vki1=0;
		cur_spec_order.vki2=reverse_order[i].vki2;
		order.push_back(cur_spec_order);
	}

  }

  Fstoichiometric=true;
}

REAL REKS_reaction::equilibrium_const()
{
  //First, get the Hk
  REAL changeS=0;
  REAL changeH=0;
  REAL tt;
  REAL t1; 
  REAL t2;

  REKS_usr_specie* temp;
  
  for (map<REKS_specie_thermo *,REAL>::iterator tempi=REKS_specs.begin(); tempi!=REKS_specs.end(); tempi++)
    {
	 
	  //temp = mygas->get_specie_thermo(tempi->first);
	  temp=&(mygas->specs[tempi->first->index]);
	  //tt = tempi->second;
	  t1 = (temp->enthalpy_molar_CKHML());
	  t2 = (temp->entropy_molar_CKSML());
	  changeH=changeH+t1*tempi->second;
	  changeS=changeS+t2*tempi->second;
#ifdef DEBUG
	  
	  printf("%-16s  %.20e \n%.20e \n%.20e\n", tempi->first->m_spec_name.c_str(),
		 tempi->second, t1, t2);
	  printf("changeH  %.20e\n", changeH);
	  printf("changeS  %.20e\n", changeS);
	    
#endif
    }
  tt = changeS/R - changeH/(R*mygas->temperature);
#ifdef DEBUG
  printf("changeS %.20e\n", changeS);
  printf("changeH %.20e\n", changeH);
  printf("R       %.20e\n", R);
  printf("TEMP    %.20e\n", mygas->temperature);
  printf("tt %.15e\n", tt);
#endif
  Kpi=exp(tt);
  return Kpi;
	
}

REAL REKS_reaction::reverse_rate()
{
  REAL topow=0;
  REAL Kci;
  REAL t2;
  int ii;

  if (type==2) //irreversible reaction
  {
	  rev_rate = 0;
	  return rev_rate;
  }

  if (if_spec_reverse)
    {
      rev_rate = reverse_params.CKABE(mygas->temperature);

      return rev_rate;
    }
  
  for (map<REKS_specie_thermo *,REAL>::iterator tempi=REKS_specs.begin(); 
       tempi!=REKS_specs.end(); tempi++)
    {
      topow+=tempi->second;

#ifdef DEBUG
      printf("%-20s  %.15e\n", tempi->first->m_spec_name.c_str(), tempi->second);
#endif

    }
  
  REAL t1=patom/(R*mygas->temperature);

  /*  t2=1.0;
      if ((topow-int(topow))==0)
      {
      t2=1.0;
      if(topow>0)
      for(ii=0; ii<topow; ii++)
      t2*=t1;
      else if(topow<0)
      for(ii=0; ii<topow; ii++)
      t2=t2/t1;
      else;
      }
      else
  */
  t2=pow(t1,topow);

#ifdef DEBUG
  printf("topow  %.15e\n", topow);
  printf("t1 %.15e\n", t1);
  printf("t2 %.15e\n", t2);
  printf("Kpi %.15e\n", Kpi);
#endif

  Kci=Kpi*t2; //Pressure here Martin think it's the gas pressure
  
  rev_rate= for_rate/Kci;

#ifdef DEBUG
  printf("rev_rate %.15e\n", rev_rate);
#endif

  return rev_rate;
  
}

REAL REKS_reaction::forward_rate()
{
	REAL k_high;
	REAL k_low;
	REAL reduced_P;
	int i;
	REAL M=0;
	REAL k;
	REAL F;
	map<string, REAL>::iterator found;
	REAL molar_c;
	REAL a;
	REKS_specie_thermo * spec;
	
	if (reaction_flag&PRES_DEP)
	{
		if (is_fall_of) //the arrhe_params is specify the high temp
		{
			k_high = arrhe_params.CKABE(mygas->temperature);
			k_low = fall_of.CKABE(mygas->temperature);
		}
		else if(is_chem_act)
		{
			k_low = arrhe_params.CKABE(mygas->temperature);
			k_high = chem_act.CKABE(mygas->temperature);
		}
	
		if (is_third_spec)
		{
			for (i=0; i<third_specs.size(); i++)
			{
				molar_c = (mygas->get_specie_thermo(third_specs[i]))->molar_concentration;
				found = thirdbodies.find(third_specs[i]->m_spec_name);
				if (found!=thirdbodies.end())
					a=found->second;
				else
					a=1;
				M+=a*molar_c;
			}

			
		}
		else
		{
			for (i=0; i<(mygas->specs).size(); i++)
			{
				molar_c=(mygas->specs)[i].molar_concentration;
				spec = (mygas->specs)[i].spec;
			
				found = thirdbodies.find(spec->m_spec_name);
			
				if (found!=thirdbodies.end())
					a=found->second;
					
				else
					a=1;
		
				M+=a*molar_c;

			}
		}

		reduced_P = k_low*M/k_high;
	
		k=k_high*reduced_P/(1.0+reduced_P);

		if (is_Lindemann)
		{
			for_rate = k;
			return for_rate;
		}
		else if (is_Troe)
		{
			REAL Fcent;
			REAL c;
			REAL n;
			REAL d;
			REAL logF;
			REAL logFcent;
			
			Fcent = (1.0-troe_params.a)*exp(-mygas->temperature/troe_params.T3star)
					+ troe_params.a*exp(-mygas->temperature/troe_params.Tstar);
			if (troe_params.T2star!=-10E+10)
				Fcent += exp((-troe_params.T2star) / (mygas->temperature));
			logFcent=log10(Fcent);
			c = -0.4-0.67*logFcent;
			n = 0.75 - 1.27*logFcent;
			d = 0.14;
			
			logF =(1/(1+pow((log10(reduced_P)+c)/(n-d*(log10(reduced_P)+c)), 2)))*logFcent;

			F=pow(10,logF);
			for_rate = k*F;
			return for_rate;
		}
		else if (is_SRI)
		{
			REAL X;
			
			X = 1/(1+pow(log10(reduced_P),2));
			F = sri_params.d*pow((sri_params.a*exp(-sri_params.b/mygas->temperature)
								+exp(-mygas->temperature/sri_params.c)), X)
							*pow(mygas->temperature, sri_params.e);
			for_rate = k*F;
			return for_rate;
		}
	}
	else if (is_LandauTeller)
	{
		for_rate=arrhe_params.A*exp(lt_params.Bi/pow(mygas->temperature, 1/3)
									+lt_params.Ci/pow(mygas->temperature, 2/3));
		return for_rate;
								
	}
	else if (if_spec_Eq79)
	{
		REAL jan=0;
		
		for (i=0; i<9; i++)
			jan+=eq79_bn[i]*pow(log(mygas->temperature),i);

		for_rate = arrhe_params.A*pow(mygas->temperature,arrhe_params.beta)
					*exp(arrhe_params.E/mygas->temperature+jan);
		return for_rate;
	}
	else if (if_spec_Eq80)
	{
		REAL fit1;

		for (i=0; i<4; i++)
			fit1 = eq80_bn[i]/pow(mygas->temperature, i);
		for_rate = arrhe_params.A * pow(mygas->temperature, arrhe_params.beta)
					*exp(fit1);

		return for_rate;
	}

	for_rate = arrhe_params.CKABE(mygas->temperature);
	return for_rate;
}

void REKS_reaction::update()
{
	equilibrium_const();
	forward_rate();
	reverse_rate();
}

void REKS_reaction::set_gas(REKS_gas* the_gas)
{
	mygas=the_gas;
	stoichiometric();
	equilibrium_const();
	forward_rate();
	reverse_rate();
//	cout<<"for_rate:     "<<for_rate<<"    rev_rate:    "<<rev_rate<<endl;
}


int REKS_reaction::already_in_rct(REKS_specie_thermo * rct)
{
	int i;
	for (i=0; i<reactants.size(); i++)
		if (reactants[i]==rct)
			return i;
	return -1;
}

int REKS_reaction::already_in_pdt(REKS_specie_thermo * pdt)
{
	int i;
	for (i=0; i<products.size(); i++)
		if (products[i]==pdt)
			return i;
	return -1;
}
