//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////

#include "reks.h"
#include "REKS_Thrm_Info.h"
#include "ReactionReader.h"
void REKS_sys::dump(FILE *output)
{
	int i;
	//	map<REKS_specie_thermo *, REAL>::iterator iter;

	fprintf(output, "PRESSURE:      %.15g Pa \n", gas_sys->pressure/10.0);
	fprintf(output, "TEMPERATURE:   %.15g K\n", gas_sys->temperature);      
	fprintf(output, "MEAN_MW:       %.15g \n", gas_sys->mean_mole_wgt_by_molef() );
	fprintf(output, "\n");
	fprintf(output, "Mole fractions of species: \n");

	for (i = 0; i<gas_sys->specs.size(); i++)
	  fprintf(output, "REAC  %-18s    %.6e\n",
		  (gas_sys->specs[i].spec->m_spec_name).c_str(), gas_sys->specs[i].mole_fraction);
}

#ifdef DEBUG
void REKS_sys::dump2(FILE *output)
{
  int i;
  //  map<REKS_specie_thermo *, REAL>::iterator iter;
  
  fprintf(output, "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  fprintf(output, "THE OUTPUT OF THE prod rate   +++++++++++++++++++++++++++++++++++++++\n");
  for (i=0; i<gas_sys->specs.size(); i++)
    {
      //iter = masf_prodrate.find((gas_sys->specs[i]).spec);
      //fprintf(output, "REAC  %-18s    %.6e\n", (gas_sys->specs[i].spec->m_spec_name).c_str(), iter->second);
      fprintf(output, "REAC  %-20s    %.15e\n", (gas_sys->specs[i].spec->m_spec_name).c_str(), masf_prodrate[i]);
    }
  fprintf(output, "+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n");
  for (i=0; i<this->m_p_reactions.size(); i++)
    fprintf(output, "RXN  %.4d    %.15e\n", i, rate_prog_v[i] );
  
}
#endif

vector<REAL>* REKS_sys::rate_of_progress()
{
  int i, ii;
  map<REKS_specie_thermo *, REAL>::iterator iter;
  map<REKS_specie_thermo *, REAL>::iterator iter2;
  REAL tmp1, tmp2, molar_c, q, tt;
  REKS_reaction* cur_rxn;
  REAL vk1;
  REAL vk2;

  int xx;

#ifdef DEBUG
  FILE *qrate;
  //  qrate=fopen("reaction.rat", "r");
  qrate=NULL;
  if (qrate==NULL)
    qrate = fopen("reaction.rat", "w");
  else
    {
      fclose(qrate);
      qrate = NULL;
    }
#endif

  // rate_prog_v.clear();
  //rate_prog_v.resize(m_p_reactions.size());
  if (rate_prog_v.size()!=m_p_reactions.size())
    rate_prog_v.resize(m_p_reactions.size());
  for (i=0; i<this->m_p_reactions.size(); i++)
    {
      tmp1=1;
      tmp2=1;
      molar_c=0;
      cur_rxn = (m_p_reactions)[i];

	  if (cur_rxn->if_spec_order_forward)
	  {
		for (xx=0; xx<cur_rxn->order.size(); xx++)
		{
		  // molar_c=(gas_sys->get_specie_thermo((cur_rxn->order)[xx].spec))->molar_concentration;
		  molar_c=gas_sys->specs[(cur_rxn->order)[xx].spec->index].molar_concentration;

		  vk1 = (cur_rxn->order)[xx].vki1;
		  vk2 = (cur_rxn->order)[xx].vki2;
		  if ((vk1-int(vk1))==0)
		    {
		      tt=1.0;
		      if(vk1>0)
			for(ii=0; ii<vk1; ii++)
			  tt*=molar_c;
		      else if(vk1<0)
			{
			  for(ii=0; ii<vk1; ii++)
			    tt=tt*molar_c;
			  tt=1.0/tt;
			}
		      else;
		      tmp1=tmp1*tt;
		    }
		  else
		    tmp1 = tmp1*pow(molar_c, vk1);
		  if ((vk2-int(vk2))==0)
		    {
		      tt=1.0;
		      if(vk2>0)
			for(ii=0; ii<vk2; ii++)
			  tt*=molar_c;
		      else if(vk1<0)
			{
			  for(ii=0; ii<vk2; ii++)
			    tt=tt*molar_c;
			  tt=1.0/tt;
			}
		      else;
		      tmp2=tmp2*tt;
		    }
		  else
		    tmp2 = tmp2*pow(molar_c, vk2);
#ifdef DEBUG
		  {
		    
		    cout<<molar_c<<endl;
		    cout<<vk1<<endl;
		    cout<<vk2<<endl;
		    cout<<tmp1<<endl;
		    cout<<tmp2<<endl;
		    cout.flush();
		  }
#endif
		}
	  }

      for(iter=cur_rxn->REKS_specs_v1.begin(),iter2=cur_rxn->REKS_specs_v2.begin(); 
	  iter != cur_rxn->REKS_specs_v1.end(); 
	  iter++, iter2++)
		{
		  //molar_c=(gas_sys->get_specie_thermo(iter->first))->molar_concentration;
		  molar_c=gas_sys->specs[iter->first->index].molar_concentration;
		  if (cur_rxn->if_spec_order_forward)
		    {
		      vk1=cur_rxn->getVk1(iter->first);
		      if (vk1==0)
			vk1 = iter->second;
		      else
			continue; //already computed as for specie order, skip this 
		    }
		  else
		    vk1 = iter->second;
		  
		  if (cur_rxn->if_spec_order_reverse)
		    {
		      vk2=cur_rxn->getVk2(iter->first);
				if (vk2==0)
				  vk2 = iter->second;
				else
				  continue; //already computed as for specie order, skip now
		    }
		  else
			vk2 = iter2->second;
		  
		  if ((vk1-int(vk1))==0)
		    {
		      tt=1.0;
		      if(vk1>0)
			for(ii=0; ii<vk1; ii++)
			  tt*=molar_c;
		      else if(vk1<0)
			{
			  for(ii=0; ii<vk1; ii++)
			    tt=tt*molar_c;
			  tt=1.0/tt;
			}
		      else;
		      tmp1=tmp1*tt;
		    }
		  else
		    tmp1 = tmp1*pow(molar_c, vk1);
		  
		  if ((vk2-int(vk2))==0)
		    {
		      tt=1.0;
		      if(vk2>0)
			for(ii=0; ii<vk2; ii++)
			  tt*=molar_c;
		      else if(vk1<0)
			{
			  for(ii=0; ii<vk2; ii++)
			    tt=tt*molar_c;
			  tt=1.0/tt;
			}
		      else;
		      tmp2=tmp2*tt;
		    }
		  else
		    tmp2 = tmp2*pow(molar_c, vk2);
		  
#ifdef DEBUG		  
		  printf("%-20s   %.15e\n", iter->first->m_spec_name.c_str(), molar_c);
		  printf("%.15e\n",vk1);
		  printf("%.15e\n",vk2);
		  printf("%.15e\n",tmp1);
		  printf("%.15e\n",tmp2);
		  fflush(NULL);
#endif
		  
		}
      
      REAL work1, work2;
      work1 = cur_rxn->for_rate*tmp1;
      work2 = cur_rxn->rev_rate*tmp2;
      
      
      
      q = cur_rxn->for_rate*tmp1 - cur_rxn->rev_rate*tmp2;
#ifdef DEBUG      
      {
	printf("REV_RATE   %.15e\n", cur_rxn->rev_rate);
	printf("%.15e\n",work1);
	printf("%.15e\n",work2);
	printf("q %.15e\n", q);
	cout<<"==============================="<<endl;
      }
#endif
      if ((cur_rxn->is_three_body||cur_rxn->is_third_spec)&&!(cur_rxn->reaction_flag&PRES_DEP))
	{
	  q=q*third_body_eff(cur_rxn);
	  work1 *= third_body_eff(cur_rxn);
	  work2 *= third_body_eff(cur_rxn);
	}
      
// Q can be outputed here

#ifdef DEBUG	 
	  if (qrate!=NULL)
	    fprintf(qrate, "%d:        %.15g \n", cur_rxn->rxn_number, q);
#endif	  
	  //fprintf("%d:   %.20g       %.20g \n",  cur_rxn->rxn_number, work1, work2);     
	  rate_prog_v[i]=q;
	
    }
#ifdef DEBUG
  if (qrate != NULL)
    fclose(qrate);
#endif

  return &rate_prog_v;
}

REAL REKS_sys::third_body_eff(REKS_reaction* cur_rxn)
{
	map<REKS_specie_thermo *, REAL>::iterator iter;
	map<string, REAL>::iterator found;
	REAL molar_c;
	REAL a;
	REAL result = 0;
	int i;
	REKS_specie_thermo * spec;

	if (cur_rxn->is_three_body) //general three body
	{ //this is going to loop through all the species in the gas system
		for (i=0; i<(gas_sys->specs).size(); i++)
		{
			molar_c=(gas_sys->specs)[i].molar_concentration;
			spec = (gas_sys->specs)[i].spec;
			
			found = (cur_rxn->thirdbodies).find(spec->m_spec_name);
			
			if (found!=cur_rxn->thirdbodies.end())
				a=found->second;
			else
				a=1;
		
			result+=a*molar_c;

		}

		return result;
	}

	//three body only for a certain specs
	for (i=0; i<cur_rxn->third_specs.size(); i++)
	{
	  //molar_c = (gas_sys->get_specie_thermo(cur_rxn->third_specs[i]))->molar_concentration;
	  molar_c=gas_sys->specs[cur_rxn->third_specs[i]->index].molar_concentration;
	  found = (cur_rxn->thirdbodies).find(cur_rxn->third_specs[i]->m_spec_name);
	  if (found!=cur_rxn->thirdbodies.end())
	    a=found->second;
	  else
	    a=1;
	  result+=a*molar_c;
	}

	return result;		
}

//map<REKS_specie_thermo *,REAL>* REKS_sys::comp_rate_all()
vector<REAL>* REKS_sys::comp_rate_all()
{
  int i, j, rxn;
  REKS_specie_thermo * p_spec;
  REKS_reaction * p_rxn;
  map<REKS_specie_thermo *,REAL>::iterator iter_vk;
  REAL omega;
  REAL v;
  REAL dy;

  //  prod_rate.clear();
  if (prod_rate.size()!=gas_sys->specs.size())
    {
      prod_rate.resize(gas_sys->specs.size());
      masf_prodrate.resize(gas_sys->specs.size());
    }
  //masf_prodrate.clear();
  
  v = 1.0/gas_sys->density;

  for (i=0; i<gas_sys->specs.size(); i++)
    {
      p_spec = (gas_sys->specs[i]).spec; //get the specie's REKS_specie_thermo pointer
      omega = 0.0;
	
      for (j=0; j<p_spec->related_reactions.size(); j++)
	{
	  rxn = p_spec->related_reactions[j]; //get the reaction index number
	  p_rxn = m_p_reactions[rxn]; //get the reaction pointer
	  iter_vk=(p_rxn->REKS_specs).find(p_spec); //get the iter of the specie in the vk list
	  if(iter_vk==(p_rxn->REKS_specs).end()) //error
	    {
	      cerr<<"Can't find the spec in the reaction"<<endl;
	      return NULL;
	    }
	  omega+=iter_vk->second*rate_prog_v[rxn];
	}
	
    dy = v*omega*p_spec->spec_wgt;
    // prod_rate.insert(pair<REKS_specie_thermo *,REAL>(p_spec, omega));
    prod_rate[i]=omega;
    // masf_prodrate.insert(pair<REKS_specie_thermo *, REAL>(p_spec, dy));  
    masf_prodrate[i]=dy;
  }

  return &prod_rate;
}

//map<REKS_specie_thermo *,REAL>* REKS_sys::comp_rate_allRED()
vector<REAL> * REKS_sys::comp_rate_allRED()
{
  int i, NKK;
  REKS_specie_thermo * cur_spec;
  REKS_usr_specie * cur_usr_sp;
  
  REAL v;
  REAL dy;
  REAL P;
  REAL T;

  prod_rate.clear();
 
  masf_prodrate.clear();
  v = 1.0/gas_sys->density;
  NKK = reks_thrm_info->m_specs.size();

  prod_rate.resize(NKK);
  masf_prodrate.resize(NKK);

  if (wdot==NULL)
	  wdot = new REAL[NKK];
  if (Y==NULL)
	  Y = new REAL[NKK];
  
  P = gas_sys->pressure;
  T = gas_sys->temperature;

  for (i=0; i<NKK; i++)
  {
    
    //Yang's change, I think now the index should be direct match
    //cur_spec =  get_specp(reks_thrm_info->m_specs[i], reks_thrm_info->m_thrm_specs);
    //cur_usr_sp = gas_sys->get_specie_thermo(cur_spec);
    //Y[i] = cur_usr_sp->mass_fraction;

    //index = get_spec_index(reks_thrm_info->m_specs[i], reks_thrm_info->m_thrm_specs);

    Y[i] = gas_sys->specs[i].mass_fraction;

  }

  (edll->get_fptr())(&P, &T, Y, NULL, NULL, wdot);

  for (i=0; i<NKK; i++)
  {
    //Yang's change, I think now the index should be direct match
    //cur_spec =  get_specp(reks_thrm_info->m_specs[i], reks_thrm_info->m_thrm_specs);
    cur_spec = reks_thrm_info->m_thrm_specs[i];
    dy = v*wdot[i]*cur_spec->spec_wgt;

    //prod_rate.insert(pair<REKS_specie_thermo *,REAL>(cur_spec, wdot[i]));
    //masf_prodrate.insert(pair<REKS_specie_thermo *, REAL>(cur_spec, dy));  
    prod_rate[i]=wdot[i];
    masf_prodrate[i]=dy;  
  }

  return &prod_rate;
}

REAL REKS_sys::comp_rate(string spec)
{
  int i, j, rxn, index;
  REKS_specie_thermo * p_spec;
  REKS_reaction * p_rxn;
  map<REKS_specie_thermo *,REAL>::iterator iter_vk;
  REAL omega;

  for (i=0; i<gas_sys->specs.size(); i++)
    {
      p_spec = (gas_sys->specs[i]).spec; //get the specie's REKS_specie_thermo pointer
      if (p_spec->m_spec_name==spec)
	break;
    }
  if (i==gas_sys->specs.size()) //didn't find the spec
    {
      cerr<<spec<<"  : No such thing in the input file"<<endl;
      return 0;
    }
  //i keeps the index

  //if ((iter_vk=prod_rate.find(p_spec))!=prod_rate.end()) //already computed, return directly
  //  if ((iter_vk=prod_rate))!=prod_rate.end())
  //  return iter_vk->second;
  
  //if (prod_rate.size()==gas_sys->specs.size())
  //  return prod_rate[i];

  //begin computation

  if (prod_rate.size()!=gas_sys->specs.size())
    prod_rate.resize(gas_sys->specs.size());

  omega = 0;

  for (j=0; j<p_spec->related_reactions.size(); j++)
    {
      rxn = p_spec->related_reactions[j]; //get the reaction index number
      p_rxn = m_p_reactions[rxn]; //get the reaction pointer
      iter_vk=(p_rxn->REKS_specs).find(p_spec); //get the iter of the specie in the vk list
      if (iter_vk!=(p_rxn->REKS_specs).end()) //error
	{
	  cerr<<"Can't find the spec in the reaction"<<endl;
	  return 0;
	}
      omega+=iter_vk->second*rate_prog_v[rxn];
    }
  
  //prod_rate.insert(pair<REKS_specie_thermo *,REAL>(p_spec, omega));
  prod_rate[i]=omega;

  return omega;
}


void REKS_sys::Initialize()
{
	
	wdot=NULL;
	Y=NULL;
}

void REKS_sys::ReadKinetics(vector<string> files)
{
	REKS_reaction_reader rxn_reader;
	rxn_reader.kinetics=this;
	rxn_reader.parse(files);
}

void REKS_sys::update_sys()
{
  int i;
    
  //for (i=0; i<gas_sys->specs.size(); i++)
  //{
  //gas_sys->specs[i].massf2molarc_by_pres();
  //gas_sys->specs[i].massf2molef_by_MMW();
  //}
  gas_sys->massf2molarc_by_pres();
  gas_sys->massf2molef();

  for (i=0; i<m_p_reactions.size(); i++)
    (m_p_reactions[i])->update();
}

void REKS_sys::set_gas_rxn(REKS_gas * the_gas)
{
	int i;

	gas_sys = the_gas;
	
	for (i=0; i<the_gas->specs.size(); i++)
	{
		the_gas->specs[i].molef2molarc_by_pres();
		the_gas->specs[i].molef2massf_by_MMW();
	}

	for (i=0; i<m_p_reactions.size(); i++)
		(m_p_reactions[i])->set_gas(gas_sys);
	
}

REAL REKS_sys::temp_rate()
{
	REAL v;
	REAL cp;
	int i;
	int K;
	REAL total = 0;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	REKS_usr_specie* p_spec;
	cp = gas_sys->mix_cp_mass_CKCPBS();
	v = 1.0/gas_sys->density;
	K = gas_sys->specs.size();

	for (i=0; i < K; i++)
	{
	  p_spec=&(gas_sys->specs[i]);
	  //iter = prod_rate.find(p_spec->spec);
	  
	  //total+=p_spec->enthalpy_mass_CKHMS()
	  //			* (iter->second) * p_spec->spec->spec_wgt;
	  total+=p_spec->enthalpy_mass_CKHMS()
	    * prod_rate[i] * p_spec->spec->spec_wgt;
	}
	return -total*v/cp;
}

REAL REKS_sys::temp_rate_v()
{
	REAL v;
	REAL cv;
	int i;
	int K;
	REAL total = 0;
	//map<REKS_specie_thermo *, REAL>::iterator iter;
	REKS_usr_specie* p_spec;
	cv = gas_sys->mix_cv_mass_CKCVBS();

	v = 1.0/gas_sys->density;
	K = gas_sys->specs.size();

	for (i=0; i < K; i++)
	  {
		p_spec=&(gas_sys->specs[i]);
		//iter = prod_rate.find(p_spec->spec);
		//total+=p_spec->internal_energy_mass_CKUMS()
		//		* (iter->second) * p_spec->spec->spec_wgt;
		total+=p_spec->internal_energy_mass_CKUMS()
		  * prod_rate[i] * p_spec->spec->spec_wgt;
	}

	return -total*v/cv;
}
