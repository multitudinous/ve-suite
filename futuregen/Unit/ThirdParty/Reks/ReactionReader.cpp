// ReactionReader.cpp: implementation of the REKS_reaction_reader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#include "ReactionReader.h"

//define the longest line length
REAL Rc;

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

REKS_reaction_reader::REKS_reaction_reader()
{

}

REKS_reaction_reader::~REKS_reaction_reader()
{
 
}
//This function tell if the line is a reation line, what type of reaction it is
//And put a space before and after the equation sign to make it easy for grab tokens
//It also change (+M) to be +(M), make the grab token an easy life

int REKS_reaction_reader::reaction_line(char* buffer)
{
	int i, j, k;
	char result[BUFFER_MAX+2];
	char temp[BUFFER_MAX+2];
	char sign[4]={'\0', '\0', '\0', '\0'};
	bool in_sign = false;
	int result_flag;

	j=0; k=0;
	
	result_flag=0;
	for (i=0; i<strlen(buffer); i++)
	{
		if (!in_sign)
			if ((buffer[i]=='<')||(buffer[i]=='='))
			{
				result[j++]=' '; //add an extra space
				result[j++]=buffer[i];
				sign[k++]=buffer[i];
				in_sign=true;
				continue;
			}
			else
				if (buffer[i]=='(')
				{
					i=next_nonspace(buffer,++i);
					if (buffer[i]=='+')
					{
						i=next_nonspace(buffer,++i);
						if (buffer[i]=='M'||buffer[i]=='m')
						{
							i=next_nonspace(buffer,++i);
							result[j++]='+';
							result[j++]='(';
							result[j++]='M';
							result[j++]=')';
							continue;
						}
						else
						{
							result[j++]='+';
							result[j++]=buffer[i];
							//A hack to fix the third-body problem

							
							//end of hack
							continue;
						}

					}
					else
					{
						result[j++]='(';
						result[j++]=buffer[i];
						continue;
					}
				}
				else if (buffer[i]=='+'&&buffer[i-1]!='E'&&buffer[i-1]!='e')
				{
					result[j++]=buffer[i];
					result[j++]=' ';
				}
				else
					result[j++]=buffer[i];
		else
			if ((buffer[i]==' ')||(buffer[i]=='\t'))
				continue;
			else
				if ((buffer[i]=='=')||(buffer[i]=='>'))
				{
					result[j++]=buffer[i];
					sign[k++]=buffer[i];
					continue;
				}
				else //advanced to the product part
				{
					result[j++]=' '; //add an space as delimiter
					result[j++]=buffer[i];
					in_sign = false;
					sign[k]='\0';
				}
	}

	result[j]='\0';
	strcpy(buffer, result);
	if ((string(sign)==string("<=>"))||(string(sign)==string("=")))
		result_flag=1; //revesible reaction
	if (string(sign)==string("=>")) //irreversible reaction
			result_flag=2;
	
//Here is the code trying to delete the space between specie name and specie coefficients
	if (result_flag) //it is a reaction line
	{
		vector<string> temp2;
		string temp3;
		strcpy(temp,buffer);
		char* token;
		i=0;
		token = strtok(temp, " \t");

		temp2.clear();
		while( token )
		{
			i++;
			temp2.push_back(token);
			token = strtok(NULL, " \t");
		}

		temp3="";
		for (i=0; i<temp2.size()-1; i++)
			if (is_number(temp2[i])&&!is_number(temp2[i+1]))
				if (temp2[i+1]=="<=>"||temp2[i+1]=="="||temp2[i+1]=="=>")
					temp3=temp3+temp2[i]+" ";
				else
					temp3+=temp2[i];
			else
				temp3=temp3+temp2[i]+" ";

		temp3=temp3+" "+temp2[i];
		strcpy(buffer, temp3.c_str());
	}

////these code try to get rid of + after E in a REAL
	bool end_of_reaction=false;
	bool expect_product=true;
	bool in_word=false;

	if (result_flag) //these code try to get rid of + after E in a REAL
	{
		i =0; j=0;
		do {
			result[j++]=buffer[i++];
		} while (buffer[i-1]!='=');

		do {
			result[j++]=buffer[i++];
		} while (buffer[i-1]!=' ');

		for (;i<strlen(buffer); i++)
		{
			if (buffer[i]!='+')
			{
				if (buffer[i]!=' '&&buffer[i]!='\n')
				{
					if (expect_product)
					{	expect_product=false;
						in_word=true;
					}
					else if (!in_word)
						end_of_reaction=true;					
				}
				else
					in_word=false;

				result[j++]=buffer[i];
			}
			else
			{
				if (!end_of_reaction)
				{
					result[j++]=buffer[i];
					expect_product=true;
				}
				else
					continue; //skip the '+' after E
			}
		}
		result[j]='\0';
		strcpy(buffer,result);
	}

	return result_flag; //not a reaction line

}

int REKS_reaction_reader::get_reaction_token(char* current_line, vector<string>& toks)
{
	char* token;
	int i=0;
	token = strtok(current_line, "+ ,\t");

	toks.clear();
	while( token )
	{
		i++;
		toks.push_back(token);
		token = strtok(NULL, "+ ,\t");
	}

  return i;
}

// this function fill up the reactantants and products with specie names
// and it returns the special type of the reaction


unsigned int REKS_reaction_reader::fill_reactants_products(vector<string> toks, vector<string> &reactants, vector<string> &products, vector<string> &third_specs)
{
	bool product_now = false;
	unsigned int flag = 0x00000000;
	char spec[17];
	string cur_spec;

	reactants.clear();
	products.clear();
	third_specs.clear();

	for (int i=0; i<toks.size(); i++)
	{
		strcpy(spec,toks[i].c_str());
		cur_spec = spec;
		if (check_parenthis(spec))
		{
			bool found_third_specs=false;
			flag|=PRES_DEP;
			flag|=THIRD_BODY_SPEC;
			
			for (int xx=0; xx < third_specs.size(); xx++)
				if (third_specs[xx]==cur_spec)
				{
					found_third_specs=true;
					break;
				}
			if (!found_third_specs)
				third_specs.push_back(spec);
			continue;
		}
		if (toks[i] == "M"||toks[i] == "m")
		{	flag|=THIRD_BODY; continue;	}
		if (toks[i] == "(M)"||toks[i] == "(m)")
		{	flag|=PRES_DEP; flag|=THIRD_BODY; continue;	}
		if (toks[i] == "HV"||toks[i] == "hv")
		{	flag|=PHOTON_RAD; continue;	};
		if (toks[i] == ")")
			continue;

		if (!product_now)
		{
			if (cur_spec == "E"||cur_spec == "e")
				flag|=ELECTRON; 
			if ((cur_spec=="=")||(cur_spec=="<=>")||(cur_spec=="=>"))
				product_now = true;
			else
				reactants.push_back(cur_spec);
		}
		else
		{
			
			if (cur_spec == "E"||cur_spec == "e")
				flag|=ELECTRON; 		
			products.push_back(cur_spec);
		}
	}
	
	return flag;


}

void REKS_reaction_reader::parse_reaction(std::ifstream & inp, vector<REKS_specie_thermo *> speclist, int &line_number)
{
	char buffer[BUFFER_MAX];
	int len;
	bool end_of_file;
	int reaction_type;
	vector<string> toks, reactants, products;
	REAL co_alpha, co_beta, co_E;
	unsigned int special = 0x00000000;
	vector<string> aux_tokens;
	vector<string> third_specs;

	REKS_reaction *p_curRxn=NULL; //current reaction pointer
	TSpec_order_param cur_spec_order;
	int i, j;
	REAL eff;
	REKS_specie_thermo * react;

	int rxn_no=0;
	do
	{
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		//debug
		
		ignore_comments(buffer);
		ignore_Espace(buffer);
		while (continued(buffer))
		{
			len = strlen(buffer);
			end_of_file = (inp.getline(&buffer[len], BUFFER_MAX-len, '\n')).eof();
			line_number++;
		}
		
		if ((reaction_type=reaction_line(buffer))>0) //this is a reaction line, it has symbol  "=","<=>" or "=>"
		{
			rxn_no++;
		
		//origianl balance check place, won't check the last (or the only ) reaction	
		//	if (p_curRxn!=NULL)
		//	{
		//		if (!p_curRxn->balance_check())
		//		{
		//			cerr<<"Error! line number: "<<line_number<<endl;
		//			return;
		//		}
		//	}
			//CompReactionCheck(line_number, p_curRxn, speclist);
			toks.clear();			
			get_reaction_token(buffer,toks);
			int n_toks = toks.size();
			string co_t= toks[n_toks-1];
			co_E = atof(co_t.c_str());
			co_t= toks[n_toks-2];
			co_beta = atof(co_t.c_str());
			co_t= toks[n_toks-3];
			co_alpha = atof(co_t.c_str());
			toks.pop_back();
			toks.pop_back();
			toks.pop_back();
			special = fill_reactants_products(toks, reactants, products, third_specs);
			//push every thing to the current reaction pointer
			p_curRxn = new REKS_reaction();
			p_curRxn->type=reaction_type;
			p_curRxn->rxn_number=kinetics->m_p_reactions.size();//make the index number to be the Reaction number
			kinetics->m_p_reactions.push_back(p_curRxn);
			if (special & THIRD_BODY)
				p_curRxn->is_three_body = true;
			if (special & PRES_DEP)
				p_curRxn->is_Lindemann = true;
			if (special & THIRD_BODY_SPEC)
				p_curRxn->is_third_spec = true;
			p_curRxn->reaction_flag = special;
			
			for (i=0; i<third_specs.size(); i++)
				p_curRxn->third_specs.push_back(get_specp(third_specs[i], speclist));
			
			react = NULL;
			for (i=0; i<reactants.size(); i++)
			{
				react = spec_keyword(reactants[i], speclist, eff);
				if (react)
				{
					int pos;
					if (eff==0)
						eff=1.0;
					if ((pos=p_curRxn->already_in_rct(react))!=-1)
						p_curRxn->reactant_effs[pos]+=eff;
					else
					{
						p_curRxn->reactants.push_back(react);
						react->add_reaction(kinetics->m_p_reactions.size()-1); //add the reaction to the spec's reaction list
						p_curRxn->reactant_effs.push_back(eff);
					}
				}
				else
				{
					cerr<<"Error! line number: "<<line_number<<endl;
					cerr<<"Reactant: "<<reactants[i]<<" Not found in Thermo database!"<<endl;
					cerr<<endl;
				}
			}
			react = NULL;
			for (i=0; i<products.size(); i++)
			{
				react = spec_keyword(products[i], speclist,eff);
				if (react)
				{
					int pos;
					if (eff==0)
							eff=1;
					if ((pos=p_curRxn->already_in_pdt(react))!=-1)
						p_curRxn->product_effs[pos]+=eff;
					else
					{
						p_curRxn->products.push_back(react);
						react->add_reaction(kinetics->m_p_reactions.size()-1); //add the reaction to the spec's reaction list
						p_curRxn->product_effs.push_back(eff);
					}
				}
				else
				{
					cerr<<"Error! line number: "<<line_number<<endl;
					cerr<<"Product: "<<products[i]<<" Not found in Thermo database!"<<endl;
					cerr<<endl;
				}
			}
			p_curRxn->arrhe_params.A = co_alpha;
			p_curRxn->arrhe_params.beta = co_beta;
			p_curRxn->arrhe_params.E = co_E;
			if (p_curRxn!=NULL)
			{
				if (!p_curRxn->balance_check())
				{
					cerr<<"Element balance error happened at reaction file line number: "<<line_number<<endl;
					cerr<<endl;
					
					return;
				}
			}

		}			
		else //Auxililiary Information Data
		{
		//	if (line_number>=408)
		//		cout<<"crap"<<endl;
			int x=get_token(buffer, aux_tokens);
			for (i=0; i<aux_tokens.size(); i++)
			{
				switch (reaction_keyword(aux_tokens[i]))
				{
				case 1:
					p_curRxn->is_fall_of=true;
					p_curRxn->fall_of.A=atof(aux_tokens[++i].c_str());
					p_curRxn->fall_of.beta=atof(aux_tokens[++i].c_str());
					p_curRxn->fall_of.E=atof(aux_tokens[++i].c_str()); 
					break;
				case 2:
					p_curRxn->is_chem_act=true;
					p_curRxn->chem_act.A=atof(aux_tokens[++i].c_str());
					p_curRxn->chem_act.beta=atof(aux_tokens[++i].c_str());
					p_curRxn->chem_act.E=atof(aux_tokens[++i].c_str()); 
					break;
				case 3:
					p_curRxn->is_Troe=true; 
					p_curRxn->is_Lindemann=false;
					p_curRxn->troe_params.a=atof(aux_tokens[++i].c_str());
					p_curRxn->troe_params.T3star=atof(aux_tokens[++i].c_str());
					p_curRxn->troe_params.Tstar=atof(aux_tokens[++i].c_str());
					if (is_a_param_REAL(aux_tokens[i+1]))
						p_curRxn->troe_params.T2star=atof(aux_tokens[++i].c_str()); //not keyword, must be the optional param 4
					break;
				case 4:
					p_curRxn->is_SRI=true; 
					p_curRxn->is_Lindemann=false;
					p_curRxn->sri_params.a=atof(aux_tokens[++i].c_str());
					p_curRxn->sri_params.b=atof(aux_tokens[++i].c_str());
					p_curRxn->sri_params.c=atof(aux_tokens[++i].c_str());
					if (is_a_param_REAL(aux_tokens[i+1]))
						p_curRxn->sri_params.d=atof(aux_tokens[++i].c_str());
					if (is_a_param_REAL(aux_tokens[i+1]))
						p_curRxn->sri_params.e=atof(aux_tokens[++i].c_str());
					break;
				case 5:
					p_curRxn->is_LandauTeller=true;
					p_curRxn->lt_params.Bi = atof(aux_tokens[++i].c_str());
					p_curRxn->lt_params.Ci = atof(aux_tokens[++i].c_str());	
					break;
				case 6:
					p_curRxn->if_spec_Eq79=true;
					for (j=0; j<8; j++) p_curRxn->eq79_bn[j++]= atof(aux_tokens[++i].c_str());
					p_curRxn->eq79_bn[j] = atof(aux_tokens[++i].c_str());	
					break;
				case 7:
					p_curRxn->if_spec_Eq80=true;
					for (j=0; j<4; j++) p_curRxn->eq80_bn[j++]= atof(aux_tokens[++i].c_str());
					p_curRxn->eq80_bn[j] = atof(aux_tokens[++i].c_str());	
					break;
				case 8:
					p_curRxn->if_spec_HV=true;
					p_curRxn->wave_length= atof(aux_tokens[++i].c_str()); 
					break;
				case 9:
					p_curRxn->if_spec_temp_dep=true;
					while (spec_keyword(aux_tokens[i+1], speclist,eff) &&is_param_spec(aux_tokens, i+1, speclist))
						p_curRxn->temp_dep_specs.push_back(aux_tokens[++i]);
					break;
				case 10:
					p_curRxn->if_spec_energy_loss=true;
					p_curRxn->e_loss= atof(aux_tokens[++i].c_str());	
					break;
				case 11:
					p_curRxn->is_MOME = true;	
					break;
				case 12:
					p_curRxn->is_XSMI = true;	
					break;
				case 13:
					p_curRxn->if_spec_reverse = true;
					p_curRxn->reverse_params.A = atof(aux_tokens[++i].c_str());
					p_curRxn->reverse_params.beta = atof(aux_tokens[++i].c_str());
					p_curRxn->reverse_params.E = atof(aux_tokens[++i].c_str());
					break;
				case 14:
					p_curRxn->if_spec_order_forward = true;
					while (spec_keyword(aux_tokens[i+1], speclist, eff)
							&&!is_param_spec(aux_tokens, i+1, speclist))
					{	

						cur_spec_order.spec =get_specp(aux_tokens[++i], speclist);
						//cur_spec_order.spec_name = aux_tokens[++i].c_str();
						cur_spec_order.vki1 = atof(aux_tokens[++i].c_str());
						cur_spec_order.vki2 = atof(aux_tokens[++i].c_str());
						(p_curRxn->forward_order).push_back(cur_spec_order); 
					}
					break;
				case 15:
					p_curRxn->if_spec_order_reverse = true;
					while (spec_keyword(aux_tokens[i+1], speclist,eff)
							&&!is_param_spec(aux_tokens, i+1, speclist))
					{	
						cur_spec_order.spec =get_specp(aux_tokens[++i], speclist);
						//cur_spec_order.spec_name = aux_tokens[++i].c_str();
						cur_spec_order.vki1 = atof(aux_tokens[++i].c_str());
						cur_spec_order.vki2 = atof(aux_tokens[++i].c_str());
						(p_curRxn->reverse_order).push_back(cur_spec_order); 
					}
					break;
				case 16:
					p_curRxn->if_spec_rxn_units = true;
					while (p_curRxn->is_legal_unit(aux_tokens[i+1]))
						p_curRxn->units.push_back(aux_tokens[++i]);
					break;
				case 17:
					p_curRxn->is_duplicate = true;
					for (j =0; j<kinetics->m_p_reactions.size(); j++)
						if (((*p_curRxn)==(*(kinetics->m_p_reactions[j])))&&(kinetics->m_p_reactions[j]!=p_curRxn))
						{   
							p_curRxn->duplicate = kinetics->m_p_reactions[j]->rxn_number;	
							break; 
						}
					break;
				case -1:
					return; //this is the end of the file;
				default:
					if (is_param_spec(aux_tokens, i, speclist))
					{   
						string spec=aux_tokens[i];
						REAL eff=atof(aux_tokens[++i].c_str());
						p_curRxn->thirdbodies.insert(pair<string,REAL>(spec, eff)); 
					}
					else
					{  
						string spec=aux_tokens[i];
						cerr<<"Line number: "<<line_number<<endl;
						cerr<<"expecting a keyword: "<< spec <<endl; 
						return; 
					}
				}
			}
		}
	} while(!end_of_file);
}				
	
	


REKS_specie_thermo * REKS_reaction_reader::spec_keyword(string spec_name, vector<REKS_specie_thermo *> speclist, REAL& eff)
{
	int i=0;
	char temp[17];
	string spec;
	string debugt;
	strcpy(temp,spec_name.c_str());
	for (i=0; i<strlen(temp); i++)
		if ((temp[i]<='9')&&(temp[i]>='0')||(temp[i]=='.'))
			continue;
		else
			break;  

	spec=string((char *)(&temp[i]));
	//the above code remove the leading coeffiency number
	
	temp[i]='\0';
	eff = atof(temp); //eff is the leading coeffiency number
	

	for (i=0; i<speclist.size(); i++)
	{
		debugt=speclist[i]->m_spec_name;
		if ((speclist[i]->m_spec_name)==spec)
			return speclist[i];
	}
	return NULL;
}

int REKS_reaction_reader::reaction_keyword(string oword)
{
	char temp[256];
	int i;
	string word;

	strcpy(temp,oword.c_str());
	for (i=0; i<strlen(temp); i++)
		temp[i]=(char)toupper(temp[i]);
	temp[i]='\0';
	word = string(temp);

	if (match(word, "LOW"))
      return 1; //low pressure, fall-off reaction
    else if (match(word, "HIGH"))
      return 2; //high pressure, chemically activated bimolecular reactions
    else if (match(word, "TROE"))
      return 3; //Troe pressure-dependent reaction
	else if (match(word, "SRI"))
      return 4; //SRI pressure-dependent reaction
	else if (match(word, "LT"))
      return 5; //Landau-Teller parameters
    else if (match(word, "JAN"))
      return 6; //optional rate-constant fit expression Eq(79)
	else if (match(word, "FIT1"))
      return 7; //optional rate-constant fit expression Eq(80)
	else if (match(word, "HV"))
      return 8; //radiation wavelength
	else if (match(word, "TDEP"))
      return 9; //species on whose temperature the reaction depends
	else if (match(word, "EXCI"))
      return 10; //energy loss per reaction event
	else if (match(word, "MOME"))
      return 11; //momentum-tranfer collision frequency for electrons
	else if (match(word, "XSMI	"))
      return 12; //flag for collision cross-section information for determination of ion momentum-transfer collision frequencies in a plasma simulation
	else if (match(word, "REV"))
      return 13; //reversible reaction arrhenius parameters
	else if (match(word, "FORD"))
      return 14; //forward reaction description
	else if (match(word, "RORD"))
      return 15; //reverse reaction description
	else if (match(word, "UNITS"))
      return 16; //specify the units for a particular reaction rate 
	else if (match(word, "DUP"))
      return 17; //duplicate reactions
    else if (match(word, "END"))
	  return -1;
	else
      return 0; //not a key word
}

bool REKS_reaction_reader::is_a_param_REAL(string word)
{
	return is_number(word);
}

bool REKS_reaction_reader::is_param_spec(vector<string> tokens, int pos, vector<REKS_specie_thermo *> speclist)
{
	REAL eff;

	if (spec_keyword(tokens[pos], speclist, eff))
	{
		if ((pos+1)>=tokens.size())
			return false;
		if (is_a_param_REAL(tokens[pos+1]))
		{	
			if ((pos+2)>=tokens.size())
				return true;
			if (!is_a_param_REAL(tokens[pos+2]))
				return true;
			else
				return false;
		}
		else
			return false;
	}
	else
		return false;
		
}

void REKS_reaction_reader::parse(vector<string> inp_files)
{
	char buffer[BUFFER_MAX+1];

	vector<string> toks; //tokerns 
	int pos=0;
	int num_toks;
	bool end_of_file;
	int line_number;
	int i;

	for (i=0; i<inp_files.size(); i++)
	{
		std::ifstream inp(inp_files[i].c_str());
		if (!inp.is_open())
		  {
		    cout<<"Error opening "<<inp_files[i]<<"! Exit."<<endl;
		    exit(-1);
		  }
		line_number = 0;
		cout<<"Parsing file "<<inp_files[i]<<"......"<<endl;

		do {
			end_of_file= (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
			line_number++;
			if (end_of_file)
			{
				cerr<<"Warning! File "<<inp_files[i]<<" line number: "<<line_number<<endl;
				cerr<<"Reach the end of CK input file before any real data."<<endl;
				goto NextFile;
			}
			pos=0;
			ignore_comments(buffer);
		} while(!(num_toks=get_token4(buffer, toks))); //toks get the tokens of the current line

		do  //get a line
		{
			switch (is_keyword(toks[pos]))
			{
				case 4:
				if (num_toks>1) //Unit is specified after the REACTION
				{
					switch (unit_keyword(toks[++pos]))
					{
					case 1:
					  Rc = 1.987; //yang 
					  break;
					case 2:
					  Rc = 1.987;
					  break;
					case 3:
					  Rc = 1.987;
					  break;
					default:
					  Rc = 1.987;
					}
				}
				else
					//Rc = 8.3144126E+7;
					Rc = 1.987; //1.98726;

				parse_reaction(inp,kinetics->reks_thrm_info->m_thrm_specs, line_number);
				//if (strictness)
				//	p_rxn_reader->parse_reaction(ckinp, p_strict_trm_reader->m_specs);
				//else
				//	p_rxn_reader->parse_reaction(ckinp, p_thermo_reader->m_specs);
				pos=num_toks;
				break;

			default:
				pos++;
				//return;
			}

			if (pos<num_toks)
				continue;
			else
			{
				do {
					end_of_file=(inp.getline(buffer, BUFFER_MAX, '\n')).eof();
					line_number++;
				
					pos=0;
					ignore_comments(buffer);
				}
				while(!(num_toks=get_token4(buffer, toks))&&!end_of_file); //toks get the tokens of the current line
			
				continue;
			}

		} while (!end_of_file);

NextFile:
		inp.close();
#ifdef DEBUG
		cout<<"Parsing file "<<inp_files[i]<<" Done!"<<endl;
#endif
	};
	return;
}

void REKS_reaction_reader::dump()
{
	int i;
	for (i=0; i<kinetics->m_p_reactions.size(); i++)
		kinetics->m_p_reactions[i]->dump();
}

