// MixedThermoReader.cpp: implementation of the MixedThermoReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#include "MixedThermoReader.h"

//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////


REKS_mixed_thermo_reader::REKS_mixed_thermo_reader()
{

}

REKS_mixed_thermo_reader::~REKS_mixed_thermo_reader()
{

}

void REKS_mixed_thermo_reader::parse_thermo(std::ifstream &inp, int& line_number,
					    REKS_element_list elems, vector<string> specs)
{
	char buffer[BUFFER_MAX+1];
	char result[19];
	char natom[4];
	REAL a[14];
	int i;
	int pos;
	bool end_of_file = false;
	string spec_name;
	vector <AtomicFormula> atom_f;
	char phase;
	REAL high_temp, low_temp, common_temp;
	REKS_specie_thermo * p_spec_thermo;
	string date;
	AtomicFormula cur_atom;
	bool replaced;

#ifndef REKS_BANFF
//Step 2 of the chemkin
	do {
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		if (end_of_file)
			return;
		ignore_REIcomments(buffer);
		ignore_Espace(buffer);
		if (check_END(buffer))
			return;
	}while (strlen(buffer)==0);

	strncpy(result, buffer, 10);
	result[10]='\0';
	reks_thrm_info->lowestT = atof(result);

	strncpy(result, &buffer[10], 10);
	result[10]='\0';
	reks_thrm_info->commonT = atof(result);

	strncpy(result, &buffer[20], 10);
	result[10]='\0';
	reks_thrm_info->highestT = atof(result);
#else
	lowestT = 0;
	commonT = 100;
	highestT =100000;
#endif

repeat:

//Step 3 of chemkin
	do {
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		//Yang debug
		//if (line_number==611)
		// {
		//   cout<<"debug 611"<<endl;
		//   cout<<"It is debuged"<<endl;
		//    cout<<"Hello from Yang"<<endl;
		//  }

		if (end_of_file)
			return;
		if (ignore_REIcomments(buffer)==-123) //this is a free style line
		{
			parse_freestyle(buffer, inp, p_spec_thermo, line_number);
			
			//check if we need add this guy into the species thermo list
#ifndef REKS_BANFF
			if (!p_spec_thermo->check_spec(specs))
			{
				delete p_spec_thermo;
				p_spec_thermo=NULL;
				goto repeat;
			}
#endif
			//check if all the elements in the specie is contained in the elements list	
			if (!p_spec_thermo->check_elem(elems))
			{
				cerr<<"Error in line number: "<<line_number<<endl;
				return;
			}

			if (p_spec_thermo)
			{
				replaced = false;
				//cout<<"Size now "<<reks_thrm_info->m_thrm_specs.size()<<endl;
				for (i=0; i<reks_thrm_info->m_thrm_specs.size(); i++)
				  if ((*(reks_thrm_info->m_thrm_specs[i]))==*p_spec_thermo)
				    {
				      delete reks_thrm_info->m_thrm_specs[i];
				      reks_thrm_info->m_thrm_specs[i]=p_spec_thermo;
				      reks_thrm_info->m_thrm_specs[i]->index = i;
				      reks_thrm_info->m_thrm_specs[i]->set = true;
				      replaced = true;
				      break;
				    } ;
				
				if (!replaced)
				  {
				    reks_thrm_info->m_thrm_specs.push_back(p_spec_thermo);
				    cerr<<"Error in line number: "<<line_number<<endl;
				    cerr<<"Somehow this thing passed the check of the specs but can't replace the existing species there"<<endl;
				  }
				
			}
			goto repeat;
		}

		ignore_Espace(buffer);
		if (check_END(buffer))
			return;
	}while (strlen(buffer)==0);

	strncpy(result,buffer,18);
	//some NASA file don't obey this rule well, So I need to cut for 0 to the first space before 18
	result[18]='\0';
	pos=0;
	grab_spec_name(result, pos, spec_name);
	//if (is_number(spec_name))
	//{
	//	cerr<<"Error in line number: "<<line_number<<endl;
	//	cerr<<"A Specie name is expected."<<endl;
	//	return;
	//}

	strncpy(result, &buffer[18], 6);
	result[6]='\0';
	date=result;

	atom_f.clear();
	for (i=0; i<4; i++)
	{
		strncpy(result,&buffer[24+i*5], 2);
		result[2]='\0';
		eatspace(result);
		cur_atom.symbol[0]=result[0];
		cur_atom.symbol[1]=result[1];
		strncpy(natom, &buffer[24+i*5+2],3);
		natom[3]='\0';
		cur_atom.num=atoi(natom);
		if (((result[0]=='0')||(result[0]=='\0')||(result[0]==' '&&result[1]==' '))
			&&(cur_atom.num==0))
		continue;
		if (((result[0]=='0')||(result[0]=='\0')||(result[0]==' '&&result[1]==' '))
			||(cur_atom.num==0))
		{
			cerr<<"Warning in line number: "<<line_number<<endl;
			cerr<<"Either specified an element without element number"
				<< " or specified element number with out element"<<endl;
			cerr<<"Besure chemkin format element start at column 25!"<<endl;
			continue;
		}
		atom_f.push_back(cur_atom);
	}

	phase = buffer[44];
	phase = (char) toupper(phase);
	if ((phase!='S')&&(phase!='G')&&(phase!='L'))
	{
		cerr<<"Error in line number: "<<line_number<<endl;
		cerr<<"Phase can only be S (solid), G (gas) or L (liquid)."<<endl;
		return;
	}

	strncpy(result, &buffer[45], 10);
	result[10] = '\0';
	low_temp = atof(result);

	strncpy(result, &buffer[55], 10);
	result[10] = '\0';
	high_temp = atof(result);

	strncpy(result, &buffer[65], 8);
	result[8] = '\0';

#ifndef REKS_BANFF
	common_temp = atof(result);
#else
	common_temp = 1000;
#endif

	strncpy(result,&buffer[73], 2);
	result[2]='\0';
	eatspace(result);
	cur_atom.symbol[0]=result[0];
	cur_atom.symbol[1]=result[1];
	strncpy(natom, &buffer[77],2);
	natom[2]='\0';
	cur_atom.num=atoi(natom);
	if (((result[0]=='0')||(result[0]=='\0')||(result[0]==' '&&result[1]==' '))
	    ||(cur_atom.num==0))
	  goto step4;
	
	atom_f.push_back(cur_atom);


// Step 4
 step4:
	do {
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		if (end_of_file)
			return;
		ignore_comments(buffer);
		ignore_Espace(buffer);
		if (check_END(buffer))
			return;
	}while (strlen(buffer)==0);

	for (i=0; i<5; i++)
	{
		strncpy(result, &buffer[i*15], 15);
		result[15]='\0';
		a[i]=atof(result);
	}

//step 5
	do {
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		if (end_of_file)
			return;
		ignore_comments(buffer);
		ignore_Espace(buffer);
		if (check_END(buffer))
			return;
	}while (strlen(buffer)==0);

	for (i=0; i<5; i++)
	{
		strncpy(result, &buffer[i*15], 15);
		result[15]='\0';
		a[i+5]=atof(result);
	}

//step 6
	do {
		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		if (end_of_file)
			return;
		ignore_comments(buffer);
		ignore_Espace(buffer);
		if (check_END(buffer))
			return;
	}while (strlen(buffer)==0);

	for (i=0; i<4; i++)
	{
		strncpy(result, &buffer[i*15], 15);
		result[15]='\0';
		a[i+10]=atof(result);
	}

//==============================================================================
//now try to fill up a specie_thermo instance
	p_spec_thermo = new REKS_specie_thermo();
	p_spec_thermo->m_spec_name = spec_name;
	p_spec_thermo->m_phase = phase;
	p_spec_thermo ->m_atom_form = atom_f;
	p_spec_thermo->m_low_temp = low_temp;
	p_spec_thermo->m_high_temp = high_temp;
	p_spec_thermo->m_common_temp = common_temp;
	p_spec_thermo->m_date = date;

	for (i=0; i<7; i++)
		p_spec_thermo->m_upper_temp_a[i] = a[i];
	for (i=0; i<7; i++)
		p_spec_thermo->m_lower_temp_a[i] = a[7+i];

#ifndef REKS_BANFF
//check if we need add this guy into the species thermo list
	if (!p_spec_thermo->check_spec(specs))
	{
		delete p_spec_thermo;
		goto repeat;
	}
#endif

//check if all the elements in the specie is contained in the elements list	
	if (!p_spec_thermo->check_elem(elems))
	{
		cerr<<"Error in line number: "<<line_number<<endl;
		return;
	}

//check if this is a updated information for a existed specie
	replaced = false;	
	//cout<<"Size now "<<reks_thrm_info->m_thrm_specs.size()<<endl;
	for (i=0; i<reks_thrm_info->m_thrm_specs.size(); i++)
		if ((*(reks_thrm_info->m_thrm_specs[i]))==*p_spec_thermo)
		{
			delete reks_thrm_info->m_thrm_specs[i];
			reks_thrm_info->m_thrm_specs[i]=p_spec_thermo;
			reks_thrm_info->m_thrm_specs[i]->index=i;
			reks_thrm_info->m_thrm_specs[i]->set=true;
			replaced = true;
			break;
		}

	if (!replaced)
	  {
	    reks_thrm_info->m_thrm_specs.push_back(p_spec_thermo);
	    cerr<<"Error in line number: "<<line_number<<endl;
	    cerr<<"Somehow this thing passed the check of the specs but can't replace the existing species there"<<endl;
	  }

goto repeat;	

	return;
}

void REKS_mixed_thermo_reader::parse_freestyle(char* buffer, std::ifstream &inp, 
										REKS_specie_thermo * & p_spec_thermo, int& line_number)
{
	//from now on, it's going to be free style lines
	//if any strict line happen before a record is complete, the record is deleted

	//format for the free style will be simple. 
	//1. every token is delimited by space or tab
	//2. first token is the specie name, followed by date
	//3. veriable number of atomic formulas, in each unit, first token is the element name, the second the number
	//4. ended with 14 REALs for coefficiencies

	int len;
	int i,j,num_atom_f;
	vector<string> toks;
	AtomicFormula temp;
	bool end_of_file=false;

	toks.clear();
	p_spec_thermo = new REKS_specie_thermo();

	get_token3(buffer, toks);

	while ((toks[toks.size()-1]!="@")&&(toks[toks.size()-1]!="$"))  //end of the current record
	{
		if (end_of_file)
		{
			cerr<<"Error! line number: "<<line_number<<endl;
			cerr<<"End of file before the end of the current sepecie record"<<endl;
			delete p_spec_thermo;
			return;
		}

		end_of_file = (inp.getline(buffer, BUFFER_MAX, '\n')).eof();
		line_number++;
		
		if(ignore_REIcomments(buffer)!=-123)
		{
		  //	ignore_Espace(buffer);
			//	get_token3(buffer, toks);
		  if ((toks[toks.size()-1]!="@")&&(toks[toks.size()-1]!="$"))
		    {
		      cerr<<toks[toks.size()-1]<<endl;
		      cerr<<"Error! line number: "<<line_number<<endl;
		      cerr<<"No other lines are allowed before the end of the current sepecie's free style record"<<endl;
		      delete p_spec_thermo;
		      return;
		    }
		}
		else
		{
			ignore_Espace(buffer);
			get_token3(buffer, toks);
		}
		
	} //get everything is the toks vector;
	
	if (toks[toks.size()-1]=="@") //normal free style NASA format
	  {
	    toks.pop_back(); //pop back the token "@", which is the ending token
	    
	    len = toks.size();
	    
	    //for last token, grab 14 REAL;
	    j=6;
	    for (i=len-1; i>=len-7; i--)
	      p_spec_thermo->m_lower_temp_a[j--]=atof(toks[i].c_str());
	    j=6;
	    for (i=len-8; i>=len-14; i--)
	      p_spec_thermo->m_upper_temp_a[j--]=atof(toks[i].c_str());
	    
	    //simply assign the specie name and the date
	    p_spec_thermo->m_spec_name=toks[0];
	    p_spec_thermo->m_date=toks[1];
	    
	    //the number of the atom fomula unit is the half number of the remain toks, 
	    //and that should be an even number
	    
	    num_atom_f= len - 20;
	    //14 REALs, 1 specname, 1 date, 1 phase, 3 temp
	    p_spec_thermo->m_low_temp = atof(toks[len-17].c_str());
	    p_spec_thermo->m_high_temp = atof(toks[len-16].c_str());

#ifndef REKS_BANFF
		p_spec_thermo->m_common_temp = atof(toks[len-15].c_str());
#else
		p_spec_thermo->m_common_temp = 1000;
#endif

		p_spec_thermo->m_phase = toks[len-18][0];
	    
	    if (num_atom_f/2*2!=num_atom_f) 
	    {	//not an even number, something is wrong
			cerr<<"Not an even number, something is wrong"<<endl; 
			delete p_spec_thermo;
			return;
	    }
	    
	    num_atom_f = num_atom_f/2;

	    for (i=0; i<num_atom_f; i++)
	    {
	      //cout<<"i"<<i<<"   "<<endl;
	      //cout<<toks[2+i*2]<<"   ";
	      //cout<<toks[2+i*2+1]<<endl;
	      temp.symbol[0]=toks[2+i*2].c_str()[0];
	      temp.symbol[1]=toks[2+i*2].c_str()[1];
	      temp.num= atoi(toks[2+i*2+1].c_str());
	      if ((temp.symbol[0]!='0')&&(temp.num!=0))
		p_spec_thermo->m_atom_form.push_back(temp);
	    } 
	  }
	else
	  {
	    toks.pop_back(); //pop back the token "@", which is the ending token
	    
	    len = toks.size();
	    
	    //for the last token, grab a REAL, this is the new thing
	    p_spec_thermo->delta_H = atof(toks[len-1].c_str());
	    //for last-1 token, grab 14 REAL;
	    j=6;
	    for (i=len-2; i>=len-8; i--)
	      p_spec_thermo->m_lower_temp_a[j--]=atof(toks[i].c_str());
	    j=6;
	    for (i=len-9; i>=len-15; i--)
	      p_spec_thermo->m_upper_temp_a[j--]=atof(toks[i].c_str());
	    
	    //simply assign the specie name and the date
	    p_spec_thermo->m_spec_name=toks[0];
	    p_spec_thermo->m_date=toks[1];
	    
	    //the number of the atom fomula unit is the half number of the remain toks, 
	    //and that should be an even number
	    
	    num_atom_f= len - 21;
	    //15 REALs, 1 specname, 1 date, 1 phase, 3 temp
	    p_spec_thermo->m_low_temp = atof(toks[len-18].c_str());
	    p_spec_thermo->m_high_temp = atof(toks[len-17].c_str());

#ifndef REKS_BANFF
		p_spec_thermo->m_common_temp = atof(toks[len-16].c_str());
#else
		p_spec_thermo->m_common_temp = 1000;
#endif
		p_spec_thermo->m_phase = toks[len-19][0];
	    if (num_atom_f/2*2!=num_atom_f) 
	      {	
		cout<<"not an even number, something is wrong"<<endl;
		delete p_spec_thermo;
		return;
	      }
	    
	    num_atom_f = num_atom_f/2;
	    
	    for (i=0; i<num_atom_f; i++)
	    {
	      temp.symbol[0]=toks[2+i*2].c_str()[0];
	      temp.symbol[1]=toks[2+i*2].c_str()[1];
	      temp.num= atoi(toks[2+i*2+1].c_str());
	      if ((temp.symbol[0]!='0')&&(temp.num!=0))
		p_spec_thermo->m_atom_form.push_back(temp);
	    } 
	  }
}

void REKS_mixed_thermo_reader::parse(vector<string> inp_files)
{
	char buffer[BUFFER_MAX+1];

	vector<string> toks; //tokerns 
	int pos=0;
	int num_toks;
	bool end_of_file;
	int line_number;
	int i;
	REKS_specie_thermo * p_spec_thermo;

	//Changed by Yang, the idea is to prepopulate the whole specie_thermo set with nonsense species information according to the specie list passed in by the mechanism file. So the order of species in this set will conform the orders with the species in the mech file, and an index is set here, so they can be indexed in the same order. Also a real thermo infor flag is used to tell if it is real thermo info or the initialization junk
	
	for (i=0; i<reks_thrm_info->m_thrm_specs.size();i++)
	  delete reks_thrm_info->m_thrm_specs[i];
	
	reks_thrm_info->m_thrm_specs.clear();
	
	for (i=0; i<reks_thrm_info->m_specs.size(); i++)
	  {
	    p_spec_thermo = new REKS_specie_thermo();
	    p_spec_thermo->m_spec_name = reks_thrm_info->m_specs[i];
	    //cout<<p_spec_thermo->m_spec_name<<endl;
	    p_spec_thermo->index = i;
	    p_spec_thermo->set = false;
	    reks_thrm_info->m_thrm_specs.push_back(p_spec_thermo);
	  }
	//cout<<"Size start "<<reks_thrm_info->m_thrm_specs.size()<<endl;

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
		} while(!(num_toks=get_token(buffer, toks))); //toks get the tokens of the current line

		do  //get a line
		{
			switch (is_keyword(toks[pos]))
			{
				case 3:
					parse_thermo(inp, line_number, reks_thrm_info->m_elements, reks_thrm_info->m_specs);
					pos++;
					break;
				default:
					pos++;
				//cerr<<"Not ready for this keyword yet"<<endl;
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
				while(!(num_toks=get_token(buffer, toks))&&!end_of_file); //toks get the tokens of the current line
			
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

void REKS_mixed_thermo_reader::dump()
{
	int i,k;
	for (i = 0; i<reks_thrm_info->m_thrm_specs.size(); i++)
	{
		cout<<reks_thrm_info->m_thrm_specs[i]->m_spec_name<<" "<<endl;
		cout<<reks_thrm_info->m_thrm_specs[i]->m_date<<" "<<endl;
		for (int j =0;  j<(reks_thrm_info->m_thrm_specs[i]->m_atom_form).size(); j++)
			cout<<(reks_thrm_info->m_thrm_specs[i]->m_atom_form)[j].symbol[0]<<(reks_thrm_info->m_thrm_specs[i]->m_atom_form)[j].symbol[1]<<" "<<(reks_thrm_info->m_thrm_specs[i]->m_atom_form)[j].num<<endl;
		cout<<reks_thrm_info->m_thrm_specs[i]->m_high_temp<<" "<<reks_thrm_info->m_thrm_specs[i]->m_low_temp<<" "<<reks_thrm_info->m_thrm_specs[i]->m_common_temp<<endl;
		for (k=0; k<7; k++)
			cout<<reks_thrm_info->m_thrm_specs[i]->m_upper_temp_a[k]<<" ";
		cout<<endl;
		for (k=0; k<7; k++)
			cout<<reks_thrm_info->m_thrm_specs[i]->m_lower_temp_a[k]<<" ";
		cout<<endl;
	

	}

}

