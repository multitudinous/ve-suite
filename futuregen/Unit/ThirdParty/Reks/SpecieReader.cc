// SpecieReader.cpp: implementation of the SpecieReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////

#include "SpecieReader.h"
#include "StringParseUtil.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

REKS_specie_reader::REKS_specie_reader()
{
}

REKS_specie_reader::~REKS_specie_reader()
{

}

bool REKS_specie_reader::spec_length_check()
{
	for (vector<string>::iterator i=reks_thrm_info->m_specs.begin(); i!=reks_thrm_info->m_specs.end(); i++)
		if((i->length()>=16) || (i->length()<=0))	
			return false;	
	return true;
}

bool REKS_specie_reader::already_in(string spec)
{
	for (vector<string>::iterator i=reks_thrm_info->m_specs.begin(); i!=reks_thrm_info->m_specs.end(); i++)
		if ((*i)==spec)
			return true;
	return false;
}

bool REKS_specie_reader::fill_specs(vector<string>specs, int& pos, int line_num)
{
	for (int i=pos; i < specs.size(); i++)
    {	
		switch (is_keyword(specs[i]))
		{
			case 0: 
				break;
			case 5: 
				pos = i+1;
				return false;
			default:
				pos++;
				cerr<<"Error! Line number: "<<line_num<<endl;
				cerr<<"Still reading elements, not supposed to have other Keyword inside"<<endl;
				return false;
		}
		
		pos++;

		if (!check_initial(specs[i],"+=1234567890"))
		  {
		    cerr<<"Error! Line number: "<<line_num<<endl;
		    cerr<<"Species name can't begin with +,=, or a number"<<endl;
		    return false;
		  }

		if (already_in(specs[i]))
			continue;
		reks_thrm_info->m_specs.push_back(specs[i]);
	}

	return true;
}

void REKS_specie_reader::parse(vector<string> inp_files)
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
		} while(!(num_toks=get_token(buffer, toks))); //toks get the tokens of the current line

		do  //get a line
		{
			switch (is_keyword(toks[pos]))
			{
				case 2:  //species
					pos++;
					while(fill_specs(toks,pos, line_number))
					{
						end_of_file=(inp.getline(buffer, BUFFER_MAX, '\n')).eof(); //get a line
						line_number++;
						ignore_comments(buffer);
						if (end_of_file)
						{
							cerr<<"Warning! line number: "<<line_number<<endl;
							cerr<<"End of file before end of the species block."<<endl;
							goto NextFile;
						}
						num_toks=get_token(buffer, toks); //toks get the tokens of the current line
						pos = 0; //reset pos
					}
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

void REKS_specie_reader::dump()
{
	cout<<"SPECIES : "<<endl;
	for (vector<string>::iterator i=reks_thrm_info->m_specs.begin(); i!=reks_thrm_info->m_specs.end(); i++)
		cout<<*i<<endl;
}

