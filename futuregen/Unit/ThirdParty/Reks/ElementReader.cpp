// ElementReader.cpp: implementation of the ElementReader class.
//
// Copyright 2002  Reaction Engineering International
// by Yang
//////////////////////////////////////////////////////////////////////
#include "GlobalConst.h"
#include "ElementReader.h"
#include "StringParseUtil.h"
//////////////////////////////////////////////////////////////////////
// Construction/Destruction
//////////////////////////////////////////////////////////////////////

//Constructor just clear up the elements list
REKS_element_reader::REKS_element_reader()
{
}

REKS_element_reader::~REKS_element_reader()
{
}

//check is the element's length is valid. Must be 1 or 2 characters
bool REKS_element_reader::element_length_check()
{
	for (REKS_element_list::iterator i=reks_thrm_info->m_elements.begin(); i!=reks_thrm_info->m_elements.end(); i++)
		if(((*i)->m_name).length()>2 || ((*i)->m_name).length()<=0)	
			return false;	
	return true;
}

//check if it's already on the periodic chart, 
//if it is already on, weight information can be read from there	
bool REKS_element_reader::is_on_chart(string elem)
{
	bool on_chart = false;
	for (int i=0; i< CHART_LEN; i++)
		if(periodic_chart_name[i]==elem)
		{
			on_chart=true;
			break;
		}

	return on_chart;

}

//check if it's already in the list
bool REKS_element_reader::already_in(string elem)
{
  for (REKS_element_list::iterator i=reks_thrm_info->m_elements.begin(); i!=reks_thrm_info->m_elements.end(); i++)
    if ((*i)->m_name==elem)
      return true;
  return false;
}

//get the weight value out of the periodic chart
REAL REKS_element_reader::weight_in_chart(string elem)
{
  int i;
  char temp[256];
  string word;

  strcpy(temp,elem.c_str());
  for (i=0; i<strlen(temp); i++)
    temp[i]=(char)toupper(temp[i]);
  temp[i]='\0';
  word = string(temp);

  REAL result =-1;
  for ( i=0; i<CHART_LEN; i++)
    if (periodic_chart_name[i]==word)
      {
	result =periodic_chart_wgt[i];
	break;
      }
  return result;
}

//parse one line for elements
//if it hits keyword "END", ends
//during parsing, you need check of the token length
//if it is not on the periodic chart, it must specify the weight
//if a element name occured more than once, ignore execpt the first time
bool REKS_element_reader::fill_elems(vector<string> elems, int& pos, int line_num) 
{
  REKS_element *p_cur_elem=NULL;
  REAL cur_weight=-1;
  bool need_number_next = false;
  int i;

  for (i=pos; i < elems.size(); i++)
    {
		switch (is_keyword(elems[i]))
		{
			case 0: 
				break;
			case 5: 
				pos = i+1;
				return false; //section ends
			default:
				pos++;
				cerr<<"Error! Line number: "<<line_num<<endl;
				cerr<<"Still reading elements, not supposed to have other Keyword inside"<<endl;
				return false; //section ends because of error
		}

		pos++;
		
		if (!is_number(elems[i])) 
		{ 
		//this is an element;
			if (need_number_next)
			{
				cerr<<"Error! Line number: "<<line_num<<endl;
				cerr<<"We need a number after "<<elems[i-1]<<endl;
				return false;
			}
			if (!element_length_check()) 
			{
				cerr<<"Error! Line number: "<<line_num<<endl;
				cerr<<elems[i]<<"length check failed"<<endl;
				return false;
			}
	  
			if (already_in(elems[i])) //already in the element list, ignored
				continue;
	  
			p_cur_elem = new REKS_element(elems[i]);
			reks_thrm_info->m_elements.push_back(p_cur_elem);
			cur_weight = weight_in_chart(elems[i]);
	  
			if (cur_weight!=-1)
			{
				p_cur_elem->m_atomic_weight=cur_weight;
				p_cur_elem->m_is_on_chart = true;
				need_number_next = false; //don't have to be a number next
			}
			else
			{ //can't find it on the chart
				p_cur_elem->m_is_on_chart = false;
				need_number_next = true;
			}
		}
		else
		{
			if (need_number_next)
				need_number_next = false;
			p_cur_elem->m_atomic_weight = atof(elems[i].c_str());
		}
	}
	return true; //not done yet
}

void REKS_element_reader::parse(vector<string> inp_files)
{
	char buffer[BUFFER_MAX+1];
	int line_number;
	vector<string> toks; //tokerns 
	int pos=0;
	int num_toks;
	bool end_of_file;
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
				cerr<<"Reach the end of input file before any real data."<<endl;
				goto NextFile;
			}
			pos=0;
			ignore_comments(buffer);
		} while(!(num_toks=get_token(buffer, toks))); //toks get the tokens of the current line

		do  //get a line
		{
			switch (is_keyword(toks[pos]))
			{
				case 1:  //elements 
					pos++;
					while(this->fill_elems(toks,pos, line_number))
					{
						end_of_file=(inp.getline(buffer, BUFFER_MAX, '\n')).eof(); //get a line
						line_number++;
						if (end_of_file)
						{
							cerr<<"Warning! File "<<inp_files[i]<<" line number: "<<line_number<<endl;
							cerr<<"End of file before end of the elements block."<<endl;
							goto NextFile;
						}

						ignore_comments(buffer);
						num_toks=get_token(buffer, toks); //toks get the tokens of the current line
						pos = 0; //reset pos
					}
					pos = num_toks;
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

void REKS_element_reader::dump()
{
	cout<<"ELEMENTS : "<<endl;
	for (REKS_element_list::iterator i=reks_thrm_info->m_elements.begin(); i!=reks_thrm_info->m_elements.end(); i++)
		cout<<(*i)->m_name<<"  "<<(*i)->m_atomic_weight<<"  "<<(*i)->m_is_on_chart<<"  "<<endl;
}




