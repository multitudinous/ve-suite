#include <iostream>
#include <fstream>

#include "FC_Process.h"

namespace Vision21 {

using namespace std;

void FC_Process::dump()
{
  map<string, double>::iterator iter;

  cout<<"Anode :"<<endl;
  for (iter=anode_s.begin(); iter!=anode_s.end(); iter++)
    cout<<iter->first<<"   "<<iter->second<<endl;
  cout<<"Anode T:  "<<anode_T<<endl;
  cout<<"Anode P:  "<<anode_P<<endl;
  cout<<"Anode Utilization:  "<<anode_U<<endl;
  
  cout<<"Cathode :"<<endl;
  for (iter=cathode_s.begin(); iter!=cathode_s.end(); iter++)
    cout<<iter->first<<"   "<<iter->second<<endl;
  cout<<"Cathode T:  "<<cathode_T<<endl;
  cout<<"Cathode P:  "<<cathode_P<<endl;
  cout<<"Cathode Utilization:  "<<cathode_U<<endl;
  
  cout<<"Produced Power [watt]:  "<<Produced_Power<<endl;
  cout<<"Overall Q Gen. [watt]:  "<<Overall_Q<<endl;
}


bool FC_Process::parse(string input_path)
{
  ifstream inputf(input_path.c_str());
  vector<string> tokens;

  char buffer[BUFFER_MAX+1];
  int num_toks;
  int i;
  
  names.clear();
  
  if (!inputf) //file open error
    return false;
    
  while (!(inputf.getline(buffer, BUFFER_MAX, '\n')).eof())
    {
      num_toks=get_token(buffer, tokens);
      
      if (num_toks < 12) // the beginning part and the end part;
	continue;

      if (names.size()==0)
	for (i=0; i<num_toks; i++)
	  names.push_back(tokens[i]);
      else
	{
	  values.clear();
	  for (i=0; i<num_toks; i++)
	    values.push_back(atof(tokens[i].c_str()));
	}
      
    }

  inputf.close();

  process();

  return true;
  
}

void FC_Process::process()
{
  char buf[BUFFER_MAX];
  char buf2[100];
  vector<string> toks;
  int num_toks;
  int len;
  int i;
  
  for(i=0; i<values.size(); i++)
    { 
      strcpy(buf, names[i].c_str());
      
      num_toks=get_token2(buf, toks);
      
      
      if (num_toks==2&&(toks[0]=="Anode"||toks[0]=="Cathode")) // the species
	{
	  strcpy(buf2, toks[1].c_str());
	  len=strlen(buf2);
	  
	  if (match(toks[1], "Sp"))
	    {	 
	      if (buf2[len-1]==flag)
		{
		  buf2[len-1]='\0';
		  if (toks[0]=="Anode")
		    anode_s[string(buf2+2)]=values[i];         //string(buf2+2), skip the 'Sp'
		  else if (toks[0]=="Cathode")
		    cathode_s[string(buf2+2)]=values[i]; 
		  else;
		}
	      else;
	    }
	  else if (match(toks[1], "T"))
	    {
	      if (buf2[len-1]==flag)
		{
		  if (toks[0]=="Anode")
		    anode_T=values[i]; 
		  else if (toks[0]=="Cathode")
		    cathode_T=values[i]; 
	      else;
		}
	      else; 
	    }
	  else if (match(toks[1], "P"))
	    {
	      if (buf2[len-1]==flag)
		{
		  if (toks[0]=="Anode")
		    anode_P=values[i]; 
		  else if (toks[0]=="Cathode")
		    cathode_P=values[i]; 
		  else;
		}
	      else; 
	    }
	  else if (match(toks[1], "Utilization"))
	    {
	      if (toks[0]=="Anode")
		anode_U=values[i]; 
	      else if (toks[0]=="Cathode")
		cathode_U=values[i]; 
	      else;
	    }
	}
      else if (num_toks==3&&toks[0]=="Produced"&&toks[1]=="Power"&&toks[2]=="[watt]")
	Produced_Power = values[i];
      else if (num_toks==4&&toks[0]=="Overall"&&toks[1]=="Q"&&toks[2]=="Gen."&&toks[3]=="[watt]")
	Overall_Q = values[i];
      else 
	continue;
    }
  
  return;
}

int FC_Process::get_token(char* current_line, vector<string>& toks)
{
  char* token;
  int i=0;
  token = strtok(current_line, ",\n");
  
  toks.clear();
  while( token )
    {
      i++;
      toks.push_back(string(token));
      token = strtok(NULL, ",\n");
    }
  
  return i;
}

int FC_Process::get_token2(char* current_line, vector<string>& toks)
{
  char* token;
  int i=0;
  token = strtok(current_line, " \n");
  
  toks.clear();
  while( token )
    {
      i++;
      toks.push_back(string(token));
      token = strtok(NULL, " \n");
    }
  
  return i;
}


bool FC_Process::match(const string& s1, const string& s2)
{
    size_t n = s2.size();
    if (s1.size() < n) return false;
    for (size_t i = 0; i < n; i++) 
	if (s2[i] != '*' && (s1[i] != s2[i])) return false;
    return true;
}

} // end namespace Vision21
