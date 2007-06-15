/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef _WIN32
#pragma warning(disable : 4786)
#endif

#include <iostream>

#include "interface.h"

Interface::Interface ()
{
  _type = 1 ;
  _category = 1; 
  _id = 1;
}

Interface::Interface (const Interface &p)
{
  copy(p);
}

Interface::~Interface ()
{

}

void Interface::copy (const Interface& p)
{
  if(this==&p) return;

  clear();

  _type = p._type ;
  _category = p._category; 
  _id = p._id;

  _Int      = p._Int;
  _Double   = p._Double;
  _String   = p._String;
  _Int1D    = p._Int1D;
  _Double1D = p._Double1D;
  _String1D = p._String1D;
}

void Interface::clear ()
{
  _Int.clear();
  _Double.clear();
  _String.clear();
  _Int1D.clear();
  _Double1D.clear();
  _String1D.clear();
}

long Interface::getInt (std::string var, bool *f)
{
  if(f) *f = true;
  if(_Int.find(var)!=_Int.end()) return _Int[var];
  if(f) *f = false;
  return 0;
}

double Interface::getDouble (std::string var, bool *f)
{
  if(f) *f = true;
  if(_Double.find(var)!=_Double.end()) return _Double[var];
  if(f) *f = false;
  return 0;
}

std::string Interface::getString (std::string var, bool *f)
{
  if(f) *f = true;
  if(_String.find(var)!=_String.end()) return _String[var];
  if(f) *f = false;
  return "";
}

std::vector<long> Interface::getInt1D (std::string var, bool *f)
{
  if(f) *f = true;
  if(_Int1D.find(var)!=_Int1D.end()) return _Int1D[var];
  if(f) *f = false;
  std::vector<long> ret;
  return ret;
}

std::vector<double> Interface::getDouble1D (std::string var, bool *f)
{
  if(f) *f = true;
  if(_Double1D.find(var)!=_Double1D.end()) return _Double1D[var];
  if(f) *f = false;
  std::vector<double> ret;
  return ret;
}

std::vector<std::string> Interface::getString1D (std::string var, bool *f)
{
  if(f) *f = true;
  if(_String1D.find(var)!=_String1D.end()) return _String1D[var];
  if(f) *f = false;
  std::vector<std::string> ret;
  return ret;
}

std::vector<std::string> Interface::getInts ()
{
  std::vector<std::string> ret;
  for(std::map<std::string, long>::iterator iter=_Int.begin(); iter!=_Int.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

std::vector<std::string> Interface::getDoubles ()
{
  std::vector<std::string> ret;
  for(std::map<std::string, double>::iterator iter=_Double.begin(); iter!=_Double.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

std::vector<std::string> Interface::getStrings ()
{
  std::vector<std::string> ret;
  for(std::map<std::string, std::string>::iterator iter=_String.begin(); iter!=_String.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

std::vector<std::string> Interface::getInts1D ()
{
  std::vector<std::string> ret;
  for(std::map<std::string, std::vector<long> >::iterator iter=_Int1D.begin(); iter!=_Int1D.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

std::vector<std::string> Interface::getDoubles1D ()
{
  std::vector<std::string> ret;
  for(std::map<std::string, std::vector<double> >::iterator iter=_Double1D.begin(); iter!=_Double1D.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

std::vector<std::string> Interface::getStrings1D ()
{
  std::vector<std::string> ret;

  for(std::map<std::string, std::vector<std::string> >::iterator iter=_String1D.begin(); iter!=_String1D.end(); iter++)
    ret.push_back(iter->first);
  return ret;
}

/*
bool Interface::pack (std::string &packed)
{
  std::map<std::string, long>::iterator iter=_Int.begin();
  std::map<std::string, double>::iterator iter0=_Double.begin();
  std::map<std::string, std::string>::iterator iter1=_String.begin();
  std::map<std::string, std::vector<long> >::iterator iter2=_Int1D.begin();
  std::vector<long> long_vec;
  std::map<std::string, std::vector<double> >::iterator iter4=_Double1D.begin();
  std::vector<double> double_vec;
  std::map<std::string, std::vector<std::string> >::iterator iter6=_String1D.begin();
  std::vector<std::string> string_vec;
  unsigned int ii;
  
  packed = "_INT_RES_";
  packed += DELIM + to_string(_Int.size());
  for(; iter!=_Int.end(); iter++)
    packed += DELIM + iter->first + DELIM + to_string((int)iter->second);
  packed += DELIM;
  
  packed += "_DOUBLE_RES_";
  packed += DELIM + to_string(_Double.size());
  for(; iter0!=_Double.end(); iter0++)
    packed += DELIM + iter0->first + DELIM + to_string((double)iter0->second);
  packed += DELIM;
  
  packed += "_STRING_RES_";
  packed += DELIM + to_string(_String.size());
  for(; iter1!=_String.end(); iter1++)
    packed += DELIM + iter1->first + DELIM + iter1->second;
  packed += DELIM;
  
  packed += "_INT1D_RES_";
  packed += DELIM + to_string(_Int1D.size());
  for(; iter2!=_Int1D.end(); iter2++) {
    packed += DELIM + iter2->first + DELIM + to_string(iter2->second.size());
	{
		long_vec=iter2->second;
    //for(iter3=(std::vector<long> (iter2->second)).begin(); iter3!=(std::vector<long> (iter2->second)).end(); iter3++)
	//{
		for (ii=0; ii<long_vec.size(); ii++)	
		{
			//vyang= cons[coni];		
		    packed += DELIM + to_string((int)long_vec[ii]);
		}
	//}
	}
  }
  packed += DELIM;

  packed += "_DOUBLE1D_RES_";
  packed += DELIM + to_string(_Double1D.size());
  for(; iter4!=_Double1D.end(); iter4++) {
    packed += DELIM + iter4->first + DELIM + to_string(iter4->second.size());
	{
		double_vec=iter4->second;
    //for(iter5=(std::vector<double> (iter4->second)).begin(); iter5!=(std::vector<double> (iter4->second)).end(); iter5++)
		for (ii=0; ii<double_vec.size(); ii++)
			packed += DELIM + to_string(double_vec[ii]);
	}
  }
  packed += DELIM;

  packed += "_STRING1D_RES_";
  packed += DELIM + to_string(_String1D.size());
  for(; iter6!=_String1D.end(); iter6++) {
    packed += DELIM + iter6->first + DELIM + to_string(iter6->second.size());
	string_vec=iter6->second;
    //for(iter7=(std::vector<std::string> (iter6->second)).begin(); iter7!=(std::vector<std::string> (iter6->second)).end(); iter7++)
    //  packed += DELIM + *iter7;
	for (ii=0; ii<string_vec.size(); ii++)
		packed += DELIM + string_vec[ii];
  }
  packed += DELIM;
  
  int s = packed.size();
  std::string size = to_string(s);
  s=size.size();
  size.append(24-s, ' ');

  std::string ids;
  
  pack_ids(ids);
 
  packed.insert(0, size);
  packed.insert(0, ids);

  return true;
}

bool Interface::unpack (std::string packed)
{
  int i, j, k;
  int pos1, pos2;
  int t_size; // the tempory size
  int length;
  vector<string> tokens;

  string vname;

  long ivalue;
  double dvalue;
  string svalue;
  int array_size;
  vector<long> ivalue1d;
  vector<double> dvalue1d;
  vector<string> svalue1d;
 
  tokens=split_string(packed, DELIM); //all the items in the interface are delimed with ' ';
  length = tokens.size();
  _Int.clear();
  _Double.clear();
  _String.clear();
  _Int1D.clear();
  _Double1D.clear();
  _String1D.clear();

  for (i=0; i<length; i++)
    {
      if (tokens[i]=="_INT_RES_") //All integer variables
	{
	  i=i+1; //Move to the next position
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1; //index move to the variable name
	      vname=tokens[i];
	      i=i+1;
	      ivalue=atoi(tokens[i].c_str());
	      _Int.insert(pair<string, int>(vname, ivalue));
	    }
	}
      else if (tokens[i]=="_DOUBLE_RES_")
	{
	  i=i+1; //Move to the next position
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1; //index move to the variable name
	      vname=tokens[i];
	      i=i+1;
	      dvalue=atof(tokens[i].c_str());
	      _Double.insert(pair<string, double>(vname, dvalue));
	    }
	}
      else if (tokens[i]=="_STRING_RES_")
	{
	  i=i+1; //Move to the next position
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1; //index move to the variable name
	      vname=tokens[i];
	      i=i+1;
	      svalue=tokens[i].c_str();
	      _String.insert(pair<string, string>(vname, svalue));
	    }
	}
      else if (tokens[i]=="_INT1D_RES_")
	{
	  i = i+1;
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1;
	      vname = tokens[i];
	      i=i+1;
	      array_size = atoi(tokens[i].c_str());
	      ivalue1d.clear();
	      for (k=0; k<array_size; k++)
		{
		  i=i+1;
		  ivalue1d.push_back(atoi(tokens[i].c_str()));
		}
	      _Int1D[vname]=ivalue1d;
	    }
	}
      else if (tokens[i]=="_DOUBLE1D_RES_")
	{
	  i = i+1;
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1;
	      vname = tokens[i];
	      i=i+1;
	      array_size = atoi(tokens[i].c_str());
	      dvalue1d.clear();
	      for (k=0; k<array_size; k++)
		{
		  i=i+1;
		  dvalue1d.push_back(atof(tokens[i].c_str()));
		}
	      _Double1D[vname]= dvalue1d;
	    }
	}
      else if (tokens[i]=="_STRING1D_RES_")
	{
	  i = i+1;
	  t_size = atoi(tokens[i].c_str());
	  for (j=0; j<t_size; j++)
	    {
	      i=i+1;
	      vname = tokens[i];
	      i=i+1;
	      array_size = atoi(tokens[i].c_str());
	      svalue1d.clear();
	      for (k=0; k<array_size; k++)
		{
		  i=i+1;
		  svalue1d.push_back(tokens[i]);
		}
	      _String1D[vname]=svalue1d;
	    }
	}
    }
      
  
  //  int int_pos  = packed.find("_INT_RES_",      0);// + 9;  
  //  int dbl_pos  = packed.find("_DOUBLE_RES_",   0);// + 12;
  //  int str_pos  = packed.find("_STRING_RES_",   0);// + 12;
  //  int int1_pos = packed.find("_INT1D_RES_",    0);// + 11;
  //  int dbl1_pos = packed.find("_DOUBLE1D_RES_", 0);// + 14;
  //  int str1_pos = packed.find("_STRING1D_RES_", 0);// + 14;

  //  cout << packed.substr(int_pos, dbl_pos) << endl;
  return true;
}


*/
