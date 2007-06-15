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
#pragma warning(disable : 4786)

#include "string_ops.h"

bool string_to_int (const string &str, int &result)
{
  return sscanf(str.c_str(), "%d", &result) == 1;
}

bool string_to_double (const string &str, double &result)
{
  return sscanf(str.c_str(), "%lf", &result) == 1;
}

std::string to_string (int val)
{
  char s[50];
  sprintf(s, "%d", val);
  return std::string(s);
}

std::string to_string (unsigned int val)
{
  char s[50];
  sprintf(s, "%u", val);
  return std::string(s);
}

std::string to_string (double val)
{
  char s[50];
  sprintf(s, "%g", val);
  return std::string(s);
}

std::vector<std::string> split_string (const std::string& str, char sep)
{
  std::vector<std::string> result;
  std::string s(str);
  while(s != ""){
    unsigned int first = s.find(sep);
    if(first < s.size()){
      result.push_back(s.substr(0, first));
      s = s.substr(first+1);
    } else {
      result.push_back(s);
      break;
    }
  }
  return result;
}
