#pragma warning(disable : 4786)

#include "string_ops.h"
#include <stdio.h>

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
