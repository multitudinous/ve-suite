#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include "VE_Conductor/Framework/string_ops.h"
//#include <cstdio>
#include <sstream>

bool string_to_int (const std::string &str, int &result)
{
  return sscanf(str.c_str(), "%d", &result) == 1;
}

bool string_to_double (const std::string &str, double &result)
{
  return sscanf(str.c_str(), "%lf", &result) == 1;
}

std::string to_string (int val)
{
  //char* s;
  //sprintf(s, "%d", val);
  std::ostringstream dirStringStream;
  dirStringStream << val;
  return dirStringStream.str();
  //std::string dirString = dirStringStream.str();
  //s = (char*)dirString.c_str();
  //return std::string(s);
}

std::string to_string (unsigned int val)
{
  char* s;
  //sprintf(s, "%u", val);
  std::ostringstream dirStringStream;
  dirStringStream << val;
  std::string dirString = dirStringStream.str();
  s = (char*)dirString.c_str();
  return std::string(s);
}

std::string to_string (double val)
{
  char* s;
  std::ostringstream dirStringStream;
  dirStringStream << val;
  std::string dirString = dirStringStream.str();
  s = (char*)dirString.c_str();
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
