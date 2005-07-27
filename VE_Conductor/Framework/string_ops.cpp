#ifdef WIN32
#pragma warning(disable : 4786)
#endif

#include "VE_Conductor/Framework/string_ops.h"

#include <sstream>

bool string_to_int (const std::string &str, int &result)
{
   if ( str.size() > 0 )
   {
      std::istringstream inputStream( str );
      inputStream >> result;
   }
   else
      return false;

   return true;
}

bool string_to_double (const std::string &str, double &result)
{
   if ( str.size() > 0 )
   {
      std::istringstream inputStream( str );
      inputStream >> result;
   }
   else
      return false;

   return true;
}

std::string to_string (int val)
{
  std::ostringstream dirStringStream;
  dirStringStream << val;
  return std::string( dirStringStream.str() );
}

std::string to_string (unsigned int val)
{
  std::ostringstream dirStringStream;
  dirStringStream << val;
  return std::string( dirStringStream.str() );
}

std::string to_string (double val)
{
  std::ostringstream dirStringStream;
  dirStringStream << val;
  return std::string( dirStringStream.str() );
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
