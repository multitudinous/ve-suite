#include "VE_Conductor/Framework/StringParse.h"

int get_tokens(char* s, std::vector<wxString> &toks, const char* delim)
{
  char* token;
  int i=0;
  token = strtok(s, delim);
  
  toks.clear();
  while( token )
    {
      i++;
      toks.push_back(wxString(token));
      token = strtok(NULL, delim);
    }
  
  return i; 
}
