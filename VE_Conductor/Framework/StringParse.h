#ifndef STRINGPARSE_H
#define STRINGPARSE_H

#include <wx/wx.h>
#include <vector>

//This is the string parsing utilities
int get_tokens(char* s, std::vector< wxString > &toks, const char* delim);

#endif
