#ifndef STRINGPARSE_H
#define STRINGPARSE_H

#include <wx/wx.h>
#include <vector>

using namespace std;
//This is the string parsing utilities

int get_tokens(char* s, vector<wxString> &toks, const char* delim);

#endif
