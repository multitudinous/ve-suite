
#ifndef STRING_OPS_H
#define STRING_OPS_H

#include <string>
#include <vector>

#ifdef _WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

using namespace std;

bool string_to_int    (const std::string &str, int &result);
bool string_to_double (const std::string &str, double &result);

std::string to_string (int val);
std::string to_string (unsigned int val);
std::string to_string (double val);

std::vector<std::string> split_string(const std::string& str, char sep);


#endif
