#ifndef STRING_OPS_H
#define STRING_OPS_H

#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"

#ifdef _WIN32
#pragma warning(disable : 4786)
#pragma warning(disable : 4101)
#pragma warning(disable : 4503)
#endif

VE_CONDUCTOR_EXPORTS bool string_to_int    (const std::string &str, int &result);
VE_CONDUCTOR_EXPORTS bool string_to_double (const std::string &str, double &result);

VE_CONDUCTOR_EXPORTS std::string to_string (int val);
VE_CONDUCTOR_EXPORTS std::string to_string (unsigned int val);
VE_CONDUCTOR_EXPORTS std::string to_string (double val);

VE_CONDUCTOR_EXPORTS std::vector<std::string> split_string(const std::string& str, char sep);
#endif
