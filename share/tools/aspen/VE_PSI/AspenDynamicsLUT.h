#ifndef ASPENDYNAMICSLUT_H
#define ASPENDYNAMICSLUT_H

#include <map>
#include <string>
#include <utility>
#include <vector>

std::map< std::pair< std::string, std::string >, std::vector< double > > GetAspenDynamicsLUT();

#endif