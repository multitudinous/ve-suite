#ifndef CFD_VE_ENUM_H
#define CFD_VE_ENUM_H

#include <string>

enum cfdIDEnum
{
   UPDATE_INTERACTIVE_DISPLAY,
   UPDATE_INTERACTIVE_DESIGN,
   UPDATE_INTERACTIVE_INITIAL_SEARCH,
   UPDATE_INTERACTIVE_GA  

};

std::string IDEnumToString(int value);
#endif
