#include "cfdVeEnum.h"

std::string IDEnumToString(int value)
{
   switch(value)
   {
      case (UPDATE_INTERACTIVE_DISPLAY):
         return "UPDATE_INTERACTIVE_DISPLAY";
      case (UPDATE_INTERACTIVE_DESIGN):
         return "UPDATE_INTERACTIVE_DESIGN";
      case (UPDATE_INTERACTIVE_INITIAL_SEARCH):
         return "UPDATE_INTERACTIVE_INITIAL_SEARCH";
      case (UPDATE_INTERACTIVE_GA): 
         return "UPDATE_INTERACTIVE_GA";
      default:
         return "Unkown value";
   }
}
