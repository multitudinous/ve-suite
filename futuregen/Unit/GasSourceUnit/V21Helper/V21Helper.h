#ifndef V21HELPER_H
#define V21HELPER_H

#include <Gas/Gas.h>
#include <Therm/thermo.h>
#include <interface.h>
#include <SummaryValues/summary_values.h>

class V21Helper {

public:

  V21Helper  (const char* thermo_path);
  ~V21Helper ();

  int IntToGas (Interface *it, Gas &gs);
  int GasToInt (Gas *gs, Interface &it);
  
  int IntToSum (Interface *it, summary_values &sv);
  int SumToInt (summary_values* sv, Interface &it);

private:
  thermo *thermo_database;
};


#endif
