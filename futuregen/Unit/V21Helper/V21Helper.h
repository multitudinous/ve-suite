#ifndef V21HELPER_H
#define V21HELPER_H

#include <Datatypes/Gas.h>
#include <Therm/thermo.h>

//#include <Datatypes/Water.h>
//#include <Steam67/Steam67.h>

#include <interface.h>
#include <SummaryValues/summary_values.h>
class Water;
class V21Helper {

public:

  V21Helper  (const char* thermo_path);
  ~V21Helper ();

  int IntToGas (Interface *it, Gas &gs);
  int GasToInt (Gas *gs, Interface &it);
  
  int IntToWat (Interface *it, Water &wt);
  int WatToInt (Water *wt, Interface &it);

  int IntToSum (Interface *it, summary_values &sv);
  int SumToInt (summary_values* sv, Interface &it);

  thermo  *thermo_database;
  //steam67 *steam67_database;
 
private:

};


#endif
