
#include "Water.h"
#include <iostream>

using namespace std;


Water::Water() { 
}

Water::Water(const Water& p)
{
  if(this==&p)  return;

  T = p.T;
  P = p.P;
  H = p.H;
  Q = p.Q;
  M = p.M;
}

void Water::print() {
    cout << "Temperature (K): " << T << endl;
    cout << "Pressure (Pa): " << P << endl;
    cout << "Enthalpy (J/kg): " << H << endl;
    cout << "Quality: " << Q << endl;
    cout << "Flowrate (kg/s): " << M << endl;
}
