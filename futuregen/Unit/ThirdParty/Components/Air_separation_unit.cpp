#include "Air_separation_unit.h"


namespace Vision21 {

//**********************************************************************

Air_separation_unit::Air_separation_unit()
{
  energy_requirement = 0;
  O2_purity  = 95;
  T_O2stream = 298.15;
  P_O2stream = 506625;
  T_N2stream = 298.15;
  P_N2stream = 101325;
    
}

//**********************************************************************

bool Air_separation_unit::Calculate(Gas &AirIn, Gas &O2stream, Gas&N2stream)
{
  //cout << "\n*************************************************************";
  //cout << "\n******************* Air Separation Unit *********************";
  //cout << "\n*************************************************************";
  
  AirIn.addSpecie("O2");
  AirIn.addSpecie("N2");
  AirIn.addSpecie("AR");

  O2stream.copy(AirIn);
  N2stream.copy(AirIn);


  //cout << "\nInlet Gas:";
  //AirIn.gas_composite.Property_Output();

  if(O2_purity > 100 || O2_purity < 0) {
    cout << "\nError, oxygen product must be between 0 and 100%";
    return false;
  }
  if(P_N2stream < 0) {
    cout << "\nError, Nitrogen product must have positive pressure";
    return false;
  }
  if(P_O2stream < 0) {
    cout << "\nError Oxygen product must have positive pressure";
    return false;
  }
  if(T_O2stream < 0 || T_N2stream < 0) {
    cout << "\nError, Temperature specified is less than zero";
    return false;
  }
  

  /////// Calculate O2 stream  
  double AR_split = 0.75;   //fraction of inlet AR to o2 product.
  int i;

  O2stream.gas_composite.T = T_O2stream;
  O2stream.gas_composite.P = P_O2stream; // * 100000;   // atm to Pa
  for(i=0; i<O2stream.gas_composite.comp_specie.size(); i++)
    O2stream.gas_composite.comp_specie[i]=0;

  O2stream.gas_composite.comp_specie[O2stream.specie["O2"]] = O2_purity/100.0;
  double d1, d2, d3;         // moles o2, ar, n2 in o2 product
  d1 = AirIn.gas_composite.moles("O2");     
  d2 = AirIn.gas_composite.comp_specie[AirIn.specie["AR"]] /
    AirIn.gas_composite.comp_specie[AirIn.specie["O2"]] * d1 * AR_split;  
  d3 = (d1 - d1*O2_purity/100.0 - d2*O2_purity/100.0)/(O2_purity/100.0);
  O2stream.gas_composite.comp_specie[O2stream.specie["AR"]] = d2/(d1+d2+d3);
  O2stream.gas_composite.comp_specie[O2stream.specie["N2"]] = d3/(d1+d2+d3);
  O2stream.gas_composite.M = (d1*O2stream.thermo_database->mweight("O2") +
    d2*O2stream.thermo_database->mweight("AR") +
    d3*O2stream.thermo_database->mweight("N2"))/1000.0;

  /////// Calculate N2 stream
  
  N2stream.gas_composite.T = T_N2stream;
  N2stream.gas_composite.P = P_N2stream; // * 100000;     // bar to Pa
  for(i=0; i<N2stream.gas_composite.comp_specie.size(); i++)
    N2stream.gas_composite.comp_specie[i]=0;

  d3 = AirIn.gas_composite.moles("N2") - d3;
  d2 = AirIn.gas_composite.moles("AR") - d2;
  N2stream.gas_composite.comp_specie[N2stream.specie["N2"]] = d3/(d3+d2);
  N2stream.gas_composite.comp_specie[N2stream.specie["AR"]] = d2/(d3+d2); 
  N2stream.gas_composite.M = d3*N2stream.thermo_database->mweight("N2")/1000.0 +
    d2*N2stream.thermo_database->mweight("AR")/1000.0;

  /////// Compute energy requirements

  double enth1, enth2, enth3, enth4;
  enth1 = AirIn.gas_composite.enthalpy()*AirIn.gas_composite.moles() /1000.0;
  enth2 = O2stream.gas_composite.enthalpy()*O2stream.gas_composite.moles()/1000.0;
  enth3 = N2stream.gas_composite.enthalpy()*N2stream.gas_composite.moles()/1000.0;
  //cout << "\n\nEnthalpy1 = " << enth1;
  // get enthalpy of water, CO2, HC's etc from molecular sieve outlet:
  // assume T = 298.15 K
  enth4 = 0;
  double mains[3];
  mains[0] = AirIn.specie["N2"];
  mains[1] = AirIn.specie["O2"];
  mains[2] = AirIn.specie["AR"];
    
  map<const string, int>::iterator iter;

  for(iter=AirIn.specie.begin(); iter != AirIn.specie.end(); iter++)
    if(iter->second != mains[0] && iter->second != mains[1] && iter->second != mains[2]) 
      enth4 += AirIn.thermo_database->enthalpy_i(iter->second, 298.15) * 
	AirIn.gas_composite.moles(iter->first) / 1000.0;

  energy_requirement = enth2 + enth3 + enth4 - enth1;  
  energy_requirement /= 1000.0;                   // kW  (provided M [=] kg/s)

  /////// check for reasonable exit streams.
  
  Gas *asu_stream[3];
  asu_stream[0] = &O2stream;
  asu_stream[1] = &N2stream;
  asu_stream[2] = &AirIn;

  for(i=0; i<3; i++) {
    double TT = asu_stream[i]->gas_composite.T;
    double PP = asu_stream[i]->gas_composite.P;
    if(asu_stream[i]->gas_composite.comp_specie[AirIn.specie["O2"]]*PP > Psat(*asu_stream[i],"O2") ||
       asu_stream[i]->gas_composite.comp_specie[AirIn.specie["N2"]]*PP > Psat(*asu_stream[i],"N2") ||
       asu_stream[i]->gas_composite.comp_specie[AirIn.specie["AR"]]*PP > Psat(*asu_stream[i],"AR")) {
      cout << "\nError: specification of T, P, and composition of ";
      switch (i) {
      case 0:
	cout << "O2 product stream \n";
	break;
      case 1:
	cout << "N2 product stream \n";
	break;
      case 2:
	cout << "Inlet stream \n";
	break;
      }
      cout << "   contains liquid.  Decrease pressure and/or increase temperature";
      return false;
    }

  }
	
  //cout << "\nO2 product:";
  //O2stream.gas_composite.Property_Output();
  //cout << "\nN2 product:";
  //N2stream.gas_composite.Property_Output();
  //cout << "\nUnit Energy Requirements (kW) = " << energy_requirement;

  return true;
}

//**********************************************************************

double Air_separation_unit::Psat(Gas &strm, string species) 
{
  double T = strm.gas_composite.T;
  double P = strm.gas_composite.P;
  
  // Antoine constants A, B, C for log10(P) = A-(B/(T+C)) P in bar, T in K
  
  double Ant[4][3] = {{3.95230, 340.024, -4.144},
		      {3.73620, 264.651, -6.788},
		      {3.73479, 302.683, -6.083},
		      {4.46903, 481.012, 22.156}};  
  
  double Trange[4][2] = {{54.36, 154.33},    // O2
			 {63.14, 126.00},    // N2
			 {90.94, 105.00},    // AR really: {90.94, 101.48},    // AR
			 {105.0, 150.31}};   // AR really: {114.4, 150.31}};   // AR
  int i = 0;
  if(species=="O2")      i = 0;
  else if(species=="N2") i = 1;
  else if(species=="AR")
    if(T < 105)          i = 2;
    else                 i = 3;

  double psatmax = pow(10.0,Ant[i][0]-Ant[i][1]/(Trange[i][1] + Ant[i][2]))*100000; // Pa

  if(T < Trange[i][0])
    cout << "\nWarning, Temperature out of range of Psat correlation"
	 << "\n   Temperature is too low, (unrealistic)";
  if(T > Trange[i][1] && strm.gas_composite.comp_specie[strm.specie[species]]*P > psatmax)
    cout << "\nWarning, Temperature out of range of Psat correlation"
	 << "\n   Temperature is too high.";
    

  return pow(10.0,Ant[i][0]-Ant[i][1]/(T + Ant[i][2]))*100000; // Pa
}
   
} // end namespace Vision21
