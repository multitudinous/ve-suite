#pragma warning(disable: 4786)
#include "surface_chem.h"

using namespace std;

///////////////////////////////////////////////////////////////////////////////

surface_chem::surface_chem(string mechFileName) {
	fSurfChemError = false;
	surf_parser surfReader;
	bool fSuccess = surfReader.parse_surf_mech(mechFileName);
	if(!fSuccess) {
		cerr << "\nError Parsing Surface Mechanism";
		fSurfChemError = true;
	}

	rxnsData	 = surfReader.rxnsData;
	spNames		 = surfReader.spNames;
	speciesMap	 = surfReader.speciesMap;
	tempRanges	 = surfReader.thermoReader.tempRanges;
	thermoCoeffs = surfReader.thermoReader.coeffs;
	s_lj         = surfReader.s_lj;
	ek_lj        = surfReader.ek_lj;
	delta_lj     = surfReader.delta_lj;

    // calculate spMWs 
    
    int i, j;
    vector<double> weightList(surfReader.thermoReader.elNames.size(),0.0);
    for(i=0; i<surfReader.thermoReader.elNames.size(); i++) 
      for(j=0; j<CHART_LEN; j++) 
	if(surfReader.thermoReader.elNames[i] == periodic_chart_name[j]) {
	  weightList[i] = periodic_chart_wgt[j]; break; 
	}
    for(i=0; i < spNames.size(); i++) {
      double weight = 0.0;
      for(j=0; j < surfReader.thermoReader.atFormula[i].size(); j++)  // not j<weightList.size(): tfile.atFormula[].size() varies
		weight += weightList[j] * surfReader.thermoReader.atFormula[i][j];
	  spMWs.push_back(weight);
    }

	nGasSp  = surfReader.nGasSp;       // 53;
	nSurfSp = surfReader.nSurfSp;      // 7;

	molf.resize(spNames.size(), 0.0);
	molf[speciesMap.find("PT(S)")->second] = 1.0;
	bulkMolf.resize(nGasSp, 0.0);
	pres = 101325.0;                   // Pa
	Rgas = 8.31451;                    // J/mol*K

	// initialize rxnGasSpInd

	for(j=0; j<nGasSp; j++){
		for(i=0; i<rxnsData.size(); i++)
			if(rxnsData[i].rxnReacCoeffs[j] != 0 || rxnsData[i].rxnProdCoeffs[j] !=0) {
				rxnGasSpInd.push_back(j);
				break;
			}
	}

	eff_factor = 1.0;
	surfSp2reksSp = vector<int> (nGasSp, -1);   // initialize in cat_comb.calculate once reks is set

}

//////////////////////////////////////////////////////////////////////////////////////////////////////
double surface_chem::viscosity_mix(const double& temp_gas, const double& temp, 
								   const vector<double>& composition) {
// composition is gas composition only
// units [=] kg/m*s
// make sure any condensed species aren't included in composition arrays and LJ parameter arrays


   int nspc = nGasSp; 
   int ii;
   
   // transport property (check out trnprp)
   
   double tfilm = (temp_gas+temp)/2.0;
   std::vector<double> emu(nspc);
   
   for(ii=0;ii<nspc;ii++){
//      if(tmid[ii]==1000.0){
         double tek = tfilm/ek_lj[ii];
         double colmu = sigmam(tek,delta_lj[ii]);
         emu[ii] = 2.6693e-6*sqrt(spMWs[ii]*tfilm)/(pow(s_lj[ii],2.0)*colmu);
  //    }
   }
   
   // loop for mixture viscosity
   double phiik;
   double sumxk  = 0.0;
   double sumxfi = 0.0;
   double sumxmu = 0.0;
   
   for(ii=0;ii<nspc;ii++){
      // cdol if(tmid[ii]==1000.0) {
         sumxfi = 0.0;
         for(int jj=0;jj<nspc;jj++){
        // cdol    if (tmid[jj]==1000.0) {
               phiik = 0.3535534*sqrt(1.0/(1.0+spMWs[ii]/spMWs[jj]))*pow((1.0+sqrt(emu[ii]/emu[jj])*
                  pow((spMWs[jj]/spMWs[ii]),(0.25))),2.0);
        //   }
            sumxfi = sumxfi+composition[jj]*phiik;
         }
         if (composition[ii]>=(1.0e-30)) {
            sumxmu = sumxmu+composition[ii]*emu[ii]/sumxfi;
         }
      //}
   }
   
   // mixture viscosity
   double emug = sumxmu;
   return(emug);
}

/////////////////////////////////////////////////////////////////////////////////////////////////////
double surface_chem::thermal_cond_mix(const int& ieuck, double& temp_gas, double& temp,
                              const vector<double>& composition)
{  // units [=] W/m*K
   int ii;
   int nspc = nGasSp;       
   // transport property (check out trnprp)
   
   double tfilm = (temp_gas+temp)/2.0;
   std::vector<double> emu(nspc);
   std::vector<double> rk(nspc);
   
   for(ii=0;ii<nspc;ii++){
     // if(tmid[ii]==1000.0){
         double tek = tfilm/ek_lj[ii];
         double colmu = sigmam(tek,delta_lj[ii]);
         emu[ii] = 2.6693e-6*sqrt(spMWs[ii]*tfilm)/(pow(s_lj[ii],2.0)*colmu);

         if(ieuck==1){
            rk[ii] = (cp_i(ii,tfilm)/spMWs[ii]*1000.+1.25*Rgas/spMWs[ii]*1000.)*emu[ii];
         } else {
            rk[ii] = 1.25*(cp_i(ii,tfilm)/spMWs[ii]*1000.+Rgas/(2.0*spMWs[ii]/1000.))*emu[ii];
         }
      //}
   }
   
   // loop for mixture thermal conductivity
   double phiik;
   double sumxk  = 0.0;
   double sumxfi = 0.0;
   
   for(ii=0;ii<nspc;ii++){
      // cdol if(tmid[ii]==1000.0) {
         sumxfi = 0.0;
         for(int jj=0;jj<nspc;jj++){
            // cdol if (tmid[jj]==1000.0) {
               phiik = 0.3535534*sqrt(1.0/(1.0+spMWs[ii]/spMWs[jj]))*pow((1.0+sqrt(emu[ii]/emu[jj])*
                  pow((spMWs[jj]/spMWs[ii]),(0.25))),2.0);
            // }
            sumxfi = sumxfi+composition[jj]*phiik;
         }
         if (composition[ii]>=(1.0e-30)) {
            sumxk = sumxk+composition[ii]*rk[ii]/sumxfi;
         }
      //}
   }
   
   // mixture thermal conductivity
   double rkg = sumxk;
   return(rkg);
}

/////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::get_diffus(const double& temp_gas, const double& temp,
                                     const vector<double>& composition, const double& pres)
{  // units [=] m2/s
   int nspc = nGasSp;          
   // transport property (check out trnprp)
   
   double tfilm = (temp_gas+temp)/2.0;
   
   // calculate diffusivities (diffusivity of THIS species through THIS mixture - mass transfer)
   
   double tek;
   double coldif;
   double dift;
   double sumxd = 0.0;
   std::vector<double> difm(nspc);
   
   // loop for diffusivities
   for(int ii=0;ii<nspc;ii++){
      sumxd = 0.0;
      for(int jj=0;jj<nspc;jj++){
         //if (tmid[jj]==1000.0&&(ii!=jj)) {
            tek = tfilm/sqrt(ek_lj[ii]*ek_lj[jj]);
            coldif = sigmad(tek);
            dift = 1.883444e-2*pow(tfilm,(1.5))*sqrt(1.0/spMWs[ii]+1.0/spMWs[jj])/(pres*pow(((s_lj[ii]+s_lj[jj])/2.0),2.0)*coldif);
            sumxd = sumxd+composition[jj]/dift;
         //}
      }
      
      // species diffusivities in mixture 
      difm[ii] = (1.0-composition[ii])/sumxd;
   }
   return(difm);
}

//////////////////////////////////////////////////////////////////////////////////////////////////////
double surface_chem::sigmam(const double& tek, const double& del)
{
   
   double tek_mod,del_mod,a,b,c;
   
   static double delta[] = {0,0.25,0.5,0.75,1.0,1.5,2.0,2.5};
   
   static double am[] = {0.47395,0.47808,0.50119,0.54668,0.60910,
      0.45351,0.89016,1.01037,0.43969,0.44882,0.48192,
      0.53732,0.60815,0.76710,0.91660,1.04383,0.16152,
      0.16285,0.17807,0.20258,0.23287,0.31112,0.41063,0.52600};
   
   static double bm[] = {-0.53203,-0.51551,-0.49752,-0.49670,-0.51945,
      -0.57471,-0.60747,-0.62594,-0.44832,-0.45212,-0.47283,
      -0.50951,-0.55388,-0.64309,-0.70603,-0.73772,-0.15835,
      -0.15840,-0.16626,-0.17878,-0.16314,-0.23119,-0.27807,-0.33159};
   
   static double cm[] = {-0.05410,-0.04057,-0.01404,0.01,0.01832,
      0.01467,0.00901,0.00461,0.07758,0.07756,0.08067,0.08685,
      0.09367,0.10489,0.10718,0.10078,0.00186,0.00178,0.00281,
      0.00442,0.00627,0.01075,0.01625,0.02242};
   
   tek_mod = tek;
   del_mod = del;
   
   if (del_mod>2.5) del_mod = 2.5;
   if (tek_mod<0.1) tek_mod = 0.1;
   if (tek_mod>100.0) tek_mod = 100.0;
   
   int m = 1;
   
   if ((tek_mod<9.0)&&(1.4<=tek_mod)) m = 2;
   if (tek_mod>=9.0) m = 3;
   
   double alogt = log(tek_mod);
   
   find(del_mod,a,delta,(am+8*(m-1)),8);
   find(del_mod,b,delta,(bm+8*(m-1)),8);
   find(del_mod,c,delta,(cm+8*(m-1)),8);
   
   double sigmam = exp(a+alogt*(b+c*alogt));
   return(sigmam);
}

//////////////////////////////////////////////////////////////////////
double surface_chem::sigmad(const double& tek)
{
   
   // this works as the banff/glacier sigmad works - tested by DAS
   static double abc[] = {0.36934,-0.48595,0.02574,0.343,-0.44203,0.07549,0.09454,-0.17612,0.00272};
   
   double tek_mod = tek;
   if(tek_mod<0.3) tek_mod = 0.3f;
   if(tek_mod>100.0) tek_mod = 100.0f;
   
   int m = 1;
   
   if((1.55<=tek_mod)&&(tek_mod<7.0)) m = 2;
   if(tek_mod>7.0) m = 3;
   
   double alogt = log(tek_mod);
   double sigmad = exp(*(abc+3*(m-1))+alogt*(*(abc+3*(m-1)+1)+alogt*(*(abc+3*(m-1)+2))));
   
   return(sigmad);
}

///////////////////////////////////////////////////////////////////////
void surface_chem::find(const double& arg, double& ans, double* x, double* y, const int& npts)
{
   
   // check ends
   if(arg<=x[0]){
      ans = y[0];
      return;
   }
   if(arg>=x[npts-1]){
      ans = y[npts-1];
      return;
   }
   
  int i=0;
  while(arg>x[++i]) ;
  ans = y[i] - (y[i] - y[i-1]) * (x[i] - arg) / (x[i] - x[i-1]);
}

///////////////////////////////////////////////////////////////////////////////
double surface_chem::kHeat() {
	// units W/m2*K
	// Turbulent
	// Gnielinski in Fluid Mechanics and Heat Transfer 1983, R.C. Armstrong et. al 
	// Hemisphere Pub. ISBN 0891161252; Also, Perry's 7th, 5-17
	// Laminar
	// Hausen (Perry's 7th 5-15)
	// use film properties

	vector<double> filmComp = getFilmMolf();  
	double MMW = getMeanMW(filmComp);            // g/mol
	double Tfilm = 0.5*(surfTemp+bulkTemp);      // K
	double rho = MMW/1000.0*pres/Rgas/Tfilm;     // kg/m3
	double vel = mdot/rho/Acs;                   // m/s
	double mu  = viscosity_mix(bulkTemp, surfTemp, filmComp);     // kg/m*s
	double ktherm = thermal_cond_mix(1, bulkTemp, surfTemp, filmComp);          // W/m*K
	double Re = rho*Dh*vel/mu;
	double Pr = cp_mix(Tfilm, filmComp) * mu / ktherm / MMW * 1000.;
	double Nu = 0.0;
	double xpos = (position < Dh) ? Dh : position;

	if(Re > 2500.0) {                            // turbulent
		double fFanning = 0.25*pow(1.82*log10(Re)-1.64, -2.0);
		Nu = fFanning/2.0 * (Re-1000.0) * Pr / 
			(1.0 + 12.7*sqrt(fFanning/2.0)*(pow(Pr,0.666666667)-1.0)) *
			(1.0+pow(Dh/xpos, 0.66666667)) * pow(bulkTemp/surfTemp, 0.15);
	}
	else  {                                      // laminar
		double Gz = Re * Pr * Dh/xpos;
		Nu = 3.66 + 0.19 * pow(Gz, 0.8) /         
			(1.0 + 0.117*pow(Gz, 0.467));
	}
	return Nu * ktherm / Dh;                     // W/m2*K
}

///////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::kMass() {
	// UNITS m/s
	vector<double> filmComp = getFilmMolf(); 
	double MMW = getMeanMW(filmComp);            // g/mol
	double Tfilm = 0.5*(surfTemp+bulkTemp);      // K
	double rho = MMW/1000.0*pres/Rgas/Tfilm;     // kg/m3
	double vel = mdot/rho/Acs;                   // m/s
	double mu  = viscosity_mix(bulkTemp, surfTemp, filmComp);     // kg/m*s
	double Re = rho*Dh*vel/mu;
	vector<double> diffusivities = get_diffus(bulkTemp, surfTemp, filmComp, pres);  // m2/s
	vector<double> Sc(nGasSp, 0.0);              // Schmidt number and later kmass
	int i,j;
	for(i=0; i < rxnGasSpInd.size(); i++)
		Sc[rxnGasSpInd[i]] = mu/rho/diffusivities[rxnGasSpInd[i]];
	
	if(Re > 2500.0) {                            // turbulent; see Perry's 7th pg 5-64
		for(i=0; i < rxnGasSpInd.size(); i++) { 
		    j = rxnGasSpInd[i];	
			Sc[j] = 0.0097*pow(Re, 9.0/10.0)*sqrt(Sc[j])*
			        (1.1+0.44*pow(Sc[j], -0.3333333)-0.7*pow(Sc[j], -1.0/6.0)) * 
					diffusivities[j] / Dh;        // Sh to km
		}
	}
	else {                                        // laminar; see Perry's 7th pg 5-62 A
		double xpos = (position < Dh) ? Dh : position;         // avoid infinite or huge values
		for(i=0; i < rxnGasSpInd.size(); i++) {
			j = rxnGasSpInd[i];
			Sc[j] = 3.66 + 0.0668*Dh/xpos*Re*Sc[j] / 
				    (1.0+0.04*pow(Dh/xpos*Re*Sc[j], 0.66666667)) * 
					diffusivities[j] / Dh;        // Sh to km
		}
	}

	return Sc;
}

////////////////////////////////////////////////////////////////////////////////

double surface_chem::dHRxnI(const int i, double& Temp) {
	// J/mol
  int j;
  double H = 0;
  for(j=0; j<spNames.size(); j++)
    H += (rxnsData[i].rxnProdCoeffs[j] - rxnsData[i].rxnReacCoeffs[j]) *
      enthalpySp(j, Temp);
  return H;
}
////////////////////////////////////////////////////////////////////////////////

double surface_chem::dSRxnI(const int i, double& Temp) {
	// J/mol*K
  int j;
  double S = 0;
  for(j=0; j<spNames.size(); j++)
    S += (rxnsData[i].rxnProdCoeffs[j] - rxnsData[i].rxnReacCoeffs[j]) *
      entropySp(j, Temp);
  return S;
}
////////////////////////////////////////////////////////////////////////////////
double surface_chem::enthalpySp(const int i, double& Temp) {
  // J/mol
  int ind = (Temp >= tempRanges[i][2]) ? 0 : 7;
  if(Temp < tempRanges[i][0] || Temp > tempRanges[i][1])
    cerr << "\nT = " << Temp << " out of range for species " 
	 << spNames[i] << " in tchem::enthalpySp";
  double enth = thermoCoeffs[i][0+ind] + thermoCoeffs[i][1+ind]*Temp/2 + 
    thermoCoeffs[i][2+ind]*Temp*Temp/3 + thermoCoeffs[i][3+ind]*Temp*Temp*Temp/4 + 
    thermoCoeffs[i][4+ind]*Temp*Temp*Temp*Temp/5 + thermoCoeffs[i][5+ind]/Temp;
  return enth * Temp * Rgas;          // J/mol
}
////////////////////////////////////////////////////////////////////////////////
double surface_chem::entropySp(const int i, double& Temp) {
  // standard state entropy of pure i at Temp
  int ind = (Temp >= tempRanges[i][2]) ? 0 : 7;
  if(Temp < tempRanges[i][0] || Temp > tempRanges[i][1])
    cerr << "\nT = " << Temp << " out of range for species " 
	 << spNames[i] << " in tchem::entropySp";
  double ent = thermoCoeffs[i][0+ind]*log(Temp) + thermoCoeffs[i][1+ind]*Temp + 
    thermoCoeffs[i][2+ind]*Temp*Temp/2 + thermoCoeffs[i][3+ind]*Temp*Temp*Temp/3 + 
    thermoCoeffs[i][4+ind]*Temp*Temp*Temp*Temp/4 + thermoCoeffs[i][6+ind];  
  return ent * Rgas ;          // J/mol*K
}
/////////////////////////////////////////////////////////////////////////////////
double surface_chem::cp_i(int i, double& temp_gas) {
	// J/mol*K
	int ind = (temp_gas >= tempRanges[i][2]) ? 0 : 7;
	if(temp_gas < tempRanges[i][0] || temp_gas > tempRanges[i][1])
		cerr << "\nT = " << temp_gas << " out of range for species " 
		<< spNames[i] << " in surface_chem::enthalpySp";
	double cpi = thermoCoeffs[i][0+ind] + thermoCoeffs[i][1+ind]*temp_gas + 
		thermoCoeffs[i][2+ind]*temp_gas*temp_gas + thermoCoeffs[i][3+ind]*temp_gas*temp_gas*temp_gas + 
		thermoCoeffs[i][4+ind]*temp_gas*temp_gas*temp_gas*temp_gas;
	return cpi * Rgas;          // J/mol*K
}

////////////////////////////////////////////////////////////////////////////////
double surface_chem::cp_mix(double& temp_gas, vector<double>& gasComp) {
	// J/mol*K
	double sum = 0.0;
	int i;
	for(i=0; i<nGasSp; i++) 
		sum += cp_i(i, temp_gas) * gasComp[i];
	return sum;                // J/mol*K
}

////////////////////////////////////////////////////////////////////////////////
double surface_chem::KpRxnI(const int i,  double& Temp) {
  // Kp is unitless by definition
  return exp( (dSRxnI(i, surfTemp) - dHRxnI(i, Temp)/Temp)/Rgas );
}

////////////////////////////////////////////////////////////////////////////////
double surface_chem::KcRxnI(const int i, double& Temp) {
  // units are (mol/m3)^n * mol/m2 ^m
  double sum1 = 0.0, sum2 = 0.0;
  int j;
  for(j=nSurfSp; j<spNames.size(); j++)
    sum1 += (rxnsData[i].rxnProdCoeffs[j] - rxnsData[i].rxnReacCoeffs[j]);
  for(j=0; j<nSurfSp; j++)
	sum2 += (rxnsData[i].rxnProdCoeffs[j] - rxnsData[i].rxnReacCoeffs[j]);
  return KpRxnI(i, Temp)*pow(101325.0/Rgas/Temp,sum1) * pow(siteDensity,sum2);
}

///////////////////////////////////////////////////////////////////////////////
double surface_chem::kfRxnI(const int i, double& Temp) {     
	// units are mol, m, s
	// assumes all sigmas are 1.  (see surf chemkin)
	// assumes only one coverage species
	// assumes third cove parameter is J/mol (as is Ea)
	const reactionObject &rxn = rxnsData[i];
	double kf = rxn.rateParameters[0] * 
			    pow(Temp,rxn.rateParameters[1]) *
				exp(-rxn.rateParameters[2]/Temp);
	if(rxn.fCov) {   
		double siteFracCovSp = molf[speciesMap.find(rxn.covSp)->second];
		kf *= pow(10.0, rxn.covParams[0] * siteFracCovSp) * 
			  pow(siteFracCovSp,rxn.covParams[1]) *
			  exp(-rxn.covParams[2]*siteFracCovSp/Rgas/Temp);

	}
	else if(rxn.fStick) {
#if WIN32
		double gamma = min(1.0, kf);
#else
		double gamma = min(1.0, kf);
#endif
		double m = 0.0;
		int i;
		for(i=nGasSp; i<spNames.size(); i++) 
			m += rxn.rxnReacCoeffs[i];
		kf = gamma/(1.0-0.5*gamma) / pow(siteDensity, m) * 
			sqrt(Rgas*Temp/2.0/3.141592654/spMWs[rxn.iStick]*1000.0);
	}

	return kf;
}

///////////////////////////////////////////////////////////////////////////////
double surface_chem::krRxnI(const int i, double & Temp, double kf) {
  // NOT TESTED
  // Units are m-mole-sec-K
  const reactionObject &rxn = rxnsData[i];
  double kr = 0.0;
  if(!rxn.fReversible) {
    cerr << "\nError surface_chem::krRxnI() should not be called for "
	 << "\n\tirreversible reaction " << i+1;
    fSurfChemError = true;
    return kr;
  }

  if(kf < 0)                                                // forward rate is default
    kf = kfRxnI(i, Temp);                                   // not passed into func

  return kf/KcRxnI(i, Temp);  
}

////////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::rxnRateOfProgress(vector<double>& comp, double& Temp) {
  // units are mol/m2*s
  vector<double> qProg(rxnsData.size(),0.0);
  int i, j;
  double prod1, prod2, kf, kr= 1.0, spConc1;
  vector<double> spConc = conc(comp, Temp);     

  for(j=0; j<rxnsData.size(); j++) {
    prod1 = prod2 = 1.0;
    kf = kfRxnI(j, Temp);
    if(rxnsData[j].fReversible) {
      kr = krRxnI(j, kf);
	  for(i=nGasSp; i<spNames.size(); i++) { // loop over surf species      // for(i=0; i<spNames.size(); i++) {
		  if(rxnsData[j].fStick) spConc1 = spConc[i];
		  else spConc1 = comp[i];
		prod1 *= pow(spConc[i], rxnsData[j].rxnReacCoeffs[i]);
		prod2 *= pow(spConc[i], rxnsData[j].rxnProdCoeffs[i]);
      }
	  for(i=0; i<rxnGasSpInd.size(); i++) {  // add the contribution of gas species that participate in any surf rxn            
		prod1 *= pow(spConc[rxnGasSpInd[i]], rxnsData[j].rxnReacCoeffs[rxnGasSpInd[i]]);
		prod2 *= pow(spConc[rxnGasSpInd[i]], rxnsData[j].rxnProdCoeffs[rxnGasSpInd[i]]);
	  }
	  qProg[j] = (kf * prod1 - kr * prod2);
    }
    else {
	  for(i=nGasSp; i<spNames.size(); i++ )    
		prod1 *= pow(spConc[i], rxnsData[j].rxnReacCoeffs[i]);
	  for(i=0; i<rxnGasSpInd.size(); i++)
		prod1 *= pow(spConc[rxnGasSpInd[i]], rxnsData[j].rxnReacCoeffs[rxnGasSpInd[i]]);
	  qProg[j] = kf * prod1;
	}
  }                // end loop over reactions
  return qProg;    // vector of rate of progress variables for each reaction
}

///////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::surfSpProdRates(vector<double>& comp, double& Temp) {
// This function evaluates only the surface species production rates
// This function is used to converge the steady state surface site fractions
// units are moles/m2*s

	vector<double> rates(nSurfSp,0.0);
	vector<double> rxnRtProg = rxnRateOfProgress(comp, Temp);
	int i,j;
	double sum=0.0;
	for(i=nGasSp; i<spNames.size(); i++, sum=0.0) {
		sum = 0.0;
		for(j=0; j<rxnsData.size(); j++)
			sum += (rxnsData[j].rxnProdCoeffs[i] - rxnsData[j].rxnReacCoeffs[i]) *
			rxnRtProg[j];
		rates[i-nGasSp] = eff_factor * sum;
	}

	return rates;
}

///////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::gasSpProdRates(double& Temp) {
// Function evaluates rates for all the gas species
// Since not all of the gas species in the system participate in surface rxns
// Most of these rates are zero
// This function is the primary interface with REKS 
// This function is used to iterate on the surface gas concentration by 
//    equating the rates to the rate of mass transfer
// Units are moles/m2*s for gas species; Surf sp rates not needed, assumed zero

	vector<double> rates(nGasSp+1, 0.0);                // NOTE THE SIZE IS GAS SPECIES
	vector<double> rxnRtProg = rxnRateOfProgress(molf, Temp);
	int i,j;
	double sum=0.0;
	
	for(i=0; i<rxnGasSpInd.size(); i++) {
		sum = 0.0;
		for(j=0; j<rxnsData.size(); j++)
			sum += (rxnsData[j].rxnProdCoeffs[rxnGasSpInd[i]] -
				    rxnsData[j].rxnReacCoeffs[rxnGasSpInd[i]]) *
					rxnRtProg[j];
		rates[rxnGasSpInd[i]] = eff_factor * sum;
	}

	return rates;
}

///////////////////////////////////////////////////////////////////////////////
double surface_chem::concI(const int i) {
	if(i >= nGasSp) 
		return molf[i]*siteDensity;          // mol/m2
	else                                       // mol/m3
		return molf[i]*pres/Rgas/surfTemp;
}

///////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::conc(vector<double>& comp, double& Temp) {
	// mol/m2 or m3
	int i;
	vector<double> concentrations(nGasSp+nSurfSp,0.0);
	for(i=0; i<nGasSp; i++)
		concentrations[i] = comp[i]*pres/Rgas/Temp;
	for(i=nGasSp; i<nGasSp+nSurfSp; i++)
		concentrations[i] = comp[i]*siteDensity;
	return concentrations;
}

///////////////////////////////////////////////////////////////////////////////
void surface_chem::normalize(vector<double>& comp) {
	double sum = 0.0;
	int i;
	for(i=0; i<nGasSp; i++)
		sum += comp[i];
	if(sum == 0.0) {
		cerr << "\nError, Gas Mole Fractions sum to 0.0 in normalization routine";
		return;
	}
	for(i=0; i<nGasSp; i++)
		comp[i] /= sum;

	if(i==comp.size())
		return;
	for(i=nGasSp, sum = 0.0; i<comp.size(); i++)
		sum += comp[i];
	if(sum == 0.0) {
		cerr << "\nError, Surface Mole Fractions sum to 0.0 in normalization routine";
		return;
	}
	for(i=nGasSp; i<spNames.size(); i++)
		comp[i] /= sum;
}

///////////////////////////////////////////////////////////////////////////////
vector<double> surface_chem::getFilmMolf() {
	// gas only
	int i;
	vector<double> filmMolf(nGasSp, 0.0);
	for(i=0; i<nGasSp; i++)
		filmMolf[i] = 0.5*(bulkMolf[i] + molf[i]);
	return filmMolf;
}

///////////////////////////////////////////////////////////////////////////////
double surface_chem::getMeanMW(vector<double>& gasMolf) {
	// gas MW avg      g/mol
	int i;
	double sum = 0.0;
	for(i=0; i<nGasSp; i++)
		sum += spMWs[i] * gasMolf[i];
	return sum;          // g/mol
}

///////////////////////////////////////////////////////////////////////////////

bool surface_chem::solveSurfaceComposition() {
	// CHECK THIS FUNCTION
    // Nonlinear equation solver
	// rhs is the POSITIVE of the function f (to be made zero)
	// A x = b ; rhs is -b, A is jacobian, x is delta_x for next update
	// pass b into the jacobian to numerically evaluate the derivatives
	// heat flux is kt * (Tb - Ts) is W/m2
	
	int						i, iter, imaxit = 1000;
	bool					fconverged = true;
	const int				sysSize = nSurfSp;
	vector<double>			func(sysSize,0.0);
	vector<double>			deltaX(sysSize,0.0);
	vector<vector<double> > Jac(nSurfSp, vector<double> (nSurfSp+1, 0.0));  // add a column cause linear solve is augmented
	double					norm=0.0, norm_old=0.0;
	vector<double>			molf2 = molf;
	

	func = surfSpProdRates(molf, surfTemp);        
	for(i=0; i<func.size(); i++)
		norm_old += fabs(func[i]);
	cout << "\nnorm_old " << norm_old;
	
	for(iter=0; iter <= imaxit; iter++) {
		if(iter == imaxit) {
			cerr << "\nError, No convergence of site composition in " << imaxit << " iterations";
			return false;
		}
		if( !getJacobian(Jac, surfTemp, func) ) {
			cerr << "\nError Obtaining Jacobian Matrix ";
			return false;
		}
		if( SolveLAE(Jac, func, deltaX) == -1 ) {                
			cerr << "\nError, Jacobian is singular";
			return false;
		}
		// debug ++
		cout << "\nNewton iteration " << iter;
		for(i=0; i<deltaX.size(); i++) 
			cout << endl << spNames[nGasSp+i] << ' ' << deltaX[i] << '\t' << func[i];
		// debug --

		// are we converged?
		fconverged = true;
		for(i=0; i<nSurfSp; i++) {
			if(fabs(molf[i+nGasSp]) >= 1.0e-7) {
				if(fabs(deltaX[i]/molf[i+nGasSp]) >= 1.0e-7) 
					fconverged = false;
			}
			else
				if(fabs(deltaX[i]) > 1.0e-7)
					fconverged = false;
		}

		if(fconverged) {
			for(i=0; i<nSurfSp; i++)
				molf[i+nGasSp] += deltaX[i];
			normalize(molf);
			break;
		}

		// did we decrease the function vector?
		int iter2, imaxit2 = 100;
		for(iter2=0; iter2<=imaxit2; iter2++) {
			if(iter2 == imaxit2) {
				cerr << "\nError, Couldn't decrease function norm in surface site solver";
				return false;
			}
			for(i=0; i<deltaX.size(); i++)
				molf2[i+nGasSp] = molf[i] + deltaX[i];
			normalize(molf2);
			func = surfSpProdRates(molf2, surfTemp);        
			for(i=0; i<func.size(); i++)
				norm += fabs(func[i]);
			//cout << "\nDecreasing step iter, norm " << iter2 << '\t' << norm;
			if(norm < norm_old)
				break;                      // take the current deltaX as next step
			else
				for(i=0;i<deltaX.size(); i++)
					deltaX[i] /= 5.0;
		}
		norm_old = norm;
			
		// update solution
		for(i=0; i<nSurfSp; i++)
			molf[i+nGasSp] += deltaX[i];
		normalize(molf);
		cout << endl << "MOL FRACS "; 
		for(i=0; i<nSurfSp; i++)
			cout << molf[nGasSp+i] << ' ' ;
		
	}    // end iteration

return true;
}

///////////////////////////////////////////////////////////////////////////////

bool surface_chem::solveSurfFixedPoint() {
	// Use a fixed point method instead of the newton's method coded, which may or may
	//      not be working
	// Assumes all rxns irreversible
	
	vector<double> xmol_i_old(molf.size(), 0.0);
    vector<double> spConc = conc(molf, surfTemp);
	double numer=0.0, denom=0.0;
	double prod;
	int i,j,k;
	double urf = 0.2; //0.65;
	double itmax = 5000;
	double rtol = 1.0E-5;
	double maxChange;
	double relChange;

	for(k=0; k<=itmax; k++) {
		vector<double> qprog = rxnRateOfProgress(molf, surfTemp);
		spConc = conc(molf, surfTemp);
		maxChange = 0.0;
		xmol_i_old = molf;
		for(i=nGasSp; i<spNames.size(); i++) {		
			numer = denom = 0.0;
			for(j=0; j<rxnsData.size(); j++) {
				if(rxnsData[j].rxnReacCoeffs[i] != 0 || rxnsData[j].rxnProdCoeffs[i] != 0) {
					prod = qprog[j] * (rxnsData[j].rxnProdCoeffs[i] - rxnsData[j].rxnReacCoeffs[i]);
					if(prod < 0.0) {                 // if order > 1 in sp i then leave one or more [sp i] in denom term
						prod /= (spConc[i]+1.0e-20);
						denom += (prod * -1.0);
					}
					else
						numer += prod;
				}
			}
			molf[i] = (spConc[i] + (numer/(denom+1.0e-30) - spConc[i])*urf)/siteDensity+1.0e-30; 
		}
		

		normalize(molf);
		
		for(i=nGasSp; i<spNames.size(); i++) {
			relChange = fabs((xmol_i_old[i] - molf[i])/xmol_i_old[i]);
			if(relChange > maxChange) 
				maxChange = relChange;
		}
		if(maxChange < rtol) {
			//cout << "\nIterations to Convergence = " << k;
			break;
		}
		if(maxChange < 3.0*rtol) urf = 1.0;
		if(k==itmax-1){
		vector<double> sRates = surfSpProdRates(molf, surfTemp);
		int l;
		cout << endl;
		//for(l=nGasSp; l<spNames.size(); l++)
		//	cout << endl << spNames[l] << '\t' << molf[l];
		//cout << endl << surfTemp << endl;
	    cout << "ITER " << k << endl;
		for(l=nGasSp; l<spNames.size(); l++)
			cout << "\nRates " << spNames[l] << '\t' << sRates[l-nGasSp] << '\t' << molf[l];
	vector<double> sources(nGasSp+1, 0.0);					 // returned; species sources, energy source

	sources = gasSpProdRates(bulkTemp);    // uses molf, not bulkmolf
		for(l=0; l<rxnGasSpInd.size(); l++)
			cout << "\nGas " << spNames[rxnGasSpInd[l]] << '\t' << molf[rxnGasSpInd[l]] << '\t' << sources[rxnGasSpInd[l]];
		} // if(k==itmax-1
		if(k == itmax) {
			cerr << "\nWARNING surface composition not converged in " << itmax << " iterations";

		vector<double> sRates = surfSpProdRates(molf, surfTemp);
		int l;
		cout << endl;
		//for(l=nGasSp; l<spNames.size(); l++)
		//	cout << endl << spNames[l] << '\t' << molf[l];
		//cout << endl << surfTemp << endl;
	    cout << "ITER " << k << endl;
		for(l=nGasSp; l<spNames.size(); l++)
			cout << "\nRates " << spNames[l] << '\t' << sRates[l-nGasSp] << '\t' << molf[l];
	vector<double> sources(nGasSp+1, 0.0);					 // returned; species sources, energy source

	sources = gasSpProdRates(bulkTemp);    // uses molf, not bulkmolf
		for(l=0; l<rxnGasSpInd.size(); l++)
			cout << "\nGas " << spNames[rxnGasSpInd[l]] << '\t' << molf[rxnGasSpInd[l]] << '\t' << sources[rxnGasSpInd[l]];
		return false;
		} // if(k == itmax
	}

	//for(int l=nGasSp; l<spNames.size(); l++)
	//	cout << endl << spNames[l] << '\t' << molf[l];
	//cout << endl;
			vector<double> sRates = surfSpProdRates(molf, surfTemp);
	int l;
	cout << endl;
	for(l=nGasSp; l<spNames.size(); l++)
			cout << "\nRates " << spNames[l] << '\t' << sRates[l-nGasSp] << '\t' << molf[l];
	vector<double> sources(nGasSp+1, 0.0);					 // returned; species sources, energy source

	sources = gasSpProdRates(bulkTemp);    // uses molf, not bulkmolf
		for(l=0; l<rxnGasSpInd.size(); l++)
			cout << "\nGas " << spNames[rxnGasSpInd[l]] << '\t' << molf[rxnGasSpInd[l]] << '\t' << sources[rxnGasSpInd[l]];
	cout << endl;

	return true;
}

///////////////////////////////////////////////////////////////////////////////
//#include <iomanip>  // for debug setprecision
bool surface_chem::getJacobian(vector<vector<double> >& Jac, double &Temp, vector<double>& surfRates) {
	// fix eps for our dilute species
	// CHECK THIS FUNCTION

	const double eps = 1.0e-5;
	double delta;
	int i, j;
	vector<double> comp2 = molf;
	vector<double> surfRates2(nSurfSp, 0.0);

	// surface species rates
	//cout << setprecision(15);
	for(i=0; i<nSurfSp; i++) {                         // loop over columns (same x)
		delta = comp2[i+nGasSp] * eps;
		if(delta < 1.0e-15) delta = 1.0e-5;
		comp2[i+nGasSp] += delta;      
		surfRates2 = surfSpProdRates(comp2, Temp);

		//// debug ++
		//cout << endl;
		//for(j=0; j<surfRates2.size(); j++)
		//	cout << endl << spNames[nGasSp+j] << '\t' << surfRates[j] << '\t' << surfRates2[j] 
		//	<< '\t' << delta << '\t' << molf[nGasSp+j] 
		//	<< '\t' << comp2[nGasSp+j];
		//// debug --

		for(j=0; j<nSurfSp; j++) {                       // place dfj/dxi along a column i	
			Jac[j][i] = (surfRates2[j] - surfRates[j]) / delta;
		}
		comp2[i+nGasSp] -= delta;	
	}

	//// debug ++
	//cout << endl << "Jacobian size: " << Jac.size();
	//for(i=0; i<Jac.size(); i++) {
	//	cout << endl;
	//	for(j=0; j<Jac.size(); j++)
	//		cout << Jac[i][j] << ' ';
	//}
	//cout << endl;
	//// debug --
	return true;
}

///////////////////////////////////////////////////////////////////////////////

vector<double> surface_chem::getBulkGasSources_Hetero() {
	// CHECK THIS FUNCTION
	// this function is the interface with REKS for gas rates
	// returns vector of size corresponding to gas species in surface rxns plus one for temp term
	// be sure to update the data members (bulk composition, Temp) externally
	
	vector<double> sources(nGasSp+1, 0.0);					 // returned; species sources, energy source
	vector<double> km(nGasSp,0.0);                           
	vector<double> old_molf = molf;
	double         old_surfTemp = surfTemp;
	int			   i, iter, itmax = 1000;
	double	       heatFlux = 0.0;
	double         tolerance = 0.0001;                      // fractional change in variables
	bool           fConverged = true;
	double		   urf = 1.0;
	double		   d1;

	for(iter = 1; iter <= itmax; iter++) {
		cout << "\nSurf Gas iter \t" << iter;
		cout << "\nSurf CO\t" << molf[speciesMap.find("CO")->second];
		cout << "\nSurf Temp\t" << surfTemp;
		old_molf = molf;
		old_surfTemp = surfTemp;
		if(iter == itmax)
			cerr << "\nSurface Gas Composition and Temperature not Converged in " << itmax << " iterations";
		
		// get surface site composition
		
		//if( !solveSurfaceComposition() ) {
		if( !solveSurfFixedPoint() ) {
			cerr << "\nError, solving Surface Site Composition";
			return sources;
		}
		
		// get gas species rates
		
		sources = gasSpProdRates(surfTemp);          // full size
		sources.push_back(0.0);

		// get heat flux ( eg sum(hi * wi) = k(Tb-Ts) ) 1st term heat flux; sum over gas+surf
		// species in surf rxns; but wi is zero for surf site species
		
		for(i=0; i<rxnGasSpInd.size(); i++)
			heatFlux += enthalpySp(rxnGasSpInd[i], surfTemp) * sources[rxnGasSpInd[i]];
		
		// update surface temperature

		surfTemp = old_surfTemp + ((bulkTemp - heatFlux / kHeat()) - old_surfTemp) * urf;

		// update surface composition

		km = kMass();
		for(i=0; i<rxnGasSpInd.size(); i++) {
			d1 = (sources[rxnGasSpInd[i]] * Rgas / km[rxnGasSpInd[i]] / pres +
			                       bulkMolf[rxnGasSpInd[i]] / bulkTemp) * surfTemp;
			molf[rxnGasSpInd[i]] = old_molf[rxnGasSpInd[i]] + (d1 - old_molf[rxnGasSpInd[i]]) * urf;
		}
		normalize(molf);
		
		// check convergence 
		
		fConverged = true;
		for(i=0; i<rxnGasSpInd.size(); i++) {
			if( fabs((molf[rxnGasSpInd[i]] - old_molf[rxnGasSpInd[i]]) / 
				old_molf[rxnGasSpInd[i]]) > tolerance )
			{
				fConverged = false;
				break;
			}
		}
		if( fabs((surfTemp - old_surfTemp)/old_surfTemp) > tolerance )
			fConverged = false;
		if(fConverged) {
			cout << "Iterations to Surf T, molf convergence\t" << iter;
			break;
		}
		

	}    // end iteration on surface gas comp and surface temperature
	
	// Convert sources array (currently gas rates) to sources for reks solver
	//		Units are 1/m for species sources (dy/dx), K/m for energy (dT/dx)


	for(i=0; i<rxnGasSpInd.size(); i++)
		sources[rxnGasSpInd[i]] *= (Dh*3.141592654*spMWs[rxnGasSpInd[i]]/1000./mdot);
		// note gases mol/m2/s = km*(Cs - Cb), so you can replace the above as
		// sources = (not *=) above * km * P/R*(molf[sp]/Ts - bulkMolf[sp]/Tb);
	sources[sources.size()-1] = heatFlux*Dh*3.141592654*getMeanMW(bulkMolf) / 
		1000. / mdot / cp_mix(bulkTemp, bulkMolf);
		// as above heatflux can be replaced by kt(Tb-Ts)
	
	return sources;	// mol/m3*s, last term is heat flux	

}	

///////////////////////////////////////////////////////////////////////////////
#include <cstdlib>
vector<double> surface_chem::getBulkGasSources_Homo() {
//	Sources for Gas phase integrator (REKS)
//  Assumes gas and surface composition and temperature are the same.
//  No heat or mass transfer coefficients needed.  Basically, just
//		augment the gas species rates to account for surface reaction.

//  Return array of terms Pw/mdot*MW*surf rate (units 1/m) to add to REKS term

	vector<double> sources;					 // returned; species sources, energy source
	int i;

	for(i=0; i<nGasSp; i++)
		molf[i]     = bulkMolf[i];
	surfTemp = bulkTemp;

	/*if( !solveSurfFixedPoint() ) {
		cerr << "\nError, solving Surface Site Composition";
		return sources;
	}*/ // already solved	
	// get gas species rates

	sources = gasSpProdRates(bulkTemp);    // uses molf, not bulkmolf
	

	// get bulk temperature source
	
	double Tsource = 0.0;
	for(i=0; i<rxnGasSpInd.size(); i++)
		Tsource += enthalpySp(rxnGasSpInd[i], bulkTemp) * sources[rxnGasSpInd[i]];
	Tsource = Tsource *Dh*3.141592654*getMeanMW(bulkMolf) / 
		1000. / mdot / cp_mix(bulkTemp, bulkMolf);
	sources[nGasSp] = -Tsource;  // note sign

	// get gas species sources

	for(i=0; i<rxnGasSpInd.size(); i++) {
		sources[rxnGasSpInd[i]] *= (Dh*3.141592654*spMWs[rxnGasSpInd[i]]/1000./mdot);
		//cout << "\nSources: " << spNames[rxnGasSpInd[i]] 
		//     << '\t' << sources[rxnGasSpInd[i]];
	}
	//cout << "\nTsouce " << '\t' << sources[nGasSp];

	return sources;
}
///////////////////////////////////////////
double surface_chem::temp_rate(const std::vector<double>& sources)
{
	// get bulk temperature source
	
	double Tsource = 0.0;
	int i;
	for(i=0; i<rxnGasSpInd.size(); i++)
		Tsource += enthalpySp(rxnGasSpInd[i], bulkTemp) * sources[rxnGasSpInd[i]] /spMWs[rxnGasSpInd[i]];
	Tsource = Tsource *getMeanMW(bulkMolf) / cp_mix(bulkTemp, bulkMolf);
	return -Tsource;
}

///////////////////////////////////////////

int surface_chem::SolveLAE(vector<vector<double> >& a, const vector<double>& func, vector<double>& x)
//solve linear algebraic equations using Gaussian elimination
//with largest pivot.  a is an augmented matrix

// Tested 6/30/03 DOL;  couldn't get an error with two rows differing by a factor of 2 (1111, 2222)
// Fixed 7/2/03
{
	int n = x.size();
	int i,j,k;
	int irow;
	double pmax;
	double aabs;
	double aswap;
	double fac;
	double sum;

	for(i=0; i<x.size(); i++)
		a[i][n] = -func[i];             // augment the matrix; note the negative sign
	
	for (i=0; i<n-1; i++)
	{
		pmax = 0;
		irow = i;
		for (k=i; k<n; k++)
		{
			aabs = fabs(a[k][i]);
			if (aabs>pmax)
			{
				pmax = aabs;
				irow = k;
			}
		}
		if (pmax<=0) return -1;		//singular
		if (irow!=i)		        //swap
		{
			for (k=i; k<=n; k++)
			{
				aswap = a[i][k];
				a[i][k] = a[irow][k];
				a[irow][k] = aswap;
			}
		}
		//do elimination
		for (j=i+1; j<n; j++)
		{
			fac = a[j][i]/a[i][i];
			for (k=i; k<=n; k++)
			{
				a[j][k] = a[j][k]-fac*a[i][k];
			}
		}
	}
	//back calculation
	k = n-1;
	if(a[k][k] == 0.0)
		return -1;
	x[k] = a[k][n]/a[k][k];
	for (i=n-2; i>=0; i--)
	{
		sum = 0;
		for (j=i+1; j<n; j++)
		{
			sum += a[i][j]*x[j];
		}
		x[i] = (a[i][n]-sum)/a[i][i];
	}
	return 0;
}

