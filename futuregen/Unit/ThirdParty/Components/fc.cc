#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cmath>
#include <algorithm>
#include "fc.h"

using std::cout;
using std::endl;
using std::cin;
using std::vector;

namespace Vision21 {

// notes:
// ------
// this model is only coded to handle CO and H2 coming into the anode as fuels
// this is "hardwired" in both the thermo and electrical calcs below

// the cathode "air" stream is NOT limited to just O2 and N2
// the composition is arbitrary - the code just pulls off enough
// o2 to fully react the CO and H2 coming in the anode stream
// There needs to be enough O2 present in the cathode stream to
// fully react the anode stream! (stoich with fuel_utilization factor)


////////////////////////////////////////////////////////////////
fuel_cell::fuel_cell(const thermo& thm)
:thm(thm)
{	
	cathin.set_thermo(&thm);
	d_cathin.set_thermo(&thm);
	o2_flow.set_thermo(&thm);
	cathout.set_thermo(&thm);
	anodein1.set_thermo(&thm);
	anodein2.set_thermo(&thm);
	anodeout.set_thermo(&thm);
}

////////////////////////////////////////////////////////////////
void fuel_cell::set_cathode_stream(const aspen_stream& ci)
{
	cathin = ci;
}

////////////////////////////////////////////////////////////////
void fuel_cell::set_anode_stream(const aspen_stream& ai)
{
	anodein1 = ai;
}

////////////////////////////////////////////////////////////////
bool fuel_cell::calc_power(double& temp, double& fc_power)
{

	// use secant iteration to converge fc power output (from nerst etc) to thermo qout from network
	
	// Begin secant method
	int flag=0;
	double a,b,fofa,fofb,p,fofp;
	
	// initial guesses for anode temperature
	a=300.0; //750.0+273.15;
	b=3000.0; //1500.0+273.15;
	
	// find f(a) and f(b)
	fofa = fc_thermo_power(a)-fc_electric_power(a);  // must run thermo first so species are correct for electric calc
	fofb = fc_thermo_power(b)-fc_electric_power(b);
	
	for(int i=0;i<500;i++) {
          p = 0.5*(a+b);
	  fofp = fc_thermo_power(p)-fc_electric_power(p);
          if(fofp<0.0) b = p;
          else a = p;

		if(fabs(a-b)<1.0e-8){
			temp=p;
			fc_power = fc_electric_power(p);
			flag=1;
			break;
		}
	  /*		p=b-fofb*(b-a)/(fofb-fofa);
		
		if(p<700) {
		  cerr << "YIKES - tanode in secant < 700, setting to 700\n";
		  p = 700;
		}
		//if(p>1920) {
		//  cerr << "YIKES - tanode in secant > 1920, setting to 1920\n";
		//  p = 1920;
		//}

		if(fabs(p-b)<1.0e-8){
			temp=p;
			fc_power = fc_electric_power(p);
			flag=1;
			break;
		}
		
		a=b; fofa=fofb; b=p;
		fofb = fc_thermo_power(p)-fc_electric_power(p);	*/
	}
	
	bool ret;
	if(flag==0){
	  //cout<<"YIKES - failed to converge in find_temperature (s)!!!!"<<endl;
	  ret=false;
	}
	else ret = true;
	
	return(ret);
}

/////////////////////////////////////////////////////////////////////////////
double fuel_cell::fc_electric_power(const double& tanode)
{
	
	
	// coin  anodein2 - after internal reformer (co molar flow rate - kmol/hr)
	// coout anodeout - after anode (co molar flow rate - kmol/hr)
	// xh2o  anodeout - mole fraction of h2o after anode
	// xh2   anodeout - mole fraction of h2 after anode
	// xco2  anodeout - mole fraction of co2 after anode
	// h2in  anodein2 - after internal reformer (h2 molar flow rate - kmol/hr)
	// h2out anodeout - after anode (molar flow rate - kmol/hr)
	// xo2   cathin - mole fraction of o2 in incoming air to cathode
	// xco   anodeout - mole fraction of co after anode
	
	double coin  = anodein2.get_molar_fr("CO")*3600.0;     
	double coout = anodeout.get_molar_fr("CO")*3600.0;    
	double xh2o  = anodeout.get_mole_fraction("H2O");     
	double xh2   = anodeout.get_mole_fraction("H2");       
	double xco2  = anodeout.get_mole_fraction("CO2");      
	double h2in  = anodein2.get_molar_fr("H2")*3600.0;     
	double h2out = anodeout.get_molar_fr("H2")*3600.0;  ;    
	double xo2   = cathin.get_mole_fraction("O2");      
	double xco   = anodeout.get_mole_fraction("CO");  
	
	double h2;
	double co;
	double dn;
	
	// make local units degC (lazy)
	double tanode_c = tanode - 273.0;
	
	// set cell params for thickness and exponential factors for resistivity (R=A*exp(E/T))
	// use local vars just because I'm lazy
	double athick=fcparams.athick;
	double cthick=fcparams.cthick;
	double ethick=fcparams.ethick; 
	double aa=fcparams.anode_A;
	double ca=fcparams.cathode_A; 
	double ea=fcparams.electrolyte_A;
	double ae=fcparams.anode_E;
	double ce=fcparams.cathode_E;
	double ee=fcparams.electrolyte_E;
	double ar=athick*aa*exp(ae/(tanode_c+273.0));
	double cr=cthick*ca*exp(ce/(tanode_c+273.0));
	double er=ethick*ea*exp(ee/(tanode_c+273.0));
	double apcell=fcparams.area_per_unit_cell;    // area per unit cell - cm^2 
	double cells=fcparams.number_of_cells; // cells=int(dn*2.0*fkg/(0.3*3600.0*apcell))+1.0  // calculate or set number of cells  
	
	// overpotentials (anode, ohmic, concentration)
	double aloss;
	double oloss;
	double closs;
	
	// voltage with losses
	double en;
	
	// set faraday's constant 
	double fg=96487.0;
	double fkg=fg*1000.0; 
	
	// calculate total stack area using area per unit cell and the number of cells
	double tarea=cells*apcell;
	
	// determine if h2 only, or co & h2 reacting 
	if(coin==0.0){
		// h2 only 
        double dgo=55.85*(tanode_c+273.0)-247500.0;
        double eo=-dgo/2.0/fg;       // fyi - not used
        double ddg=log((xh2o)/(xh2*(sqrt(xo2))));
        ddg=ddg*8.314*(tanode_c+273.0);
        double dg=dgo+ddg; 
        en=-dg/2.0/fg + 8.314*(tanode_c+273.0)/4.0/fg*log(fcparams.pressure/100000.0);  // pressure converted to bars
	}
	else {
		// h2 + co reacting 
        double dgo=141.299*(tanode_c+273.0)-525388.0;
        double eo=-dgo/4.0/fg;   // fyi - not used
        double xxx=xh2o*xco2;
        double yyy=xh2*xco*xo2;
        double ddg=log((xxx)/yyy);
        ddg=ddg*8.314*(tanode_c+273.0);
        double dg=dgo+ddg;
        en=-dg/4.0/fg + 8.314*(tanode_c+273.0)/4.0/fg*log(fcparams.pressure/100000.0);  // pressure converted to bars;
	}
	
	// calculate total molar flux of reactants 
	h2=h2in-h2out;
	co=coin-coout;
	dn=h2+co;
	
	// calculate electron flux, current density, exchange cd 
	double fluxh=dn/tarea;
	double fluxs=fluxh/3600;
	double curr=fluxs*4.0*fkg;
	double curro=(-13595.0+15.0*(tanode_c+273))/100.0/100.0;

	// make sure curro is not negative and not zero
	if(curro<1.0e-12){
	  curro = curr/exp(.99/(8.314*(tanode_c+273.0)/2.0/fg/0.5));
	  //cout<<"WARNING: curro = negative!\n";
	}
	
	// calculate anode overpotential 
	if(coin==0.0){
        aloss=(8.314*(tanode_c+273.0)/2.0/fg/0.5)*log(curr/curro);
	}
	else {
        aloss=(8.314*(tanode_c+273.0)/4.0/fg/0.5)*log(curr/curro);
	}
	
	// calculate total resistivity - ohm-cm^2 
	double rtot=ar+cr+er;
	
	// calculate ohmic overpotential 
	oloss=rtot*curr;
	
	// calculate concentration overpotential
	double term = 1.0 - curr/4.0;

	if(term<1.0e-12){
	  term = exp(-.99/(8.314*(tanode_c+273.0)/2.0/fg));
	  //cout<<"WARNING: concentration overpotential term = negative!\n";
	}

	if(coin==0.0){
        closs=-(8.314*(tanode_c+273.0)/2.0/fg)*log(term);
	}
	else {
        closs=-(8.314*(tanode_c+273.0)/4.0/fg)*log(term);
	}

	// calculate total voltage loss 
	double tloss=oloss; //aloss+oloss+closs;
	
	// calculate operating voltage 
	double e=en-tloss;
	
	// calculate power density - w/cm^2, and total power - w
	double tpdens=e*curr;
	double tpower=tpdens*tarea;

	//cout<<"electric iter: "<<tpower/1000.0/1163.09<<endl;

	return(tpower);
}

//////////////////////////////////////////////////////////////////
double fuel_cell::fc_thermo_power(const double& tanode)
{
	
	// we will need these in a few places
	double q_reformer;
	double non_const_tanode = tanode;
	int i,size = thm.get_spec_nam().size();
	vector<double> molar_masses(size,0.0);   // kmol(i)/kg-total
	
	// internal reformer present?
	
	if(fcparams.internal_reformer){
		
		// first, let's do the internal reformer (intref block)
		// incoming stream is anodein1
		// temperature for this equilib calc 
		// is held constant at our current tanode value
		
		// set initial guesses for the molar masses
		vector<double> anodein1_mf = anodein1.get_mass_fractions();
		
		for(int i=0;i<size;i++)
			molar_masses[i] = anodein1_mf[i]/(const_cast<thermo&>(thm)).mweight(i);
		
		(const_cast<thermo&>(thm)).equilb(true,non_const_tanode,fcparams.pressure,0.0,anodein1_mf,molar_masses);
				
		// store results in anodein2 stream
		anodein2.set_temp(tanode);
		anodein2.set_mass_flow_rate(anodein1.get_mass_flow_rate());
		anodein2.set_mole_fractions_from_mmasses(molar_masses);
		
		// calc energy required to heat anodein1 stream up to current tanode temperature
		q_reformer = (const_cast<thermo&>(thm)).enthalpy_mix(anodein2.get_mole_fractions(),non_const_tanode)
			*anodein2.get_total_molar_fr() -
			(const_cast<thermo&>(thm)).enthalpy_mix(anodein1.get_mole_fractions(),anodein1.get_temp())
			*anodein1.get_total_molar_fr();
	}
	else {
		
		// no internal reformer (V21)
		anodein2 = anodein1;
		q_reformer = 0.0;
	}
	
	// now do the cathode splitter (cathspl block) 
	
	// calculate o2 flow based on fuel utilization   
	// set fuel utilization for fuel cell
	
	// calculate o2 flow using fuelut, and h2 and co flow,   
	// based upon full conversion (ie. h2o and co2)

	double o2;
	double required_o2  = (anodein2.get_molar_fr("H2")+anodein2.get_molar_fr("CO"))*0.5*fcparams.fuel_util;
	double available_o2 = cathin.get_molar_fr("O2");

	if(required_o2>available_o2){
	  //cout<<"YIKES: not enough O2 in cathode stream to fully react anode stream - using what's available"<<endl;
		o2 = available_o2;
	}
	else {
		o2 = required_o2;
	}

	o2_flow.set_mass_flow_rate(o2*(const_cast<thermo&>(thm)).mweight("O2"));
	o2_flow.set_mole_fraction("O2",1.0);
	o2_flow.set_temp(cathin.get_temp());
	
	// set o2 depleted cathode flow stream   
	double remaining_o2 = cathin.get_molar_fr("O2") - o2;

	double total_molar_fr_d_cathin = cathin.get_total_molar_fr()-o2;
	
    d_cathin.set_mass_flow_rate(cathin.get_mass_flow_rate()-o2_flow.get_mass_flow_rate());
	d_cathin.set_temp(cathin.get_temp());

	// let's recreate mole fraction array now for depleted stream
	vector<double> dmix(size,0.0);
	vector<double> cathin_mf = cathin.get_mole_fractions();
	
	for(i=0;i<size;i++)
		dmix[i] = cathin_mf[i]*cathin.get_total_molar_fr()/total_molar_fr_d_cathin;
	
	int index = (const_cast<thermo&>(thm)).spec_int("O2");
	dmix[index] = remaining_o2/total_molar_fr_d_cathin;
	d_cathin.set_mole_fractions(dmix);
	
	
	// now let's do the anode
	
	double q_anode=0.0;
	
	// first let's "mix" the streams to get a new mole_fraction array
	double total_mfr_to_anode = o2_flow.get_total_molar_fr() + anodein2.get_total_molar_fr();
	double total_o2_to_anode = o2_flow.get_molar_fr("O2") + anodein2.get_molar_fr("O2");
	
	double anodein2_tmfr = anodein2.get_total_molar_fr();
	vector<double> mix(size,0.0);
	vector<double> anodein2_mf = anodein2.get_mole_fractions();
	
	for(i=0;i<size;i++)
		mix[i] = anodein2_mf[i]*anodein2_tmfr/total_mfr_to_anode;
	
	index = (const_cast<thermo&>(thm)).spec_int("O2");
	mix[index] = total_o2_to_anode/total_mfr_to_anode;
	
	// create a temp stream for this mix
	aspen_stream mix_stream;
	mix_stream.set_thermo(&(const_cast<thermo&>(thm)));
	mix_stream.set_mass_flow_rate(total_mfr_to_anode*(const_cast<thermo&>(thm)).mweight(mix));
	mix_stream.set_mole_fractions(mix);
	
	// set initial guesses for the molar masses
	std::fill(molar_masses.begin(),molar_masses.end(),0.0);
	
	for(i=0;i<size;i++)
		molar_masses[i] = mix[i]/(const_cast<thermo&>(thm)).mweight(i);
	
	vector<double> ms_mf = mix_stream.get_mass_fractions();
	(const_cast<thermo&>(thm)).equilb(true,non_const_tanode,fcparams.pressure,0.0,ms_mf,molar_masses);
		
	// store results in anodeout stream
	anodeout.set_temp(tanode);
	anodeout.set_mass_flow_rate(anodein2.get_mass_flow_rate()+o2_flow.get_mass_flow_rate());
	anodeout.set_mole_fractions_from_mmasses(molar_masses);
	
	// calc energy required to heat anodein2+o2flow streams up to current tanode temperature
	q_anode    = (const_cast<thermo&>(thm)).enthalpy_mix(anodeout.get_mole_fractions(),non_const_tanode)
		         *anodeout.get_total_molar_fr() - 
		         ((const_cast<thermo&>(thm)).enthalpy_mix(anodein2.get_mole_fractions(),anodein2.get_temp())
		         *anodein2.get_total_molar_fr() + (const_cast<thermo&>(thm)).enthalpy_mix(o2_flow.get_mole_fractions(),
		         o2_flow.get_temp())*o2_flow.get_total_molar_fr());
	
	// now do cathode heater block (cathhtr)
	double q_htr=0.0;
	
	cathout = d_cathin;
	cathout.set_temp(tanode);
	
	q_htr      = (const_cast<thermo&>(thm)).enthalpy_mix(cathout.get_mole_fractions(),non_const_tanode)
		         *cathout.get_total_molar_fr() -
		         (const_cast<thermo&>(thm)).enthalpy_mix(d_cathin.get_mole_fractions(),d_cathin.get_temp())
		         *d_cathin.get_total_molar_fr();

	//cout<<"thermo iter: "<<-1.0*(q_reformer+q_anode+q_htr)/1000.0/1163.09<<endl;
	
	return(-1.0*(q_reformer+q_anode+q_htr));
}

///////////////////////////////////////////////////////////////////////
void fuel_cell::get_output_stream(aspen_stream& fc_out)
{

	// combine cathout and anodeout to get fuel cell exit stream

  //aspen_stream fc_out;

	int i,size = thm.get_spec_nam().size();
	
	double cathout_tmfr  = cathout.get_total_molar_fr();
	double anodeout_tmfr = anodeout.get_total_molar_fr();

	double total_mfr = cathout_tmfr + anodeout_tmfr;
	vector<double> cathout_mf  = cathout.get_mole_fractions();
	vector<double> anodeout_mf = anodeout.get_mole_fractions();
	vector<double> mix_mole_fractions(size,0.0);

	for(i=0;i<size;i++)
		mix_mole_fractions[i] = (cathout_mf[i]*cathout_tmfr + anodeout_mf[i]*anodeout_tmfr)/total_mfr;

	// build stream
	fc_out.set_thermo(&thm);
	fc_out.set_temp(cathout.get_temp());  // or anodeout.get_temp since they are equal by definition of this code
	fc_out.set_mass_flow_rate(cathout.get_mass_flow_rate()+anodeout.get_mass_flow_rate());
	fc_out.set_mole_fractions(mix_mole_fractions);
	//return(fc_out);
}

} //# End namespace Vision21
