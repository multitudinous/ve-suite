#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cmath>
#include <vector>
#include "aspen_stream.h"

using std::cout;
using std::endl;
using std::cin;
using std::vector;

namespace Vision21 {

////////////////////////////////////////////////////////////////////
void aspen_stream::set_thermo(const thermo* thermo)
{
	my_thermo=thermo;
	mole_fractions.resize(my_thermo->get_spec_nam().size());
}

////////////////////////////////////////////////////////////////////
void aspen_stream::set_temp(const double& t)
{
	temp=t;
}

////////////////////////////////////////////////////////////////////
void aspen_stream::set_mass_flow_rate(const double& mdot)
{
	mass_flow_rate=mdot;
}

////////////////////////////////////////////////////////////////////
bool aspen_stream::set_mole_fraction(const string& specie, const double& mf)
{
	bool ret;
	int index = ((thermo*)(my_thermo))->spec_int(specie);  // cast away constness - painful

	if((index>=0)&&(index<mole_fractions.size())){
		mole_fractions[index]=mf;
		ret = true;
	}
	else {
		ret = false;
	}
	return(ret);
}

////////////////////////////////////////////////////////////////////
bool aspen_stream::set_mole_fractions(const vector<double>& mf)
{
	bool ret;

	if(mf.size()==mole_fractions.size()){
		mole_fractions=mf;
		ret=true;
	}
	else {
		ret=false;
	}
	return(ret);
}

////////////////////////////////////////////////////////////////////
bool aspen_stream::set_mole_fractions_from_mmasses(const vector<double>& mm)
{
  int i;

	// set mole fractions using molar masses (kmol(i)/kg-total)
	double total_molar_fr=0.0;
	
	if(mm.size()!=mole_fractions.size()){
		cout<<"Error - set_mole_fractions_from_mmasses\n";
		return(false);
	}

	// find total molar flow rate
	for(i=0;i<mm.size();i++)
		total_molar_fr += mm[i]*mass_flow_rate;

	// now find mole fractions
	for(i=0;i<mm.size();i++)
		mole_fractions[i] = mm[i]*mass_flow_rate/total_molar_fr;

	return(true);
}

////////////////////////////////////////////////////////////////////
double aspen_stream::get_mole_fraction(const std::string& specie) const
{
	double ret;
	int index = ((thermo*)(my_thermo))->spec_int(specie);  // cast away constness - painful

	if((index>=0)&&(index<mole_fractions.size())){
		ret = mole_fractions[index];
	}
	else {
		ret = 0.0;
	}
	return(ret);
}

////////////////////////////////////////////////////////////////////
std::vector<double> aspen_stream::get_mass_fractions() const
{
	
	int size = mole_fractions.size();
	vector<double> mass_fracs(size,0.0);

	double total_molar_fr = mass_flow_rate/((thermo*)(my_thermo))->mweight(mole_fractions);

	for(int i=0;i<size;i++)
		mass_fracs[i] = mole_fractions[i]*total_molar_fr*((thermo*)(my_thermo))->mweight(i)/mass_flow_rate;

	return(mass_fracs);
}

////////////////////////////////////////////////////////////////////
double aspen_stream::get_total_molar_fr() const
{
	return(mass_flow_rate/((thermo*)(my_thermo))->mweight(mole_fractions));
}

////////////////////////////////////////////////////////////////////
double aspen_stream::get_molar_fr(const std::string& specie) const
{
	int index = ((thermo*)(my_thermo))->spec_int(specie);

	if(index<0){
		cout<<"YIKES: get_molar_fr\n";
		return(0.0);
	}

	return(mole_fractions[index]*get_total_molar_fr());
}

////////////////////////////////////////////////////////////////////
void aspen_stream::dump_data() const
{
	cout<<"--------------------------------"<<endl;
	cout<<"Temp: "<<temp<<endl;
	cout<<"Mdot: "<<mass_flow_rate<<endl;
	cout<<"Mole Fractions:"<<endl;
	for(int i=0;i<mole_fractions.size();i++)cout<<mole_fractions[i]<<endl;
	cout<<"--------------------------------"<<endl;
}

} //# End namespace Vision21





