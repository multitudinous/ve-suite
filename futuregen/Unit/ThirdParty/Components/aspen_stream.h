
// class definition for aspen_stream class

#ifndef _aspen_stream_h_
#define _aspen_stream_h_

#include <V21Helper/Therm/thermo.h>
#include <vector>

namespace Vision21 {

class aspen_stream
{
	
public:
	aspen_stream(){my_thermo = NULL;}
	void set_thermo(const thermo* thermo);
	virtual ~aspen_stream(){}

	double get_temp() const {return temp;}
	double get_mass_flow_rate() const {return mass_flow_rate;}
	double get_mole_fraction(const std::string& specie) const;
	std::vector<double> get_mass_fractions() const;
	std::vector<double> get_mole_fractions() const {return mole_fractions;}
	double get_total_molar_fr() const;
	double get_molar_fr(const std::string& specie) const;

	void set_temp(const double& temp);
	void set_mass_flow_rate(const double& mdot);
	bool set_mole_fraction(const std::string& specie, const double& mf);
	bool set_mole_fractions(const std::vector<double>& mf);
	bool set_mole_fractions_from_mmasses(const std::vector<double>& mm);
	void dump_data() const;


private:
	double temp;
	double mass_flow_rate;
	const thermo* my_thermo;
	std::vector<double> mole_fractions;
};

} //# End namespace Vision21

#endif

