// implementation file for the cyclone class

#pragma warning(disable: 4786)
#include <cstdio>
#include <cstring>
#include <string>
#include "cyclone.h"
#include <cmath>

using std::cout;
using std::endl;
using std::string;

namespace Vision21 {

//   constructor
cyclone::cyclone()
{}

///////////////////////////////////////////////////////////////////////////////////
cyclone::cyclone(const float& N, const float& Vc, const float& particle_density, 
                 const float& Wi, const float& K, const float& gas_den,
                 const float& gas_vis)

:N(N),Vc(Vc),particle_density(particle_density),Wi(Wi),K(K),gas_den(gas_den),gas_vis(gas_vis)
{
	// These parameters are from "Air pollution control engineering" by Noel De Nevers pg 210-216
	// They are used to calcuate particle removal efficiency and pressure drop through a cyclone

	// N is # of turns gas makes around cyclone before exiting (normally N=5)
	// Vc is velocity of particles along curved path in cyclone
	// particle_density is the density of the particles in the cyclone
	// Wi is the with of the inlet duct to the cyclone
	// K is the constant needed for the pressure drop calculation (K is normally =8 for cyclone separators)
}

////////////////////////////////////////////////////////////////////////////////////
float cyclone::calc_dcut()
{
	return(sqrt(9.0*Wi*gas_vis/(2.0*3.14159265*N*Vc*particle_density)));
}

////////////////////////////////////////////////////////////////////////////////////
float cyclone::calc_removal_eff(const float& part_diameter)
{
	return(pow(part_diameter/calc_dcut(),2.0)/(1.0+pow(part_diameter/calc_dcut(),2.0)));
}

////////////////////////////////////////////////////////////////////////////////////
float cyclone::calc_pdrop()
{
	return(K*(gas_den*Vc*Vc/2.0));  // I assume here that Vc = Vi (see page 216 of de never's book)
}

} // end namespace Vision21

