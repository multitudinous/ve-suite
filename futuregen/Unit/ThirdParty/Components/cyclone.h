
    // class definition for cyclone class

#ifndef _cyclone_h_
#define _cyclone_h_

#include <cstdio>
#include <cstring>
#include <string>
#include <iostream>
#include <vector>

namespace Vision21 {
  
class cyclone
{

public:

   // mutators
  	cyclone();
	~cyclone(){};
	cyclone(const float& N, const float& Vc, const float& particle_density, 
	        const float& Wi, const float& K, const float& gas_den,
            const float& gas_vis);
   

	float calc_dcut();
	float calc_removal_eff(const float& part_diameter);
	float calc_pdrop();


private:

	float N,Vc,particle_density,Wi,K,gas_den,gas_vis;

};

} // end namespace Vision21 

    //inlines
#endif
