// implementation file for the thermo class

#pragma warning(disable: 4786)
#include <cstdio>
#include <cstring>
#include <string>
#include "equil.h"
#include <cmath>

using std::cout;
using std::endl;

/////////////////////
void equil::equilibrium(thermo& thm, stream& strm, REAL qht, REAL burnout, REAL mdot,
                 REAL& temp1, std::vector<REAL>& comp, std::vector<REAL>& mol)
{
   REAL pres=strm.get_pres(), temp;
   //cout<<"Pressure (Pa)? ";cin>>pres;cout<<endl;
   const std::map< int, std::vector<int> >& bc_streams = strm.get_bc_streams();
   const std::map< int, std::vector<REAL> >& frac_stream = strm.get_frac_stream();
   //const std::vector<int>& istreams = strm.get_istreams();
   std::map< int, std::vector<int> >::const_iterator itbcst;
   std::map< int, std::vector<REAL> >::const_iterator itfrst;
   std::map< int, std::vector<REAL> >::const_iterator ittmp;
   const std::vector< std::vector<int> >& species = strm.get_species();
   const std::vector< std::vector<REAL> >& frac_spec = strm.get_frac_spec();
   const std::map< int, REAL >& hc0 = strm.get_hc0();
   const std::map< int, bool >& bc_const_temp = strm.get_bc_const_temp_eql();
   std::map< int, bool >::const_iterator itconst;
   //#define PRINT
#ifdef PRINT
   cout << "equilibrium solutions " << endl;
#endif
   int nspc = thm.get_spec_nam().size();
   std::vector<REAL> mass;
   mass.resize(nspc); mol.resize(nspc); comp.resize(nspc);
   for(itbcst=bc_streams.begin(), itfrst=frac_stream.begin(), ittmp = strm.get_bc_temp().begin(),
      itconst=bc_const_temp.begin();
   itbcst!=bc_streams.end(); itbcst++,itfrst++,ittmp++,itconst++){
      bool const_temp = itconst->second;
      int is;
      for(is=0; is<nspc; is++) mass[is] = 0.0;
      int num = (*itbcst).second.size();
      int i, istr;
      // first do all the streams that are not elementally defined
      // enthalpy is calculated at the bc temp
      REAL enth = 0.0;
      for(i=0; i<num; i++){
         istr = (*itbcst).second[i];
         temp = (*ittmp).second[i];
#ifdef PRINT
         cout << " bc " << (*itbcst).first << " temp (K) " << temp << endl;
#endif
         if(!strm.get_lelem()[istr]){
            REAL fr = (*itfrst).second[i];
#ifdef PRINT
	    cout << istr << " " << fr << endl;
#endif
            int nsp = species[istr].size(), is0;
            for(is0=0; is0<nsp; is0++){
               is = species[istr][is0];
               mass[is] += frac_spec[istr][is0]*fr;
               enth += thm.enthalpy_i(is,temp)*frac_spec[istr][is0]*fr/thm.get_mwt()[is];
            }
         }
      }
      int nsp = thm.get_mwt().size();
      for(is=0; is<nsp; is++) mol[is] = mass[is]/thm.get_mwt()[is];
      //REAL enth = 0.0;
      //for(is=0; is<nsp; is++){
      //   enth += thm.enthalpy_i(is,temp)*mol[is];
      //}
      enth *= thm.get_rgas();
      REAL enth_el = 0.0;
      // now do all the streams that are elementally defined
      for(i=0; i<num; i++){
         istr = (*itbcst).second[i];
         temp = (*ittmp).second[i];
#ifdef PRINT
         cout << " bc " << (*itbcst).first << " temp (K) " << temp << endl;
#endif
         if(strm.get_lelem()[istr]){
            std::map< int, REAL >::const_iterator ithc0;
            int ist_num = strm.get_istreams()[istr];
            REAL fr = (*itfrst).second[i];
#ifdef PRINT
	    cout << istr << " " << fr << endl;
#endif
            ithc0 = hc0.find(ist_num);
            enth_el = (*ithc0).second;
            if((*ithc0).first==2){
               enth_el += (enthalpy_coal(temp) - enthalpy_coal(298.0))*thm.get_rgas();
            }else if((*ithc0).first==3){
               enth_el += (enthalpy_h2o_l(temp) - enthalpy_h2o_l(298.0))*thm.get_rgas();
            }
            enth += fr*enth_el;
            int nspa = species[istr].size(), is0;
            for(is0=0; is0<nspa; is0++){
               is = species[istr][is0];
               mass[is] += frac_spec[istr][is0]*fr;
            }
         }
      }
#ifdef PRINT
      if(!const_temp){
         cout << "burnout " << burnout << endl;
         cout << "heat transfer " << qht << " W" << endl;
      }
#endif
      enth -= qht/mdot;
      for(is=0; is<nsp; is++) mol[is] = mass[is]/thm.get_mwt()[is];
      thm.equilb(const_temp,temp1,pres,enth,mass,mol);
      // convert molar mass to mole fraction
#ifdef PRINT
      cout << "temperature " << temp << endl;
#endif
      /*REAL cp_mix;
      if(!const_temp){
         thm.find_temperature(temp,cp_mix,enth,mol);
#ifdef PRINT
         cout << "temperature " << temp << endl;
#endif
}*/
      REAL mwt = 0.0;
      for(is=0; is<nspc; is++){
	 comp[is] = mol[is];
         mwt += mol[is];
      }
      mwt = (REAL)1.0/mwt;
#ifdef PRINT
      cout << " mixture mwt " << mwt << endl;
      cout << " equilibrium mole fractions" << endl;
#endif
      REAL sum = 0.0;
      for(is=0; is<nspc; is++){
         mol[is] *= mwt;
#ifdef PRINT
         cout << thm.get_spec_nam()[is] << " " << mol[is] << endl;
#endif
         sum += mol[is];
      }
#ifdef PRINT
      cout << sum << endl;
#endif
      thm.check_temp_range(mol,temp1);
   }
}
///////////////
REAL equil::enthalpy_h2o_l(const REAL temp)
{
   REAL denom[6];
   denom[0] = 1.0;
   int i;
   for(i=1; i<6; i++) denom[i] = 1.0/i;
   REAL zc[14] = {28.63080, -0.2026099, 7.8529480E-04, -1.3653020E-06, 9.1326968E-10,
      -38579.54,  -118.9505, 28.63080, -0.2026099, 7.8529480E-04,
      -1.3653020E-06, 9.1326968E-10, -38579.54, -118.9505};
   REAL tk[6];
   tk[0] = 1.0;
   int it = 0;
   for(i=1; i<6; i++) tk[i] = tk[i-1]*temp;
   if(temp<1000.0) it = 7;
   REAL h = zc[it+5];
   for(i=0; i<5; i++) h += zc[it+i]*tk[i+1]*denom[i+1];
   return(h/18.0153);
}
///////////////
REAL equil::cp_h2o_l(const REAL temp)
{
   int i;
   REAL zc[14] = {28.63080, -0.2026099, 7.8529480E-04, -1.3653020E-06, 9.1326968E-10,
      -38579.54,  -118.9505, 28.63080, -0.2026099, 7.8529480E-04,
      -1.3653020E-06, 9.1326968E-10, -38579.54, -118.9505};
   REAL tk[6];
   tk[0] = 1.0;
   int it = 0;
   for(i=1; i<5; i++) tk[i] = tk[i-1]*temp;
   if(temp<1000.0) it = 7;
   REAL cp = zc[it+0];
   for(i=1; i<5; i++) cp += zc[it+i]*tk[i];
   return(cp/18.0153);
}
///////////////
REAL equil::enthalpy_coal(const REAL temp)
{
   REAL z1 = 380.0, z2 = 3600.0, theta1 = 380.0, theta2 = 1800.0;
   REAL t1 = theta1/temp, t2 = theta2/temp, avmwt = 12.0;
   REAL h = z1/(exp(t1)-1.0) + z2/(exp(t2)-1.0);
   return(h/avmwt);
}
///////////////
REAL equil::cp_coal(const REAL temp)
{
   REAL theta1 = 380.0, theta2 = 1800.0;
   REAL t1 = theta1/temp, t2 = theta2/temp, avmwt = 12.0;
   REAL z1d = (exp(t1)-1.0)/t1, z2d = (exp(t2)-1.0)/t2; 
   REAL cp = exp(t1)/(z1d*z1d) + 2.0*exp(t2)/(z2d*z2d);
   return(cp/avmwt);
}
///////////////
REAL equil::enthalpy_ash(const REAL temp)
{
   return(593.0*temp + 0.293*temp*temp);
}
///////////////
REAL equil::cp_ash(const REAL temp)
{
   return(593.0 + 0.586*temp);
}
///////////////
REAL equil::vap_pres_h2o(const REAL temp, REAL& hfg)
{
   /*REAL zc[14] = {28.63080, -0.2026099, 7.8529480E-04, -1.3653020E-06, 9.1326968E-10,
      -38579.54,  -118.9505, 28.63080, -0.2026099, 7.8529480E-04,
      -1.3653020E-06, 9.1326968E-10, -38579.54, -118.9505};
  REAL zv[14] = {2.63406539, 0.00311218994, -9.02784507E-07, 1.26730543E-10, -6.91647337E-15,
 -29876.2578, 7.08238745, 4.16755629, -0.00181068678, 5.94508765E-06,
 -4.86708718E-09, 1.52841444E-12, -30289.5469, -0.730879962};*/
   REAL zhg[14] = {-2.599673E+001, 2.057221E-001, -7.861976E-004, 1.365429E-006,- 9.132766E-010,
 8.703282E+003, 1.260329E+002, -2.446324E+001, 2.007992E-001, -7.793497E-004,
 1.360435E-006, -9.117413E-010, 8.289993E+003, 1.182196E+002};
   REAL term;
   REAL temp2 = temp*temp;
   REAL temp3 = temp2*temp;
   REAL temp4 = temp3*temp;
   REAL tboil = 373.15;
   REAL tboil2 = tboil*tboil;
   REAL tboil3 = tboil2*tboil;
   REAL tboil4 = tboil3*tboil;
/*   cout << zhg[12]*(1.0/tboil - 1.0/temp) << " z6 " << endl;
   cout << zhg[7]*log(temp/tboil) << " z1" << endl;
   cout << zhg[8]/2.0*(temp - tboil) << " z2" << endl;
   cout << zhg[9]/6.0*(temp2-tboil2) << " z3" << endl;
   cout << zhg[10]/12.0*(temp3-tboil3) << " z4" << endl;
   cout << zhg[11]/20.0*(temp4-tboil4) << " z5" << endl;*/
   term = zhg[12]*(1.0/tboil-1.0/temp) + zhg[7]*log(temp/tboil) + zhg[8]/2.0*(temp - tboil) + zhg[9]/6.0*(temp2-tboil2) + zhg[10]/12.0*(temp3-tboil3) + zhg[11]/20.0*(temp4-tboil4);
   hfg = zhg[12] + zhg[7]*temp + zhg[8]*0.5*temp2 + zhg[9]/3.0*temp3 + zhg[10]*0.25*temp4 + zhg[11]*0.2*temp*temp4;
   hfg /= 18.0153;
   return(101325.0*exp(term));
}
