/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
// implementation file for the thermo class
#pragma warning (disable:4786)
#pragma warning (disable:4305)
#include <cstdio>
#include <cstring>
#include <string>
#include "thermo.h"
#include <cmath>

using std::cout;
using std::endl;
using std::cerr;
using std::string;

//   constructor
thermo::thermo()
:mtx()
{
   rgas = (REAL)8314.0;  // J/(kmol*K)
   denom[0] = 1.0;
   int i;
   for(i=1; i<6; i++) denom[i] = 1.0/(REAL)i;
   int junk = mwt.size();
}

////////////////////////////////
thermo::thermo(const string thermo_file)
:mtx()
{
   read_thermo(thermo_file);
   rgas = (REAL)8314.0;  // J/(kmol*K)
   denom[0] = 1.0;
   int i;
   for(i=1; i<6; i++) denom[i] = 1.0/(REAL)i;
   int junk = mwt.size();
}

////////////////////////////////
bool thermo::read_thermo(const string& thermo_file)
{
   
   FILE *s1;
   
   char line[200];
   
   has_ljs = true; // true until proven false
   
   // open file
   if((s1=fopen(thermo_file.c_str(),"rt"))==NULL){
      fprintf(stderr, "Fail to open therm file");
      return false;
   }
   
   fscanf(s1,"%d",&nel);
   fscanf(s1,"%[^\n]",line);
   //cout << endl;
   //cout << "thermodynamic input" << endl;
   //cout << " number of elements " << nel << endl;
   el_nam.resize(nel);
   atwt.resize(nel);
   valence.resize(nel);
   
   char str[20], nam[20];
   strcpy(str,"%s ");
   strcat(str,FSTR);
   strcat(str," ");
   strcat(str,FSTR);
   std::map<std::string, int>::iterator iter;
   int iel, cnt = 0;
   for(iel=0; iel<nel; iel++){
      fscanf(s1,str,&nam,&atwt[iel],&valence[iel]);
      el_nam[iel] = nam;
      iter = nam_el.find(el_nam[iel]);
      if(iter==nam_el.end()){
         nam_el[el_nam[iel]] = iel;
      }else{
         cnt++;
         cout << " element " << el_nam[iel] << " appears twice in therm file " << endl;
      }
      //cout << el_nam[iel] << " " << atwt[iel] << " " << valence[iel] << endl;
   }
   
   fscanf(s1,"%d",&nspc);
   //cout << " number of species " << nspc << endl;
   spec_nam.resize(nspc);
   mwt.resize(nspc);
   mol_at_spc.resize(nel);
   for(iel=0; iel<nel; iel++) mol_at_spc[iel].resize(nspc);
   phase.resize(nspc);
   iphase.resize(nspc);
   tmin.resize(nspc);
   tmax.resize(nspc);
   tmid.resize(nspc);
   zcoef.resize(nspc);
   int isp;
   for(isp=0; isp<nspc; isp++) zcoef[isp].resize(14);
   tmp_min = (REAL)0.0;
   tmp_max = (REAL)10000.0;
   strcat(str," ");
   strcat(str,FSTR);
   char str1[20];
   strcpy(str1,"%s ");
   strcat(str1,FSTR);
   int nel_sp, ie, iph = -1;
   REAL at_sp;
   std::string name;
   for(isp=0; isp<nspc; isp++){
      fscanf(s1,"%s",&nam);
      spec_nam[isp] = nam;
      iter = nam_spec.find(spec_nam[isp]);
      if(iter==nam_spec.end()){
         nam_spec[spec_nam[isp]] = isp;
      }else{
         cnt++;
         cout << " species " << spec_nam[isp] << " appears twice in therm file " << endl;
      }
      //cout << spec_nam[isp] << endl;
      mwt[isp] = 0.0;
      for(iel=0; iel<nel; iel++) mol_at_spc[iel][isp] = 0.0;
      fscanf(s1,"%d",&nel_sp);
      for(ie=0; ie<nel_sp; ie++){
         fscanf(s1,str1,&nam,&at_sp);
         name = nam;
         iter = nam_el.find(name);
         if(iter!=nam_el.end()){
            iel = (*iter).second;
         }else{
            cnt++;
            cout << " element " << name << "  for species " << spec_nam[isp] <<
               " not found" << endl;
         }
         mol_at_spc[iel][isp] = at_sp;
         mwt[isp] += mol_at_spc[iel][isp]*atwt[iel];
      }
      fscanf(s1,str,&nam,&tmin[isp],&tmax[isp],&tmid[isp]);
      if(tmin[isp]>tmp_min) tmp_min = tmin[isp];
      if(tmax[isp]<tmp_max) tmp_max = tmax[isp];
      phase[isp] = nam;
      iter = nam_phase.find(phase[isp]);
      if(iter!=nam_phase.end()){
         iphase[isp] = (*iter).second;
      }else{
         phase_nam.push_back(phase[isp]);
         iph = phase_nam.size()-1;
         nam_phase[phase[isp]] = iph;
         iphase[isp] = iph;
      }
      int i;
      for(i=0; i<14; i++){
         fscanf(s1,FSTR,&zcoef[isp][i]);
      }
   } // for(isp
   int nph = phase_nam.size();
   totmol.resize(nph);
   sum_phase.resize(nph);
   sumh_phase.resize(nph);
   
   // read lenard-jones parameters for transport properties
   s_lj.resize(nspc);
   ek_lj.resize(nspc);
   delta_lj.resize(nspc);
   
   strcpy(str,"%s ");
   strcat(str,FSTR);
   strcat(str," ");
   strcat(str,FSTR);
   strcat(str," ");
   strcat(str,FSTR);
   
   REAL s_lj0, ek_lj0, delta_lj0;
   int i;
   for(isp=0; isp<nspc; isp++){
      if(4!=fscanf(s1,str,&nam,&s_lj0,&ek_lj0,&delta_lj0)) {
         cout<<"Error reading LJ parameters!!!"<<endl;
         s_lj.clear();
         ek_lj.clear();
         delta_lj.clear();
         has_ljs = false;
         break;
      }
      name = nam;
      iter = nam_spec.find(name);
      if(iter!=nam_spec.end()){
         i = (*iter).second;
         s_lj[i] = s_lj0;
         ek_lj[i] = ek_lj0;
         delta_lj[i] = delta_lj0;
      }else{
         cnt++;
         cout << " species " << name << " not found " << endl;
      }
   }
   
   if(cnt) abort();
   
   int nmat = nel + phase_nam.size() + 1;
   mtx.resize(nmat);
   
   fclose(s1); 
   
   b0.resize(nel);
   gfe.resize(nspc);
   gfe0.resize(nspc);
   dmol.resize(nspc);
   enth0.resize(nspc);
   cp0.resize(nspc);

   return true;
}

//////////////////////////////////////////////////////////
int thermo::spec_int(const string& name)
{
   int isp = -1;
   std::map< std::string, int >::const_iterator iter;
   iter = nam_spec.find(name);
   
   if(iter!=nam_spec.end())
      isp = (*iter).second;
   
   return(isp);
}

//////////////////////////////////////////////////////////
REAL thermo::mweight(const int& index)
{

	if(index<0||index>(nspc-1)){
		cout << "mweight (index) index out of range!"<< endl;
		return(0.0);
	}

	return(mwt[index]);
}

//////////////////////////////////////////////////////////
REAL thermo::mweight(const string& name)
{
  std::map< std::string, int >::const_iterator iter = nam_spec.find(name);
	
  if(iter==nam_spec.end()) {
    cerr << " mweight(): specie " << name << " not known\n";
    return(-1.0);
  }

  return(mwt[(*iter).second]);
}

//////////////////////////////////////////////////////////
REAL thermo::mweight(const std::vector<REAL>& composition)
{

	// this function calculates composite molecular weight
	// composition is in mole fractions

	int is, num = composition.size();
	if(num!=nspc){
		cout << " the mol array must be sized to " << nspc << endl;
		cout << " consistent with therm file - fatal error!!! " << endl;
		return(0.0);
	}
	
	// calculate MW for mixture
	REAL mw = 0.0;
	for(is=0; is<nspc; is++){
		mw += mwt[is]*composition[is];
	}
	return(mw);
}
//////////////////////////////////////////////////////////
REAL thermo::atweight(const string& name)
{
  std::map< std::string, int >::const_iterator iter = nam_el.find(name);
	
  if(iter==nam_el.end()) {
    cerr << " atweight(): specie " << name << " not known\n";
    return(-1.0);
  }

  return(atwt[(*iter).second]);
}

//////////////////////////////////////////////////////////
REAL thermo::enthalpy_is(const int& is, REAL temp)
{
   return(enthalpy_i(is, temp)*rgas);
}
//////////////////////////////////////////////////////////
REAL thermo::enthalpy_is(const std::string& name, REAL temp)
{
  std::map< std::string, int >::const_iterator iter = nam_el.find(name);
  
  if(iter==nam_el.end()) {
    cerr << " enthalpy_is(): specie " << name << " not known\n";
    return(-1.0);
  }
  
  return(enthalpy_i(iter->second, temp)*rgas);
}
//////////////////////////////////////////////////////////
REAL thermo::enthalpy_i(const int& is, REAL temp)
{
   REAL tk[6], cpi, temp1 = temp;
   bool extrap = (temp>tmax[is]||temp<tmin[is]);
   if(temp>tmax[is]) temp = tmax[is];
   if(temp<tmin[is]) temp = tmin[is];
   if(extrap) cpi = cp_i(is,temp);
   tk[0] = (REAL)1.0;
   int i, it = 0;
   for(i=1; i<6; i++) tk[i] = tk[i-1]*temp;
   if(temp<tmid[is]) it = 7;
   REAL h = zcoef[is][it+5];
   for(i=0; i<5; i++) h += zcoef[is][it+i]*tk[i+1]*denom[i+1];
   if(extrap) h += cpi*(temp1 - temp);
   return(h);
}

///////////////////////////////////////////////
REAL thermo::enthalpy_mix(const std::vector<REAL>& composition, const REAL& temp)
{
   
   int is, num = composition.size();
   if(num!=nspc){
      cout << " the mol array must be sized to " << nspc << endl;
      cout << " consistent with therm file - fatal error!!! " << endl;
      return(-1.0);
   }
   
   // calculate enthalpy for mixture
   REAL enth = 0.0;
   for(is=0; is<nspc; is++){
      enth += enthalpy_i(is,temp)*composition[is];
   }
   return(enth *= rgas);
}

//////////////////////////////////////////////////////////
REAL thermo::entropy_is(const int& is, REAL temp)
{
   return(entropy_i(is,temp)*rgas);
}
//////////////////////////////////////////////////////////
REAL thermo::entropy_is(const std::string& name, REAL temp)
{
  std::map< std::string, int >::const_iterator iter = nam_el.find(name);
  
  if(iter==nam_el.end()) {
    cerr << " entropy_is(): specie " << name << " not known\n";
    return(-1.0);
  }
  
   return(entropy_i(iter->second,temp)*rgas);
}
//////////////////////////////////////////////////////////
REAL thermo::entropy_i(const int& is, REAL temp)
{
   REAL tk[6], cpi, temp1 = temp;
   bool extrap = (temp>tmax[is]||temp<tmin[is]);
   if(temp>tmax[is]) temp = tmax[is];
   if(temp<tmin[is]) temp = tmin[is];
   if(extrap) cpi = cp_i(is,temp);
   tk[0] = (REAL)1.0;
   int i, it = 0;
   for(i=1; i<5; i++) tk[i] = tk[i-1]*temp;
   if(temp<tmid[is]) it = 7;
   REAL s = zcoef[is][it+6] + zcoef[is][it]*log(temp);
   for(i=1; i<5; i++) s += zcoef[is][it+i]*tk[i]*denom[i];
   if(extrap) s += cpi*log(temp1/temp);
   return(s);
}

///////////////////////////////////////////////
REAL thermo::entropy_mix(const std::vector<REAL>& composition, const REAL& temp, const REAL& pres)
{
   
   int is, num = composition.size();
   if(num!=nspc){
      cout << " the mol array must be sized to " << nspc << endl;
      cout << " consistent with therm file - fatal error!!! " << endl;
      return(-1.0);
   }
   
   // calculate entropy for mixture
   REAL entropy = 0.0;
   for(is=0; is<nspc; is++){
      entropy += composition[is]*(entropy_i(is,temp)*rgas - 
         ((composition[is]>0.0)?(rgas*log(composition[is]*pres/101.3e3)):0.0));   
   }
   return(entropy);
}

///////////////////
REAL thermo::cp_is(const int& is, REAL temp)
{
   return(cp_i(is,temp)*rgas);
}
///////////////////
REAL thermo::cp_is(const std::string& name, REAL temp)
{
  std::map< std::string, int >::const_iterator iter = nam_spec.find(name);
  
  if(iter==nam_spec.end()) {
    cerr << " cp_is(): specie " << name << " not known\n";
    return(-1.0);
  }
  
   return(cp_i(iter->second,temp)*rgas);
}
///////////////////
REAL thermo::cp_i(const int& is, REAL temp)
{
   REAL tk[6];
   if(temp>tmax[is]) temp = tmax[is];
   if(temp<tmin[is]) temp = tmin[is];
   tk[0] = (REAL)1.0;
   int i, it = 0;
   for(i=1; i<5; i++) tk[i] = tk[i-1]*temp;
   if(temp<tmid[is]) it = 7;
   REAL cp = zcoef[is][it+0];
   for(i=1; i<5; i++) cp += zcoef[is][it+i]*tk[i];
   return(cp);
}

///////////////////////////////////////////////
REAL thermo::cp_mix(const std::vector<REAL>& composition, const REAL& temp)
{
   
   int is, num = composition.size();
   if(num!=nspc){
      cout << " the mol array must be sized to " << nspc << endl;
      cout << " consistent with therm file - fatal error!!! " << endl;
      return(-1.0);
   }
   // calculate cp for mixture
   REAL cp = 0.0;
   for(is=0; is<nspc; is++){
      cp += cp_i(is, temp)*composition[is];
   }
   return(cp *= rgas);
}

///////////////////////////////////////////////////////////////
bool thermo::find_temperature(REAL& temp, REAL& cp_mix, const REAL& enth,
                              const std::vector<REAL>& mol)
{
  int is, num = mol.size();
   if(num!=nspc){
      cout << " the mol array must be sized to " << nspc << endl;
      cout << " consistent with therm file " << endl;
      return false;
   }
   int iter=0;
   REAL enth_mix;
   do{
      enth_mix = 0.0;
      cp_mix = 0.0;
      for(is=0; is<nspc; is++){
         enth_mix += enthalpy_i(is,temp)*mol[is];
         cp_mix += cp_i(is,temp)*mol[is];
      }
      enth_mix *= rgas;
      cp_mix *= rgas;
      temp = temp - (enth_mix - enth)/cp_mix;
   } while((++iter<100) && (fabs(enth_mix-enth)>=(REAL)1.0e-6*cp_mix*temp));

   if(!(iter<100))
     cerr << "YIKES - failed to converge in find_temperature (s)!!!!\n";
   
   return(iter<100);
}

//////////////////////////////////////////////////////////////////
bool thermo::find_temperature(REAL& temp, const REAL& entropy,
                              const std::vector<REAL>& composition, const REAL& pres)
{
   
   int num = composition.size();
   if(num!=nspc){
      cout << " the composition array must be sized to " << nspc << endl;
      cout << " consistent with therm file " << endl;
      return false;
   }
   
   // Begin secant method
   bool flag=false;
   float a,b,fofa,fofb,p;
   
   // initial guesses
   a=temp;
   b=temp*1.1;
   
   // find f(a) and f(b)
   fofa = entropy_mix(composition, a, pres)-entropy;
   fofb = entropy_mix(composition, b, pres)-entropy;
   
   int i=0;
   do {
      
      p=b-fofb*(b-a)/(fofb-fofa);
      
      if(fabs(p-b)<1.0e-6*entropy) {
         temp=p;
         flag=true;
      }
      
      a=b; fofa=fofb; b=p;
      fofb = entropy_mix(composition, p, pres)-entropy;  

   } while((++i<500) && !flag);
   
   if(!flag)
     cout << "YIKES - failed to converge in find_temperature (s)!!!!" << endl;
        
   return(flag);
}

//////////////////////////////////////////////////////////////////////////////////
REAL thermo::thermal_cond_mix(const int& ieuck, const REAL& temp_gas, const REAL& temp,
                              const std::vector<REAL>& composition)
{
   int ii;
   
   // transport property (check out trnprp)
   
   REAL tfilm = (temp_gas+temp)/2.0;
   std::vector<REAL> emu(nspc);
   std::vector<REAL> rk(nspc);
   
   for(ii=0;ii<nspc;ii++){
      if(tmid[ii]==1000.0){
         REAL tek = temp_gas/ek_lj[ii];
         REAL colmu = sigmam(tek,delta_lj[ii]);
         emu[ii] = 2.6693e-6*sqrt(mwt[ii]*tfilm)/(pow(s_lj[ii],2.0)*colmu);
         if(ieuck==1){
            rk[ii] = (cp_i(ii,temp_gas)*rgas/mwt[ii]+1.25*rgas/mwt[ii])*emu[ii];
         } else {
            rk[ii] = 1.25*(cp_i(ii,temp_gas)*rgas/mwt[ii]+rgas/(2.0*mwt[ii]))*emu[ii];
         }
      }
   }
   
   // loop for mixture thermal conductivity
   REAL phiik;
   REAL sumxk  = 0.0;
   REAL sumxfi = 0.0;
   
   for(ii=0;ii<nspc;ii++){
      if(tmid[ii]==1000.0) {
         sumxfi = 0.0;
         for(int jj=0;jj<nspc;jj++){
            if (tmid[jj]==1000.0) {
               phiik = 0.3535534*sqrt(1.0/(1.0+mwt[ii]/mwt[jj]))*pow((1.0+sqrt(emu[ii]/emu[jj])*
                  pow((mwt[jj]/mwt[ii]),(0.25))),2.0);
            }
            sumxfi = sumxfi+composition[jj]*phiik;
         }
         if (composition[ii]>=(1.0e-30)) {
            sumxk = sumxk+composition[ii]*rk[ii]/sumxfi;
         }
      }
   }
   
   // mixture thermal conductivity
   REAL rkg = sumxk;
   return(rkg);
}

//////////////////////////////////////////////////////////////////////////////////
REAL thermo::viscosity_mix(const REAL& temp_gas, const REAL& temp,const std::vector<REAL>& composition)
{
   int ii;
   
   // transport property (check out trnprp)
   
   REAL tfilm = (temp_gas+temp)/2.0;
   std::vector<REAL> emu(nspc);
   
   for(ii=0;ii<nspc;ii++){
      if(tmid[ii]==1000.0){
         REAL tek = temp_gas/ek_lj[ii];
         REAL colmu = sigmam(tek,delta_lj[ii]);
         emu[ii] = 2.6693e-6*sqrt(mwt[ii]*tfilm)/(pow(s_lj[ii],2.0)*colmu);
      }
   }
   
   // loop for mixture viscosity
   REAL phiik;
   REAL sumxk  = 0.0;
   REAL sumxfi = 0.0;
   REAL sumxmu = 0.0;
   
   for(ii=0;ii<nspc;ii++){
      if(tmid[ii]==1000.0) {
         sumxfi = 0.0;
         for(int jj=0;jj<nspc;jj++){
            if (tmid[jj]==1000.0) {
               phiik = 0.3535534*sqrt(1.0/(1.0+mwt[ii]/mwt[jj]))*pow((1.0+sqrt(emu[ii]/emu[jj])*
                  pow((mwt[jj]/mwt[ii]),(0.25))),2.0);
            }
            sumxfi = sumxfi+composition[jj]*phiik;
         }
         if (composition[ii]>=(1.0e-30)) {
            sumxmu = sumxmu+composition[ii]*emu[ii]/sumxfi;
         }
      }
   }
   
   // mixture viscosity
   REAL emug = sumxmu;
   return(emug);
}

//////////////////////////////////////////////////////////////////////////////////
REAL thermo::prandtl_mix(const int& ieuck, const std::vector<REAL>& composition, 
                         const REAL& temp_gas, const REAL& temp_film_ref)
{
   
   // transport property (check out trnprp)
   
   // calculate prandtl number
   REAL prandtl = cp_mix(composition,temp_gas)*viscosity_mix(temp_gas,temp_film_ref,composition)
      /thermal_cond_mix(ieuck,temp_gas,temp_film_ref,composition);
   
   return(prandtl);
}

/////////////////////////////////////////////////////////////////////////
std::vector<REAL> thermo::get_diffus(const REAL& temp_gas, const REAL& temp,
                                     const std::vector<REAL>& composition, const REAL& pres)
{
   
   // transport property (check out trnprp)
   
   REAL tfilm = (temp_gas+temp)/2.0;
   
   // calculate diffusivities (diffusivity of THIS species through THIS mixture - mass transfer)
   
   REAL tek;
   REAL coldif;
   REAL dift;
   REAL sumxd = 0.0;
   std::vector<REAL> difm(nspc);
   
   // loop for diffusivities
   for(int ii=0;ii<nspc;ii++){
      sumxd = 0.0;
      for(int jj=0;jj<nspc;jj++){
         if (tmid[jj]==1000.0&&(ii!=jj)) {
            tek = tfilm/sqrt(ek_lj[ii]*ek_lj[jj]);
            coldif = sigmad(tek);
            dift = 1.883444e-2*pow(tfilm,(1.5))*sqrt(1.0/mwt[ii]+1.0/mwt[jj])/(pres*pow(((s_lj[ii]+s_lj[jj])/2.0),2.0)*coldif);
            sumxd = sumxd+composition[jj]/dift;
         }
      }
      
      // species diffusivities in mixture 
      difm[ii] = (1.0-composition[ii])/sumxd;
   }
   return(difm);
}

//////////////////////////////////////////////////////////////////////
bool thermo::equilb(bool const_temp,
                    REAL& temp, const REAL& pres, const REAL& enth,
                    std::vector<REAL>& mass, std::vector<REAL>& mol)
{
	// incoming units:
	// ---------------
	// mass = mass fractions (kgi/kgtotal)
	// mol  = molar masses (kgmol/kgtotal)

	// for fixed temperature calculations, enth is IGNORED
	// routine gets elemental composition from "mass"
	// but "mass" is otherwise unchanged

	// the result of equil calc is put in "mol" and has units of kgmol/kg

   std::vector<REAL>& mat = mtx.get_mat();
   std::vector<REAL>& bb = mtx.get_bb();
   std::vector<REAL>& uu = mtx.get_uu();
   int& nmat = mtx.get_nmat();
   int nmat_sav = nmat;
   if(const_temp) nmat--;
   int is, num = mass.size();
   if(num!=nspc){
      cout << " the species arrays must be sized to " << nspc << endl;
      cout << " consistent with therm file " << endl;
      return false;
   }
   REAL mol0;
   int ie, iph, nph = phase_nam.size();
   for(iph=0; iph<nph; iph++) totmol[iph] = 0.0;
   for(ie=0; ie<nel; ie++) b0[ie] = 0.0;
   for(is=0; is<num; is++){
      mol0 = mol[is];
      mol[is] = mass[is]/mwt[is];
      iph = iphase[is];
      totmol[iph] += mol[is];
      gfe0[is] = enthalpy_i(is,temp)/temp - entropy_i(is,temp);
      if(phase[is]=="G") gfe0[is] += log(pres/(REAL)101325.0);
      for(ie=0; ie<nel; ie++) b0[ie] += mol_at_spc[ie][is]*mass[is]/mwt[is];
      mol[is] = mol0 + (REAL)1.0e-10;
   }
   
   REAL term, act_coef = 1.0;
   int i, nmat2 = nmat*nmat, iter, niter = 500;
   for(iter=0; iter<niter; iter++){
      for(i=0; i<nmat2; i++) mat[i] = 0.0;
      for(i=0; i<nmat; i++) bb[i] = 0.0;
      for(iph=0; iph<nph; iph++) totmol[iph] = 0.0;
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         totmol[iph] += mol[is];
      }
      for(is=0; is<nspc; is++) {
         enth0[is] = enthalpy_i(is,temp)/temp;
         cp0[is] = cp_i(is,temp);
         gfe0[is] = enth0[is] - entropy_i(is,temp);
         if(phase[is]=="G") gfe0[is] += log(pres/(REAL)101325.0);
         gfe[is] = gfe0[is];
         iph = iphase[is];
         // set activity coef here
         if(mol[is]) gfe[is] += log(mol[is]/(totmol[iph]+1.0e-20));
      }
      int j;
      for(i=0; i<nel; i++){
         for(j=0; j<nel; j++){
            for(is=0; is<nspc; is++){
               mat[i+j*nmat] += mol_at_spc[i][is]*mol_at_spc[j][is]*mol[is];
            }
         }
         for(is=0; is<nspc; is++){
            term = mol_at_spc[i][is]*mol[is];
            iph = iphase[is];
            mat[i+(nel+iph)*nmat] += term;
            if(!const_temp) mat[i+(nmat-1)*nmat] += term*enth0[is];
         }
      }
      for(j=0; j<nel; j++){
         for(is=0; is<nspc; is++){
            iph = iphase[is];
            mat[nel+iph+j*nmat] += mol_at_spc[j][is]*mol[is];
         }
      }
      for(iph=0; iph<nph; iph++){
         sum_phase[iph] = 0.0;
         sumh_phase[iph] = 0.0;
      }
      REAL sumh = 0.0;
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         sum_phase[iph] += mol[is];
         term = mol[is]*enth0[is];
         sumh_phase[iph] += term;
         sumh += term;
      }
      for(iph=0; iph<nph; iph++){
         mat[nel+iph+(nel+iph)*nmat] += sum_phase[iph] - totmol[iph];
         if(!const_temp) mat[nel+iph+(nmat-1)*nmat] += sumh_phase[iph];
      }
      if(!const_temp){
         for(j=0; j<nel; j++){
            for(is=0; is<nspc; is++){
               mat[nmat-1+j*nmat] += mol_at_spc[j][is]*mol[is]*enth0[is];
            }
         }
         for(iph=0; iph<nph; iph++)
            mat[nmat-1+(nel+iph)*nmat] += sumh_phase[iph];
         REAL sum1 = 0.0;
         for(is=0; is<nspc; is++){
            sum1 += mol[is]*(cp0[is]+enth0[is]*enth0[is]);
         }
         mat[nmat-1+(nmat-1)*nmat] += sum1;
      } // if(!const_temp)
      for(i=0; i<nel; i++){
         bb[i] += b0[i];
         for(is=0; is<nspc; is++){
            term = mol_at_spc[i][is]*mol[is];
            bb[i] += term*gfe[is];
            bb[i] -= term;
         }
      }
      for(is=0; is<nspc; is++){
         term = mol[is]*gfe[is];
         iph = iphase[is];
         bb[nel+iph] += term;
         if(!const_temp) bb[nmat-1] += term*enth0[is];
      }
      for(iph=0; iph<nph; iph++)
         bb[nel+iph] += totmol[iph] - sum_phase[iph];
      if(!const_temp) bb[nmat-1] += enth/(rgas*temp) - sumh;
      mtx.solv_mat((REAL)1.0e-8);
      for(is=0; is<nspc; is++){
         iph = iphase[is];
         dmol[is] = uu[nel+iph] + enth0[is]*uu[nmat-1] - gfe[is];
         for(i=0; i<nel; i++) dmol[is] += mol_at_spc[i][is]*uu[i];
      }
      REAL lam1 = 1.0, lam2 = 1.0, lam, max, max1;
      for(is=0; is<nspc; is++){
         if(dmol[is]>0.0){
            iph = iphase[is];
            if(mol[is]/(totmol[iph]+1.0e-20)>(REAL)1.0e-8){
               max = 0.0;
               max1 = fabs(5.0*uu[nel+iph]);
               if(max1>max) max = max1;
               max1 = fabs(5.0*uu[nmat-1]);
               if(max1>max) max = max1;
               max1 = fabs(dmol[is]);
               if(max1>max) max = max1;
               lam = (REAL)2.0/max;
               if(lam<lam1) lam1 = lam;
            }else{
               lam = fabs((-log(mol[is]/(totmol[iph]+1.0e-20))-(REAL)9.2103404)
                  /(dmol[is] - uu[nel+iph]));
               if(lam<lam2) lam2 = lam;
            }
         } // if(dmol[is]>0.0
      } // for(is
      lam = 1.0;
      if(lam1<1.0) lam = lam1;
      if(lam2<lam) lam = lam2;
      max = 0.0;
      REAL term1;
      for(is=0; is<nspc; is++){
         term1 = lam*dmol[is];
         if(term1>70.0) term1 = 70.0;
         mol[is] *= exp(term1);
         iph = iphase[is];
         term1 = mol[is]/(totmol[iph]+1.0e-20)*dmol[is];
         if(fabs(term1)>max) max = fabs(term1);
      }
      for(iph=0; iph<nph; iph++){
         term1 = lam*uu[nel+iph];
         if(term1>70.0) term1 = 70.0;
         totmol[iph] *= exp(term1);
         if(fabs(uu[nel+iph])>max) max = fabs(uu[nel+iph]);
      }
      if(!const_temp) temp *= exp(lam*uu[nmat-1]);
      if(max<=0.5e-4){
         if(!const_temp){
            if(fabs(uu[nmat-1])<=1.0e-4){              
               break;
            }
         }else{
            break;
         }
      }
   } // for(iter
   nmat = nmat_sav;
   if(iter==niter) {
     //cout << "Equilibrium may not be converged" << endl;
     return false;
   }

   return true;
}
///////////////////////////////////////////////////////////////////////
REAL thermo::sigmam(const REAL& tek, const REAL& del)
{
   
   REAL tek_mod,del_mod,a,b,c;
   
   static REAL delta[] = {0,0.25,0.5,0.75,1.0,1.5,2.0,2.5};
   
   static REAL am[] = {0.47395,0.47808,0.50119,0.54668,0.60910,
      0.45351,0.89016,1.01037,0.43969,0.44882,0.48192,
      0.53732,0.60815,0.76710,0.91660,1.04383,0.16152,
      0.16285,0.17807,0.20258,0.23287,0.31112,0.41063,0.52600};
   
   static REAL bm[] = {-0.53203,-0.51551,-0.49752,-0.49670,-0.51945,
      -0.57471,-0.60747,-0.62594,-0.44832,-0.45212,-0.47283,
      -0.50951,-0.55388,-0.64309,-0.70603,-0.73772,-0.15835,
      -0.15840,-0.16626,-0.17878,-0.16314,-0.23119,-0.27807,-0.33159};
   
   static REAL cm[] = {-0.05410,-0.04057,-0.01404,0.01,0.01832,
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
   
   REAL alogt = log(tek_mod);
   
   find(del_mod,a,delta,(am+8*(m-1)),8);
   find(del_mod,b,delta,(bm+8*(m-1)),8);
   find(del_mod,c,delta,(cm+8*(m-1)),8);
   
   REAL sigmam = exp(a+alogt*(b+c*alogt));
   return(sigmam);
}

//////////////////////////////////////////////////////////////////////
REAL thermo::sigmad(const REAL& tek)
{
   
   // this works as the banff/glacier sigmad works - tested by DAS
   static REAL abc[] = {0.36934,-0.48595,0.02574,0.343,-0.44203,0.07549,0.09454,-0.17612,0.00272};
   
   REAL tek_mod = tek;
   if(tek_mod<0.3) tek_mod = 0.3f;
   if(tek_mod>100.0) tek_mod = 100.0f;
   
   int m = 1;
   
   if((1.55<=tek_mod)&&(tek_mod<7.0)) m = 2;
   if(tek_mod>7.0) m = 3;
   
   REAL alogt = log(tek_mod);
   REAL sigmad = exp(*(abc+3*(m-1))+alogt*(*(abc+3*(m-1)+1)+alogt*(*(abc+3*(m-1)+2))));
   
   return(sigmad);
}

///////////////////////////////////////////////////////////////////////
void thermo::find(const REAL& arg, REAL& ans, REAL* x, REAL* y, const int& npts)
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
////////////////////
void thermo::check_temp_range(const std::vector<REAL>& xspc, const REAL& temp)
{
   bool out_o_range = false;
   int isp, nsp = xspc.size();
   for(isp=0; isp<nsp; isp++){
      out_o_range = (xspc[isp]>1.0e-10&&(temp>tmax[isp]||temp<tmin[isp]));
      if(out_o_range) cout<< "Temp out of range for species " << spec_nam[isp]
         << " temp " << temp << " tmin " << tmin[isp] << " tmax " << tmax[isp] << endl;
   }
}

