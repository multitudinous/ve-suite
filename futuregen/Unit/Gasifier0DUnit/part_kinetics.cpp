#pragma warning(disable: 4786)

#include "part_kinetics.h"
#include <iostream>
#include <fstream>
#include <cmath>

using std::cout;
using std::endl;

//////////////////
part_kinetics::part_kinetics()
:model()
{
}
////////////////////////
part_kinetics::part_kinetics(std::vector<REAL>& typ_frac, std::vector< std::vector<REAL> >& adev0,
      std::vector< std::vector<REAL> >& edev0, std::vector< std::vector<REAL> >& ydev0,
      std::vector< std::vector<REAL> >& ahet0, std::vector< std::vector<REAL> >& nhet0,
      std::vector< std::vector<REAL> >& ehet0, std::vector< std::vector<REAL> >& nuhet0,
      std::vector< std::vector<std::string> >& oxyd0, std::vector<REAL>& part_den0, 
      std::vector< std::vector<REAL> >& part_dia0,
      std::vector< std::vector<REAL> >& size_frac0, std::vector<REAL>& omega_srf0,
      std::vector<REAL>& omega_char0, std::vector<REAL>& omega_liq0, 
      std::vector<REAL>& omega_ash0, REAL& temp_init, thermo& thm, stream& stm)
  : model     (),
    thm       (&thm),
    stm       (&stm),
    part_den  (part_den0),
    adev      (adev0),
    edev      (edev0),
    ydev      (ydev0),
    ahet      (ahet0),
    nhet      (nhet0),
    ehet      (ehet0),
    nuhet     (nuhet0),
    oxyd      (oxyd0),
    part_dia  (part_dia0),
    size_frac (size_frac0),
    type_frac (typ_frac),
    temp_init (temp_init),
    burnedout (false),
    desired_burnout (-1.0)
{
   REAL frac_daf=0, frac_water=0;
   int ibc = 1, istr;
   std::map< int, std::vector<int> >& bc_streams = stm.get_bc_streams();
   std::map< int, std::vector<REAL> >& frac_stream = stm.get_frac_stream();
   //std::map< int, bool >& bc_const_temp = stm.get_bc_const_temp_eql_m();
   std::map< int, std::vector<int> >::iterator itbcst;
   std::map< int, std::vector<REAL> >::iterator itfrst;
   itbcst = bc_streams.find(ibc);
   if(itbcst==bc_streams.end()) cout << "did not find bc_streams bc" << endl;
   itfrst = frac_stream.find(ibc);
   if(itfrst==frac_stream.end()) cout << "did not find frac_stream bc" << endl;
   int num = itfrst->second.size(), i;
   for(i=0; i<num; i++){
      istr = (*itbcst).second[i];
      int ist_num = stm.get_istreams()[istr];
      if(ist_num==2) frac_daf = itfrst->second[i];
      if(ist_num==3) frac_water = itfrst->second[i];
   }

   int nspc = thm.get_spec_nam().size();
   comp.resize(nspc); mol.resize(nspc);//diff.resize(nspc);
   REAL pi = 3.141592654, tot_part_mass, diam;
   int ntyp = part_dia.size(), it, ns, is;
   start_time.resize(ntyp,0.0);
   omega_srf.resize(ntyp);
   solid_raw_fuel.resize(ntyp);
   omega_char.resize(ntyp);
   Char.resize(ntyp);
   omega_liq.resize(ntyp);
   liq.resize(ntyp);
   omega_ash.resize(ntyp);
   ash.resize(ntyp);
   num_flow.resize(ntyp);
   mass_cp.resize(ntyp);
   diff.resize(ntyp);
   double ash_frac = 0.0;
   for(it=0; it<ntyp; it++){
     ash_frac += type_frac[it]*omega_ash0[it];
   }
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      mass_cp[it].resize(ns);
      diff[it].resize(ns);
      for(is=0; is<ns; is++){
         diam = part_dia[it][is];
         tot_part_mass = pi/6.0*diam*diam*diam*part_den[it];
         omega_srf[it].push_back(omega_srf0[it]);
         solid_raw_fuel[it].push_back(tot_part_mass*omega_srf0[it]);
         omega_char[it].push_back(omega_char0[it]);
         Char[it].push_back(tot_part_mass*omega_char0[it]);
         omega_liq[it].push_back(omega_liq0[it]);
         liq[it].push_back(tot_part_mass*omega_liq0[it]);
         omega_ash[it].push_back(omega_ash0[it]);
         ash[it].push_back(tot_part_mass*omega_ash0[it]);
         //if(omega_srf[it][is]+omega_char[it][is]+omega_ash[it][is]>0.0)
         num_flow[it].push_back(frac_daf/(1.0-ash_frac)*type_frac[it]*size_frac[it][is]/
            (tot_part_mass*(omega_srf[it][is]+omega_char[it][is]+omega_ash[it][is])));
         //else num_flow[it].push_back(frac_water*type_frac[it]*size_frac[it][is]
         //   /(tot_part_mass*omega_liq[it][is]));
	 diff[it][is].resize(nspc);
      }
   }
   initial_fuel = 0.0;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      for(is=0; is<ns; is++){
         initial_fuel += (solid_raw_fuel[it][is] + Char[it][is])*num_flow[it][is];
      }
   }
   //cout << "INITIAL_FUEL " << initial_fuel << endl;
   // determine boiling temperature
   REAL t1, t2, t3, hfg, vapp;
   t2 = 200.0;
   for(it=0; it<20; it++){
      t1 = t2;
      t2 = t1 + 100.0;
      vapp = equil::vap_pres_h2o(t2,hfg);
      if(vapp>stm.get_pres()) break;
   }
   do{
      t3 = 0.5*(t1 + t2);
      vapp = equil::vap_pres_h2o(t3,hfg);
      if(vapp>stm.get_pres()) t2 = t3;
      else t1 = t3;
   }while(fabs(t2-t1)>0.01);
   tboil = t3;
   //cout << "boiling temp " << tboil << " K" << endl;

   // calculate initial ash enthalpy
   ash_enth_init_1 = 0.0;
   for(it=0; it<ntyp; it++){
     ns = part_dia[it].size();
     for(is=0; is<ns; is++){
       ash_enth_init_1 += ash[it][is]*num_flow[it][is]*equil::enthalpy_ash(temp_init);
     }
   }
   ash_enth = ash_enth_init_1;
}
////////////////////////
void part_kinetics::initialize(REAL x, std::vector<REAL>& y0, std::vector<REAL>& yscal)
{
   int iy0 = 0;
   int ntyp = part_dia.size(), it, ns, is;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      for(is=0; is<ns; is++){
         y0.push_back(solid_raw_fuel[it][is]);
         yscal.push_back(solid_raw_fuel[it][is]);
         iy0++;
         y0.push_back(Char[it][is]);
         yscal.push_back((solid_raw_fuel[it][is]+Char[it][is])*10000.01);
         iy0++;
         y0.push_back(liq[it][is]);
         yscal.push_back(liq[it][is]*10000.01);
         iy0++;
         y0.push_back(temp_init);
         yscal.push_back(temp_init*10000.00);
         iy0++;
      }
   }
}
////////////////////////
void part_kinetics::derivs(REAL& x, std::vector<REAL>& y, std::vector<REAL>& dydx)
{
   int ntyp = part_dia.size(), it, ns, is, ndevrx, idevrx, nhetrx, ihetrx;
   REAL rpt, rhet, km, kr, krp, surf_area, rdev, rvol, sigma = 5.67e-8, time_interp;
   if(x<res_time1) temp_gas = temp_gas1;
   else{
     time_interp = pow(1.0 - (x-res_time1)/(res_time2-res_time1),3.0);
     temp_gas = temp_gas1 + (temp_gas2-temp_gas1)*(1.0 - time_interp);
   }
   REAL cg = stm->get_pres()/8314.0/temp_gas, mwc = 12.0, mwg = 29.0;
   REAL cop, nu = 2.0, pcop, rgas = 8314.0, rhog = cg*mwg, blow_mt, blow_fac;
   REAL qrad, rdry;
   std::map< std::string, int>::const_iterator itspc;
   int cnt=0, isp, ih2o;
   itspc = thm->get_nam_spec().find("H2O");
   ih2o = itspc->second;
   ash_enth = 0.0;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      ndevrx = adev[it].size();
      nhetrx = ahet[it].size();
      std::vector<REAL> rhet_i(nhetrx), cog(nhetrx);
      std::vector<int> ihetsp(nhetrx);
      for(is=0; is<ns; is++){
         surf_area = 3.141592654*part_dia[it][is]*part_dia[it][is];
         REAL& raw_fuel = y[cnt];
         REAL& solid_raw_fuel_dot = dydx[cnt];
         REAL& chaR = y[cnt+1];
         REAL& char_dot = dydx[cnt+1];
         REAL& temp = y[cnt+3];
         REAL& temp_dot = dydx[cnt+3];
	 ash_enth += ash[it][is]*num_flow[it][is]*equil::enthalpy_ash(temp);
        if(x<start_time[it]){
          solid_raw_fuel_dot = 0.0;
          char_dot = 0.0;
          temp_dot = 0.0;
          dydx[cnt+2] = 0.0;
	}else{
         solid_raw_fuel_dot = 0.0;
         //temp = temp_gas;
         //temp_dot = 0.0;
         if(temp<273.0) temp = 273.0;
         qrad = surf_area*sigma*(temp_gas*temp_gas*temp_gas*temp_gas - temp*temp*temp*temp);
         REAL hfg;
         REAL vapp = equil::vap_pres_h2o(temp,hfg);
         hfg *= thm->get_rgas();
         if(mass_cp[it][is]>0.0) temp_dot = qrad/mass_cp[it][is];
         else temp_dot = 0.0;
         char_dot = 0.0;
         rvol = 0.0;
         for(idevrx=0; idevrx<ndevrx; idevrx++){
            rdev = adev[it][idevrx]*exp(-edev[it][idevrx]/8314.0/temp)*raw_fuel;
            solid_raw_fuel_dot -= rdev;
            char_dot += (1.0 - ydev[it][idevrx])*rdev;
            rvol += ydev[it][idevrx]*rdev;
         }
         if(raw_fuel<=0.0){
            raw_fuel = 0.0;
            solid_raw_fuel_dot = 0.0;
         }
         rhet = 0.0;
         rdry = 0.0;
         for(ihetrx=0; ihetrx<nhetrx; ihetrx++){
            rhet_i[ihetrx] = 0.0;
            itspc = thm->get_nam_spec().find(oxyd[it][ihetrx]);
            isp = itspc->second;
            cog[ihetrx] = mol[isp]*cg;
            ihetsp[ihetrx] = isp;
         }
         for(int j=0; j<5; j++){
            rpt = rvol+rhet+rdry;
            rhet = 0.0;
            for(ihetrx=0; ihetrx<nhetrx; ihetrx++){
               km = nu*diff[it][is][ihetsp[ihetrx]]/part_dia[it][is];
               blow_fac = 1.0;
               blow_mt = rpt/(2.0*3.14159265*diff[it][is][ihetsp[ihetrx]]*rhog*part_dia[it][is]);
               if(blow_mt>1.0e-7){
                  if(blow_mt<80.0) blow_fac = exp(blow_mt) - 1.0;
                  else blow_fac = 1.0e25;
                  blow_fac = blow_mt/blow_fac;
               }
               km *= blow_fac;
               cop = mwg*cg*(km*surf_area*cog[ihetrx]*mwc*nuhet[it][ihetrx] - rhet_i[ihetrx])
                  /(mwc*nuhet[it][ihetrx]*(km*surf_area*mwg*cg+rpt));
               pcop = cop*rgas*temp;
               if(pcop<1.0e-30) pcop = (REAL)1.0e-30;
//           CONVERSION  [KgC/s m2 (Pa O2)n] --> [KgC/s m2 (Pa O2)]
               krp = ahet[it][ihetrx]*exp(-ehet[it][ihetrx]/rgas/temp);
               krp *= pow(pcop,nhet[it][ihetrx] - 1.0);
//           CONVERSION FROM [KgC/s m2 Pa O2] --> [m/s]
               kr = krp*temp*692.84/nuhet[it][ihetrx];
               rhet_i[ihetrx] = nuhet[it][ihetrx]*mwc*mwg*kr*km*surf_area*surf_area*
                  cog[ihetrx]*cg/((km+kr)*surf_area*mwg*cg+rpt);
               rhet += rhet_i[ihetrx];
            }
           // drying
           if(y[cnt+2]>0.0){
              REAL xl = vapp/stm->get_pres(), cg0 = stm->get_pres()/8.314/temp;
              if(xl>0.98&&y[cnt+2]>0.0){
                 temp = tboil;
                 temp_dot = 0.0;
                 rdry = qrad/hfg;
              }else{
                 km = nu*diff[it][is][ih2o]/part_dia[it][is];
                 blow_fac = 1.0;
                 blow_mt = rpt/(2.0*3.14159265*diff[it][is][ih2o]*rhog*part_dia[it][is]);
                 if(blow_mt>1.0e-7){
                    if(blow_mt<80.0) blow_fac = exp(blow_mt) - 1.0;
                    else blow_fac = 1.0e25;
                    blow_fac = blow_mt/blow_fac;
                 }
                 km *= blow_fac;
                 rdry = 18.01534*(km*surf_area*(cg0*xl-cg*mol[ih2o]) + xl*rpt/29.0);
              }
           } // if(y[cnt+2]>0.0
         }
         char_dot -= rhet;
         dydx[cnt+2] = -rdry;
         if(chaR<=0.0){
            chaR = 0.0;
            if(raw_fuel<=0.0) char_dot = 0.0;
         }
         if(y[cnt+2]<=0.0){
            y[cnt+2] = 0.0;
            dydx[cnt+2] = 0.0;
         }
        } // if(x<start_time[it]
         cnt += 4;
      } // for(is
   } // for(it
   if(x<res_time1) ash_enth_init_2 = ash_enth;
}
////////////////////////
void part_kinetics::update(REAL& x, REAL& x2, REAL& x0, std::vector<REAL>& y, std::vector<REAL>& dydx)
{
   int ntyp = part_dia.size(), it, ns, is;
   int cnt=0;
   REAL daf_flow = 0.0, fuel_remaining = 0.0, sum = 0.0;
   dp_mean = 0.0;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      for(is=0; is<ns; is++){
         REAL& raw_fuel = y[cnt];
         REAL& chaR = y[cnt+1];
         dp_mean += (raw_fuel + chaR + ash[it][is])*num_flow[it][is]*part_dia[it][is];
         sum += (raw_fuel + chaR + ash[it][is])*num_flow[it][is];
         fuel_remaining += (raw_fuel + chaR)*num_flow[it][is];
         cnt += 4;
      }
   }
   dp_mean /= sum;
   burnout = 1.0 - fuel_remaining/initial_fuel;
   //cout << " t " << x << " burnout " << burnout << endl;
   if(desired_burnout>0.0){
     if(burnout>=desired_burnout) x2 = x;
   }
   daf_flow = initial_fuel - fuel_remaining;
   dp_var = 0.0;
   cnt = 0;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      for(is=0; is<ns; is++){
         REAL& raw_fuel = y[cnt];
         REAL& chaR = y[cnt+1];
         dp_var += (raw_fuel + chaR + ash[it][is])*num_flow[it][is]
          *(part_dia[it][is] - dp_mean)*(part_dia[it][is] - dp_mean);
         cnt += 4;
      }
   }
   dp_var /= sum;
   int nspc = thm->get_spec_nam().size();
   if(!comp_table.size()){
     int ntable = 10;
     for(it=0; it<=ntable; it++){
       REAL burnout0 = REAL(it)/REAL(ntable);
       stream stm2 = *stm;
       std::map< int, std::vector<int> >& bc_streams = stm2.get_bc_streams();
       std::map< int, std::vector<REAL> >& frac_stream = stm2.get_frac_stream();
       std::map< int, std::vector<REAL> >& frac_stream1 = stm->get_frac_stream();
       std::map< int, bool >& bc_const_temp = stm2.get_bc_const_temp_eql_m();
       std::map< int, std::vector<int> >::iterator itbcst;
       std::map< int, std::vector<REAL> >::iterator itfrst, itfrst1;
       std::map< int, std::vector<REAL> >::iterator ittmp;
       std::map< int, bool >::iterator itconst;
       for(itbcst=bc_streams.begin(), itfrst=frac_stream.begin(),
	    itfrst1=frac_stream1.begin(),
	    ittmp = stm2.get_bc_temp().begin(),itconst=bc_const_temp.begin();
	  itbcst!=bc_streams.end();
	  itbcst++,itfrst++,itfrst1++,ittmp++,itconst++){
         itconst->second = true;
         int num = itfrst->second.size(), i, ist_num, istr;
         REAL sum = 0.0;
         for(i=0; i<num; i++){
            (*ittmp).second[i] = temp_gas;
            istr = (*itbcst).second[i];
            ist_num = stm2.get_istreams()[istr];
            if(ist_num==2) itfrst->second[i] = itfrst1->second[i]*burnout0;
            sum += (*itfrst).second[i];
         }
         for(i=0; i<num; i++){
            itfrst->second[i] /= sum;
         }
       }
      
       REAL qht = 0.0, mdot = 1.0;
       equil::equilibrium(*thm, stm2, qht, 1.0, mdot, temp_gas, comp, mol);
       comp_table.push_back(mol);
     } // for(it
   } // if(!comp_table.size())
   int ntable = comp_table.size(), itable = burnout*REAL(ntable);
   itable = burnout*REAL(ntable-1);
   if(itable==ntable-1) itable--;
   REAL frac = burnout - REAL(itable);
   for(is=0; is<nspc; is++){ 
     mol[is] = (1.0 - frac)*comp_table[itable][is] + frac*comp_table[itable+1][is];
   }
   cnt = 0;
   for(it=0; it<ntyp; it++){
     ns = part_dia[it].size();
     for(is=0; is<ns; is++){
       //REAL& raw_fuel = y[cnt];
       //REAL& chaR = y[cnt+1];
       REAL& temp = y[cnt+3];
       diff[it][is] = thm->get_diffus(temp_gas, temp, mol, stm->get_pres());
       cnt += 4;
     }
   }
   if(burnout==1.0) burnedout = true;
   cnt = 0;
   for(it=0; it<ntyp; it++){
      ns = part_dia[it].size();
      for(is=0; is<ns; is++){
         REAL& raw_fuel = y[cnt];
         REAL& chaR = y[cnt+1];
         REAL& temp = y[cnt+3];
         REAL& mois = y[cnt+2];
         mass_cp[it][is] = (raw_fuel+chaR)*equil::cp_coal(temp)*thm->get_rgas()
	   + ash[it][is]*equil::cp_ash(temp) + mois*equil::cp_h2o_l(temp)*thm->get_rgas();
         //if(mass_cp[it][is]<1.0e-20) mass_cp[it][is] = 1.0e-20;
	 cnt += 4;
      }
   }
}
//////////////////////////
void part_kinetics::push_back(REAL& x, std::vector<REAL>& y,
      std::vector<REAL>& dydx)
{
   //cout << x << " " << dydx[0] << endl;
}
//////////////////////////
void part_kinetics::output(std::vector<REAL>& xp, std::vector< std::vector<REAL> >& yp, char* dir)
{
   int nts = xp.size(), i;
   for(i=0; i<nts; i++){
      cout << xp[i] << " " << yp[0][i] << " " << yp[1][i] << endl;
   }
   std::ofstream df("out");
   df << "time,s" << " coal" <<" char" << endl;
   for(i=0; i<nts; i++){
      df << xp[i];
      df << " " << yp[0][i] << " " << yp[1][i] << endl;
   } // for(i
   cout << "burnout " << burnout << endl;
   df.close();
}
