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
// implementation file for the stream class

#pragma warning(disable: 4786)
#include <cstdio>
#include <cstring>
#include "stream.h"
#include <cmath>

using std::cout;
using std::endl;

//   constructor
stream::stream(const thermo& thm, const string stream_file)
{
    read_stream(thm, stream_file);
}
////////////////////////////////
stream::stream(const thermo& thm, REAL& pres0,  std::vector<int>& istream0,
      std::vector<std::string>& frac_typ, std::vector<std::string>& fuel0,
      std::vector< std::vector<std::string> >& spc_nam, 
      std::vector< std::vector<REAL> >& frac, std::vector<REAL>& hform,
	  std::vector<int>& istream1, std::vector<REAL>& str_frac,
	  std::vector<REAL>& temp0)
{

    const std::map< std::string, int >& nam_spec = thm.get_nam_spec();
    const std::map< std::string, int >& nam_el = thm.get_nam_el();

    //cout << endl;

    std::map< std::string, int >::const_iterator iter;
    std::map< int, int >::iterator itstr;
    std::map< int, std::vector<int> >::iterator itbc;
    std::string name;
    //cout << "stream definition input" << endl;
    pres = pres0;
    //cout << "pressure " << pres << endl;
    int cnt = 0, istream = 0, errcnt = 0, ibc, num;
    int isp, nstr = frac_typ.size();
    for(cnt=0; cnt<nstr; cnt++){
//       if(!strcmp(nam,"stream")) {
       bool mol = frac_typ[cnt]=="mole";
       bool lelm = frac_typ[cnt]=="elem";
       bool fl = fuel0[cnt]=="fuel";
       fuel.push_back(fl);
       istream = istream0[cnt];
       //cout << "stream " << istream << endl;
       std::vector<int> ispec, iel;
       std::vector<REAL> frc;
       std::vector<REAL> molfrc;
       num = spc_nam[cnt].size();
       ispec.resize(num); iel.resize(num);
       frc.resize(num);
       molfrc.resize(num);
       for(isp=0; isp<num; isp++){
          name = spc_nam[cnt][isp];
          if(lelm){
             iter = nam_el.find(name);
             if(iter!=nam_el.end()){
                iel[isp] = (*iter).second;
                frc[isp] = frac[cnt][isp];
                //cout << (*iter).first << " " << frac[cnt][isp] << endl;
             }else{
                errcnt++;
                //cout << "element " << name << " not found" << endl;
             }
          }else{ // if(lelm
             iter = nam_spec.find(name);
             if(iter!=nam_spec.end()){
                ispec[isp] = (*iter).second;
                frc[isp] = frac[cnt][isp];
                //cout << (*iter).first << " " << frac[cnt][isp] << endl;
             }else{
                errcnt++;
                //cout << "species " << name << " not found" << endl;
             }
          }
       }
       // check that fractions sum to one
       REAL sum = 0.0;
       for(isp=0; isp<num; isp++) sum += frc[isp];
       if(fabs(sum-(REAL)1.0)>1.0e-5){
          //cout << "fractions sum to " << sum << " renormalizing" << endl;
          for(isp=0; isp<num; isp++) frc[isp] /= sum;
       }
       if(lelm){
          hc0[istream] = hform[cnt];
          //cout << "heat of formation " << hform[cnt] << endl;
          speciate(thm,num,errcnt,ispec,iel,frc);
          molfrc.resize(num);
       } // if(lelm
       // calculate mole or mass fraction
       sum = 0.0;
       for(isp=0; isp<num; isp++){
          if(mol){
             molfrc[isp] = frc[isp];
             frc[isp] = molfrc[isp]*thm.get_mwt()[ispec[isp]];
             sum += frc[isp];
          }else{
             molfrc[isp] = frc[isp]/thm.get_mwt()[ispec[isp]];
             sum += molfrc[isp];
          }
       }
       if(mol){
          //cout << " mass fractions ";
       }else{
          //cout << " mole fractions";
       }
       for(isp=0; isp<num; isp++) {
          if(mol){
             frc[isp] /= sum;
             //cout << " " << frc[isp];
          }else{
             molfrc[isp] /= sum;
             //cout << " " << molfrc[isp];
          }
       }
       //cout << endl;
       // calculate atomic composition of stream
       int nel = thm.get_nel(), ie;
       std::vector<REAL> mol_at;
       const std::vector< std::vector<REAL> >& mol_at_spc = thm.get_mol_at_spc();
       mol_at.resize(nel);
       for(ie=0; ie<nel; ie++) mol_at[ie] = 0.0;
       for(isp=0; isp<num; isp++){
          int is = ispec[isp];
          for(ie=0; ie<nel; ie++){
             mol_at[ie] += mol_at_spc[ie][is]*frc[isp]/thm.get_mwt()[is];
          }
       }
       std::vector<REAL> frac_at;
       frac_at.resize(nel);
       for(ie=0; ie<nel; ie++){
          frac_at[ie] = mol_at[ie]*thm.get_atwt()[ie];
         // if(lelm&&frac_at[ie]>0.0) //cout << thm.get_el_nam()[ie] << " " << frac_at[ie] << endl;
       }
       // push back the vectors
       istreams.push_back(istream);
       species.push_back(ispec);
       frac_spec.push_back(frc);
       molfrac_spec.push_back(molfrc);
       mol_atom.push_back(mol_at);
       frac_atom.push_back(frac_at);
       lelem.push_back(lelm);
       if(mol){
          mwt.push_back(sum);
          //cout << " mixture mol wt " << sum << endl;
       }else{
          mwt.push_back(1.0/sum);
          //cout << " mixture mol wt " << 1.0/sum << endl;
       }
       sum = 0.0;
       //cout << " wt fraction of elements";
       for(ie=0; ie<nel; ie++){
          sum += frac_at[ie];
          //cout << " " << frac_at[ie];
       }
       //cout << " total " << sum << endl;
       num = species.size() - 1;
       itstr = streams.find(istream);
       if(itstr==streams.end()){
          streams[istream] = num;
       }else{
          errcnt++;
          //cout << "stream number already used" << endl;
       }
    } // for(cnt       
//       }else if(!strcmp(nam,"bc")){
    ibc = 1;
    bc_const_temp_eql[ibc] = false;
	 //cout << "bc " << ibc << endl;
    itbc = bc_streams.find(ibc);
	 if(itbc!=bc_streams.end()){
       errcnt++;
       //cout << "bc # already used for streams" << endl;
    }
 //   bc_temp[ibc] = temp0[0];
    int istr;
    num = str_frac.size();
    for(istr=0; istr<num; istr++){
       istream = istream1[istr];
       itstr = streams.find(istream);
       if(itstr!=streams.end()){
          bc_streams[ibc].push_back((*itstr).second);
          bc_istreams[ibc][(*itstr).second] = istr;
          frac_stream[ibc].push_back(str_frac[istr]);
          bc_temp[ibc].push_back(temp0[istr]);
          //cout << " stream " << istream << " " << str_frac[istr] << endl;
       }else{
          //cout << "stream " << istream << " not found" << endl;
          errcnt++;
       }
    }
    // check that fractions sum to one
    REAL sum = 0.0;
    for(istr=0; istr<num; istr++) sum += frac_stream[ibc][istr];
    if(fabs(sum-(REAL)1.0)>1.0e-5){
       //cout << "fractions sum to " << sum << " renormalizing" << endl;
       for(istr=0; istr<num; istr++) frac_stream[ibc][istr] /= sum;
    }
    if(errcnt) exit(0);

                   
}
////////////////////////////////
void stream::read_stream(const thermo& thm, const string& stream_file)
{
    FILE *s1;

    char nam[20], nam1[20];
    const std::map< std::string, int >& nam_spec = thm.get_nam_spec();
    const std::map< std::string, int >& nam_el = thm.get_nam_el();

    //cout << endl;
    // open file
    if((s1=fopen(stream_file.c_str(),"rt"))==NULL){
        fprintf(stderr, "Fail to open stream file\n");
        exit(0);
        return;
    }

    char str[10], str1[10], str2[10];
    REAL frac;
    std::map< std::string, int >::const_iterator iter;
    std::map< int, int >::iterator itstr;
    std::map< int, std::vector<int> >::iterator itbc;
    std::string name;
    strcpy(str,"%s ");
    strcat(str,FSTR);
    strcpy(str1,"%d ");
    strcat(str1,FSTR);
    strcat(str1," ");
    strcat(str1,FSTR);
    strcpy(str2,"%d %d");
    //strcat(str2,FSTR);
    fscanf(s1,FSTR,&pres);
    //cout << "stream definition input" << endl;
    //cout << "pressure " << pres << endl;
    int cnt = 0, istream = 0, errcnt = 0, ibc, num;
    do {
       cnt++;
       nam[0] = '\0';
       fscanf(s1,"%s",&nam);
       int isp;
       if(!strcmp(nam,"stream")) {
          fscanf(s1,"%d %d %s %s",&istream,&num,&nam,&nam1);
          bool mol = !strcmp(nam,"mole");
          bool lelm = !strcmp(nam,"elem");
          bool fl = !strcmp(nam1,"fuel");
          fuel.push_back(fl);
          //cout << "stream " << istream << endl;
          std::vector<int> ispec, iel;
          std::vector<REAL> frc;
          std::vector<REAL> molfrc;
          ispec.resize(num); iel.resize(num);
          frc.resize(num);
          molfrc.resize(num);
          for(isp=0; isp<num; isp++){
             fscanf(s1,str,&nam,&frac);
             name = nam;
             if(lelm){
                iter = nam_el.find(name);
                if(iter!=nam_el.end()){
                   iel[isp] = (*iter).second;
                   frc[isp] = frac;
                   //cout << (*iter).first << " " << frac << endl;
                }else{
                   errcnt++;
                   //cout << "element " << name << " not found" << endl;
                }
             }else{ // if(lelm
                iter = nam_spec.find(name);
                if(iter!=nam_spec.end()){
                   ispec[isp] = (*iter).second;
                   frc[isp] = frac;
                   //cout << (*iter).first << " " << frac << endl;
                }else{
                   errcnt++;
                   //cout << "species " << name << " not found" << endl;
                }
             }
          }
          // check that fractions sum to one
          REAL sum = 0.0;
          for(isp=0; isp<num; isp++) sum += frc[isp];
          if(fabs(sum-(REAL)1.0)>1.0e-5){
             //cout << "fractions sum to " << sum << " renormalizing" << endl;
             for(isp=0; isp<num; isp++) frc[isp] /= sum;
          }
          if(lelm){
             REAL hform;
             fscanf(s1,FSTR,&hform);
             hc0[istream] = hform;
             //cout << "heat of formation " << hform << endl;
             speciate(thm,num,errcnt,ispec,iel,frc);
             molfrc.resize(num);
          } // if(lelm
          // calculate mole or mass fraction
          sum = 0.0;
          for(isp=0; isp<num; isp++){
             if(mol){
                molfrc[isp] = frc[isp];
                frc[isp] = molfrc[isp]*thm.get_mwt()[ispec[isp]];
                sum += frc[isp];
             }else{
                molfrc[isp] = frc[isp]/thm.get_mwt()[ispec[isp]];
                sum += molfrc[isp];
             }
          }
          if(mol){
             //cout << " mass fractions ";
          }else{
             //cout << " mole fractions";
          }
          for(isp=0; isp<num; isp++) {
             if(mol){
                frc[isp] /= sum;
                //cout << " " << frc[isp];
             }else{
                molfrc[isp] /= sum;
                //cout << " " << molfrc[isp];
             }
          }
          //cout << endl;
          // calculate atomic composition of stream
          int nel = thm.get_nel(), ie;
          std::vector<REAL> mol_at;
          const std::vector< std::vector<REAL> >& mol_at_spc = thm.get_mol_at_spc();
          mol_at.resize(nel);
          for(ie=0; ie<nel; ie++) mol_at[ie] = 0.0;
          for(isp=0; isp<num; isp++){
             int is = ispec[isp];
             for(ie=0; ie<nel; ie++){
                mol_at[ie] += mol_at_spc[ie][is]*frc[isp]/thm.get_mwt()[is];
             }
          }
          std::vector<REAL> frac_at;
          frac_at.resize(nel);
          for(ie=0; ie<nel; ie++){
             frac_at[ie] = mol_at[ie]*thm.get_atwt()[ie];
             //if(lelm&&frac_at[ie]>0.0) //cout << thm.get_el_nam()[ie] << " " << frac_at[ie] << endl;
          }
          // push back the vectors
          istreams.push_back(istream);
          species.push_back(ispec);
          frac_spec.push_back(frc);
          molfrac_spec.push_back(molfrc);
          mol_atom.push_back(mol_at);
          frac_atom.push_back(frac_at);
          lelem.push_back(lelm);
          if(mol){
             mwt.push_back(sum);
             //cout << " mixture mol wt " << sum << endl;
          }else{
             mwt.push_back(1.0/sum);
             //cout << " mixture mol wt " << 1.0/sum << endl;
          }
          sum = 0.0;
          //cout << " wt fraction of elements";
          for(ie=0; ie<nel; ie++){
             sum += frac_at[ie];
             //cout << " " << frac_at[ie];
          }
          //cout << " total " << sum << endl;
          num = species.size() - 1;
          itstr = streams.find(istream);
          if(itstr==streams.end()){
             streams[istream] = num;
          }else{
             errcnt++;
             //cout << "stream number already used" << endl;
          }
       }else if(!strcmp(nam,"bc")){
          REAL tmp;
          fscanf(s1,"%d %d %s",&ibc,&num,&nam);
          //cout << "bc " << ibc << endl;
          itbc = bc_streams.find(ibc);
          if(itbc!=bc_streams.end()){
             errcnt++;
             //cout << "bc # already used for streams" << endl;
          }
          if(!strcmp(nam,"const")) bc_const_temp_eql[ibc] = true;
          else bc_const_temp_eql[ibc] = false;
          //bc_temp[ibc] = tmp;
          int istr;
          for(istr=0; istr<num; istr++){
             fscanf(s1,str1,&istream,&frac,&tmp);
             itstr = streams.find(istream);
             if(itstr!=streams.end()){
                bc_streams[ibc].push_back((*itstr).second);
                bc_istreams[ibc][(*itstr).second] = istr;
                frac_stream[ibc].push_back(frac);
                bc_temp[ibc].push_back(tmp);
                //cout << " stream " << istream << " " << frac << " temp " << tmp << endl;
             }else{
                //cout << "stream " << istream << " not found" << endl;
                errcnt++;
             }
          }
          // check that fractions sum to one
          REAL sum = 0.0;
          for(istr=0; istr<num; istr++) sum += frac_stream[ibc][istr];
          if(fabs(sum-(REAL)1.0)>1.0e-5){
             //cout << "fractions sum to " << sum << " renormalizing" << endl;
             for(istr=0; istr<num; istr++) frac_stream[ibc][istr] /= sum;
          }
          // calculate bc density
          std::vector<REAL> mol;
          int nspc = thm.get_mwt().size(), is, is0;
          mol.resize(nspc);
          for(is=0; is<nspc; is++) mol[is] = 0.0;
          for(istr=0; istr<num; istr++){
             int istr1 = bc_streams[ibc][istr];
             int num1 = species[istr1].size();
             for(is0=0; is0<num1; is0++){
                is = species[istr1][is0];
                mol[is] += frac_stream[ibc][istr]*frac_spec[istr1][is0];
             }
          }
          REAL inv_mwt=0.0;
          for(is=0; is<nspc; is++){
             inv_mwt += mol[is]/thm.get_mwt()[is];
          }
          bc_dens[ibc] = pres/8314.0/tmp/inv_mwt;
          //cout << " density " << bc_dens[ibc] << endl;
       }else {
          break;
       }
    } while (cnt < 500);
    if(errcnt) exit(0);

    fclose(s1);
                   
}
///////////////////////
void stream::speciate(const thermo& thm, int& num, int& errcnt,
      std::vector<int>& ispec, std::vector<int>& iel,
      std::vector<REAL>& frc)
{
   std::string name;
   const std::map< std::string, int >& nam_spec = thm.get_nam_spec();
   std::map< std::string, int >::const_iterator iter;
   std::vector<int> i_sp;
   std::vector<REAL> mol_el, mol_sp;
   i_sp.resize(3*num);
   mol_el.resize(3*num); mol_sp.resize(3*num);
   int isp;
   int id_C = -1, id_H = -1, id_O = -1, id_N = -1, id_S = -1;
   int id_CL = -1, id_HG = -1;
   for(isp=0; isp<num; isp++) {
      mol_el[isp] = frc[isp]/thm.get_atwt()[iel[isp]];
   }
   for(isp=0; isp<num; isp++){
      if(thm.get_el_nam()[iel[isp]]=="C") id_C = isp;
      if(thm.get_el_nam()[iel[isp]]=="H") id_H = isp;
      if(thm.get_el_nam()[iel[isp]]=="O") id_O = isp;
      if(thm.get_el_nam()[iel[isp]]=="N") id_N = isp;
      if(thm.get_el_nam()[iel[isp]]=="S") id_S = isp;
      if(thm.get_el_nam()[iel[isp]]=="CL") id_CL = isp;
      if(thm.get_el_nam()[iel[isp]]=="HG") id_HG = isp;
   }
   isp = -1;
   if(id_CL>-1){
      if(id_H>-1){
         if(mol_el[id_H]>=mol_el[id_CL]){
            name = "HCL";
            iter = nam_spec.find(name);
            if(iter!=nam_spec.end()){
               isp++;
               i_sp[isp] = (*iter).second;
            }else{
               errcnt++;
               //cout << "species " << name << " not found" << endl;
            }
            mol_sp[isp] = mol_el[id_CL];
            mol_el[id_H] -= mol_el[id_CL];
            mol_el[id_CL] = 0.0;
         }else{
            errcnt++;
            //cout << "insufficient H to speciate CL" << endl;
         }
      }else{ // if(ic_H>-1
         errcnt++;
         //cout << "need H to speciate CL" << endl;
      }
   }
   if(id_S>-1){
      if(id_O>-1&&id_H>-1){
         name = "SO2";
         iter = nam_spec.find(name);
         if(iter!=nam_spec.end()){
            isp++;
            i_sp[isp] = (*iter).second;
         }else{
            errcnt++;
            //cout << "species " << name << " not found" << endl;
         }
         if(mol_el[id_O]>=2.0*mol_el[id_S]){
            mol_el[id_O] -= 2.0*mol_el[id_S];
            mol_sp[isp] = mol_el[id_S];
            mol_el[id_S] = 0.0;
         }else{
            mol_sp[isp] = mol_el[id_O]*0.5;
            mol_el[id_S] -= mol_el[id_O]*0.5;
            mol_el[id_O] = 0.0;
            name = "H2S";
            iter = nam_spec.find(name);
            if(iter!=nam_spec.end()){
               isp++;
               i_sp[isp] = (*iter).second;
            }else{
               errcnt++;
               //cout << "species " << name << " not found" << endl;
            }
            if(mol_el[id_H]>=mol_el[id_S]*0.5){
               mol_sp[isp] = mol_el[id_S];
               mol_el[id_H] -= mol_el[id_S]*2.0;
               mol_el[id_S] = 0.0;
            }else{
               errcnt++;
               //cout << "insufficient H for H2S" << endl;
            }
         } // if(mol_el[id_O
      }else{
         errcnt++;
         //cout << "need O and H to speciate S" << endl;
      }
   } // if(id_S>-1
   if(id_C>-1){
      if(id_O>-1){
	name = "CO";
	iter = nam_spec.find(name);
	if(iter!=nam_spec.end()){
	  isp++;
	  i_sp[isp] = (*iter).second;
	}else{
	  errcnt++;
	  //cout << "species " << name << " not found" << endl;
	}
	if(mol_el[id_O] >= mol_el[id_C]){
	  mol_sp[isp] = mol_el[id_C];
	  mol_el[id_O] -= mol_el[id_C];
	  mol_el[id_C] = 0.0;
	}else{
	  mol_sp[isp] = mol_el[id_O];
	  mol_el[id_C] -= mol_el[id_O];
	  mol_el[id_O] = 0.0;
	}
      } // if(id_O>-1
      if(mol_el[id_C]>0.0){
         name = "C(GR)";
         iter = nam_spec.find(name);
         if(iter!=nam_spec.end()){
            isp++;
            i_sp[isp] = (*iter).second;
         }else{
            errcnt++;
            //cout << "species " << name << " not found" << endl;
         }
         mol_sp[isp] = mol_el[id_C];
         mol_el[id_C] = 0.0;
      }
   } // if(id_C>-1
   if(id_H>-1){
      if(mol_el[id_H]>0.0){
         name = "H2";
         iter = nam_spec.find(name);
         if(iter!=nam_spec.end()){
            isp++;
            i_sp[isp] = (*iter).second;
         }else{
            errcnt++;
            //cout << "species " << name << " not found" << endl;
         }
         mol_sp[isp] = mol_el[id_H]*0.5;
         mol_el[id_H] = 0.0;
      }
   }
   if(id_O>-1){
      if(mol_el[id_O]>0.0){
         name = "O2";
         iter = nam_spec.find(name);
         if(iter!=nam_spec.end()){
            isp++;
            i_sp[isp] = (*iter).second;
         }else{
            errcnt++;
            //cout << "species " << name << " not found" << endl;
         }
         mol_sp[isp] = mol_el[id_O]*0.5;
         mol_el[id_O] = 0.0;
      }
   }
   if(id_N>-1){
      name = "N2";
      iter = nam_spec.find(name);
      if(iter!=nam_spec.end()){
         isp++;
         i_sp[isp] = (*iter).second;
      }else{
         errcnt++;
         //cout << "species " << name << " not found" << endl;
      }
      mol_sp[isp] = mol_el[id_N]*0.5;
      mol_el[id_N] = 0.0;
   }
   if(id_HG>-1){
      name = "HG";
      iter = nam_spec.find(name);
      if(iter!=nam_spec.end()){
         isp++;
         i_sp[isp] = (*iter).second;
      }else{
         errcnt++;
         //cout << "species " << name << " not found" << endl;
      }
      mol_sp[isp] = mol_el[id_N];
      mol_el[id_N] = 0.0;
   }
   num = isp+1;
   ispec.resize(num);
   frc.resize(num);
   //cout << "speciation " << endl;
   for(isp=0; isp<num; isp++){
      ispec[isp] = i_sp[isp];
      frc[isp] = mol_sp[isp]*thm.get_mwt()[ispec[isp]];
      //cout << thm.get_spec_nam()[ispec[isp]] << " " << frc[isp] << endl;
   }
}

