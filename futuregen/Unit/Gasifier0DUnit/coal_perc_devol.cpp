/*C  THIS IS THE CPD MODEL
C
C  THIS MODEL WAS DEVELOPED BY SANDIA NATIONAL LABORATORIES UNDER 
C  FWP 0709 FOR THE DEPARTMENT OF ENERGY'S PITTSBURGH ENERGY
C  TECHNOLOGY CENTER AND THE DOE DIVISION OF ENGINEERING AND GEOSCIENCES
C  THROUGH THE OFFICE OF BASIC ENERGY SCIENCES;
C  AND BY THE UNIVERSITY OF UTAH THROUGH FUNDING FROM 
C  THE ADVANCED COMBUSTION ENGINEERING RESEARCH CENTER (ACERC), WHICH 
C  IS PRINCIPALLY SPONSORED BY THE NATIONAL SCIENCE FOUNDATION, THE
C  STATE OF UTAH, AND BY A CONSORTIUM OF INDUSTRIAL COMPANIES.
C  THE CODE WILL NOT BE FORMALLY LICENSED.  NEITHER THE U.S. OR THE 
C  DOE, NOR ANY OF THEIR EMPLOYEES, MAKES ANY WARRANTY, EXPRESS OR IMPLIED, 
C  OR ASSUMES ANY LEGAL LIABILITY OR RESPONSIBILITY FOR THE ACCURACY, 
C  COMPLETENESS, OR USEFULNESS OF ANY INFORMATION, APPARATUS, PRODUCT, OR 
C  PROCESS DISCLOSED, OR REPRESENTS THAT ITS USE WOULD INFRINGE PRIVATELY 
C  OWNED RIGHTS.

c
c  The CPD model is intended to solve pyrolysis rate equations
c  based on a percolative bond-breaking scheme.  This version includes the 
c  flash distillation program to distinguish between tar and metaplast.
c  This program also includes a crosslinking scheme.  (January, 1991)*/
#include <cstdlib>
#include <cstdio>
#include <iostream>
#include <cmath>
#include "coal_perc_devol.h"

using namespace std;

////////////////////////
   coal_perc_devol::coal_perc_devol(std::string filenam)
   {
    FILE *s1;
    // open file
    if((s1=fopen(filenam.c_str(),"rt"))==NULL){
        fprintf(stderr, "Fail to open cpd_input file");
        exit(0);
    }

/*c  read in measured p0 from NMR data
      read(1,*)p0
c  estimate c0
      read(1,*)c0
c  read in sigma+1 (coordination number) from NMR data
      read(1,*)sigp1
c  read in real MW/Cluster from NMR data (includes side chains)
      read(1,*)mw1
c  read in real mdel from NMR data
      read(1,*)mdel		
c  kinetic parameters
      read(1,*)ab
      read(1,*)eb0
      read(1,*)ebsig
      read(1,*)ac
      read(1,*)ec0
      read(1,*)ag
      read(1,*)eg0
      read(1,*)egsig
      read(1,*)acr
      read(1,*)ecr
c  pressure in atmospheres
      read(1,*)press */

    char line[200];

    fscanf(s1,FSTR,&p0);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&c0);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&sigp1);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&mw1);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&mdel);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ab);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&eb0);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ebsig);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ac);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ec0);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ag);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&eg0);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&egsig);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&acr);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&ecr);
    fscanf(s1,"%[^\n]",line);

    fscanf(s1,FSTR,&press);
    fscanf(s1,"%[^\n]",line);
    fclose(s1);

    init();

   }
   //////////////////////
   void coal_perc_devol::init()
   {
//c  empirical correlation to allow a small portion of alpha-carbon to stay with
//c  the aromatic cluster
      mdel = mdel-7;
//c  now calculate other chemical structure coefficients
      l0 = p0 - c0;
      mb = 2.*mdel;
      ma = mw1-sigp1*mdel;
      sig = sigp1-1;
      finf = 1./(2.*ma/(mb*sigp1*(1-c0))+1);
      beta = ma;
      rba = mb/ma;
//      print*,'rba=',rba
      rg = (REAL)1.987;

      tim.resize(50); tem.resize(50); metold.resize(35,0.0); ftold.resize(35,0.0);
      tarold.resize(50,0.0);
      /*k.resize(36,0.0); l.resize(36,0.0); f.resize(36,0.0), xmw.resize(36,0.0);
      z.resize(36,0.0); pv.resize(36,0.0); x.resize(36,0.0); y.resize(36,0.0);
      v.resize(36,0.0);*/
      fgas0 = 0.0;
      iprint = 0;
   }
   //////////////////////////////
   coal_perc_devol::coal_perc_devol(REAL carbon, REAL hydrogen, REAL oxygen,
      REAL volm, REAL pressure)
   {
      mdel = 421.957 - 8.64692*carbon + 0.0463894*carbon*carbon
         - 8.47272*hydrogen + 1.18173*hydrogen*hydrogen
         + 1.15366*oxygen - 0.0434024*oxygen*oxygen
         + 0.556772*volm - 0.00654575*volm*volm;

      mw1 = 1301.41 + 16.3879*carbon - 0.187493*carbon*carbon
         - 454.773*hydrogen + 51.7109*hydrogen*hydrogen
         - 10.072*oxygen + 0.0760827*oxygen*oxygen
         + 1.36022*volm - 0.0313561*volm*volm;

      p0 = 0.489809 - 0.00981566*carbon + 0.000133046*carbon*carbon
         + 0.155483*hydrogen - 0.0243873*hydrogen*hydrogen
         + 0.00705248*oxygen + 0.000219163*oxygen*oxygen
         - 0.0110498*volm + 0.000100939*volm*volm;

      sigp1 = -52.1054 + 1.63872*carbon - 0.0107548*carbon*carbon
         - 1.23688*hydrogen + 0.0931937*hydrogen*hydrogen
         - 0.165673*oxygen + 0.00409556*oxygen*oxygen
         + 0.00926097*volm - 8.26717E-05*volm*volm;

/*CPD model calculations also require values for c0, the fraction of stable bridges. The correlation for c0 that seems to work best is as follows:

For %C > 85.9% (daf), c0 = 0.1183*%C - 10.16, with a maximum value of 0.36.
For %O > 12.5% (daf), c0 = 0.014*%O - 0.175, with a maximum value of 0.15.

Otherwise, c0 = 0.0.*/

      if(carbon>85.9){
         c0 = 0.1183*carbon - 10.16;
         if(c0>0.36) c0 = 0.36;
      }else{
         c0 = 0.014*oxygen - 0.175;
         if(c0>0.15) c0 = 0.15;
         if(c0<0.0) c0 = 0.0;
      }
      
      ab = 2.602e15;
      eb0 = 55400.;
      ebsig = 1800.;
      ac = 0.9;
      ec0 = 0.;
      ag = 3.e15;
      eg0 = 69000.;
      egsig = 8100.;
      acr = 3.e15;//(pre-exponential factor for crosslinking rate)
      ecr = 65000.; //(Activation energy for crosslinking rate)
      press = pressure/101325.0; // pressure (atm)

      init();

   }
   //////////////////////////////
   void coal_perc_devol::yield(std::vector<REAL>& tim, std::vector<REAL>& tem,
       FILE *s2, FILE *s3, REAL dt0, REAL dtmax, int iprint, REAL timax,
       int nmax0, std::vector<REAL>& ytar, std::vector<REAL>& ygas,
       std::vector<REAL>& ytime)
   {
//      program cpd
//      IMPLICIT real*4 (A-H,O-Z)
//c      save
//c  Input parameters are found in PERKIN
//c  Outut parameters are found in PERKOT
//      character*80 perkin,perkot,perkot2
//      character*1 ans
//      dimension y(3,0.0),yp(3,0.0),ypp(3,0.0),ypred(3,0.0),tim(50,0.0),tem(50,0.0)
      std::vector<REAL> y(3),yp(3),ypp(3),ypred(3);
//      real*4 l0,l,ma,kb,kc,kg,kp,ft(35),mt(35),mgas,mtot,mw1,mdel
      REAL l,mgas;
      std::vector<REAL> ft(35,0.0),mt(35,0.0);
//c  intar = .true. calculates tar molecular weight distribution
//c                 in subroutine perkp.
//      logical intar,idiff,imolw
      bool intar = false, idiff = false, imolw;
//      common/cinit/l0,c0,g0,ma,rba,finf,sig,siginv,nmax,pstar
//      common/rate/ab,eb0,ebsig,ac,ec0,ecsig,ag,eg0,egsig,rg
//      common/tarmw/u0,beta,kkk,umax,yield,press   u0 kkk umax yield not used beta set but not used
//      common/timer/tms,time
//c  idiff = .true. for calculations on a differential basis rather than
//c           printing out integrated values.  This option worked on early versions,
//c           but has not been tested on the latest versions.
//      data nmax/10/,nt/1/,intar/.false./,idiff/.false./
//      data ftar0,fgas0,fchar0/0,0,1./,ip/30/,zero/0.0/
      REAL ftar0 = 0.0, fgas0 = 0.0, fchar0 = 1.0, zero = 0.0;
      int ip = 30; nmax = 10;
/*      data y/3*0./,yp/3*0./,ypp/3*0./ypred/3*0./,tim/50*0./,
     x     tem/50*0./,ft/35*0./,mt/35*0./,u0/0./
     x     ,kkk/1/,umax/0./,yield/0./*/
//c  y1 = l	labile bridges
//c  y2 = del	ends
//c  y3 = c	char links
//      data g0,g1,g2/0.,0.,0./
      REAL g0 = 0.0, g1 = 0.0, g2 = 0.0;
//c   read in input data for kinetics
/*      print*,' Enter input file name:'
      read 5,perkin
      print*,' Enter output file name:'
      read 5,perkot
      print*,' Enter 2nd output file name:'
      read 5,perkot2
5     format(a)*/
/*      open(unit=1,name=perkin, type='old')
      open(unit=2,name=perkot, type='unknown')
      open(unit=20,name=perkot2, type='unknown')*/

/*c   read in input data for temperature profile
      read(1,*)ntim
      do 10 i=1,ntim
        read(1,*)tim(i),tem(i)
        tim(i) = tim(i)/1000.
10    continue
c   read time step for calculation
      read(1,*)dt0,iprint,dtmax
      read(1,*)timax
      read(1,*)nmax*/
    time = 0.0;

    /* moved to argument list of yield(...
    REAL dt0 = (REAL)5.e-4, dtmax = (REAL)5.0e-3;
    int iprint = 2;		//dt (s),print increment,max dt (s)
    REAL timax = (REAL)60.e-3;//	!timax (maximum residence time [s] for calculations)*/
    nmax = nmax0; //20;		//!nmax (maximum number of mers for tar molecular wt)c   initialize variables
      y[0] = l0; //y(1) = l0
      y[1] = 2.*(1.-c0-l0); //y(2) = 2.*(1.-c0-l0)
      y[2] = c0; //y(3) = c0
      siginv = 1./sig;
      pstar = 0.5*siginv;
//C   START calculation DO LOOP
//      Rg = 1.987                       !CAL/GMOLE K
//      write(20,203)
/*203   format('c time(ms)      l         c      del/2    g1/2',
     x'      g2/2      gtot',
     x'     p')*/
   if(s3) 
      fprintf(s3,"%s\n","c time(ms)      l         c      del/2    g1/2      g2/2      gtot     p");
   char str7[200], str9[200], str8[200];
   strcpy(str7,FSTR);
   strcpy(str9,FSTR);
   strcpy(str8,FSTR);
   int i;
   for(i=1; i<7; i++){
      strcat(str7," ");
      strcat(str7,FSTR);
   }
   for(i=1; i<8; i++){
      strcat(str8," ");
      strcat(str8,FSTR);
   }
   for(i=1; i<9; i++){
      strcat(str9," ");
      strcat(str9,FSTR);
   }
   strcat(str7,"\n");
   strcat(str9,"\n");
   strcat(str8,"\n");
      int iii = 0;
      fgas0 = 0.;
      REAL fcross = 0.0, dt, tp;
      int ntmax = timax/dt0+1, ntim = tim.size();
      if(dt0<dtmax) dt = dt0;
      else dt = dtmax; //dt = min(dt0,dtmax)
      int nt = 0;
      REAL fmet = 0.0, fchar = 0.0, gasmw, fvol = 0.0, ftar = 0.0, fgas;
      bool finished = false;
      for(iii=0; iii<ntmax; iii++){ //DO 100 iii=1,ntmax
        time = time+dt;
/*CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C PREDICTOR
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C   CALCULATE particle temperature*/
        while(nt<ntim-1){
           if(time<=tim[nt+1]){
           tp = tem[nt] + (time-tim[nt])*(tem[nt+1]-tem[nt])/
                                         (tim[nt+1]-tim[nt]);
           break;
           }else if(nt<ntim-2){
              nt++;
           }else{
              if(s2) cout << "REACHED END OF GAS TEMPERATURE CORRELATION" << endl;
              finished = true;
              break;
           }
        }
        if(finished) break;
/*1       IF(time.LE.tim(nt+1))then
          Tp = tem(nt) + (time-tim(nt))*(tem(nt+1)-tem(nt))/
     x                                  (tim(nt+1)-tim(nt))
        elseif(nt.lt.ntim)then
          nt = nt+1
          go to 1
        ELSE
          PRINT*,'REACHED END OF GAS TEMPERATURE CORRELATION'
          go to 400
        ENDIF*/
        if(tp>4000.){ //if(tp.gt.4000.)then
           cout << "        >>>> WARNING <<<<<" << endl; //print*,'     >>>>   WARNING    <<<<<'
           cout << /*print*,'*/" gas temperature too high----" << tp << endl;//',tg
        } //endif
/*C--    DEVOLATILIZATION RATES 
        call perks(y,ypp,Tp)*/
        perks(y,ypp,tp);
//C COMPONENT MASS CONSERVATION
        int j;
        for(j=0; j<3; j++){ //do 50 j=1,3
           ypred[j] = y[j] + dt*ypp[j];
           if(ypred[j]<0.0) ypred[j] = 0.0; //ypred(j) = max(ypred(j),zero)
        } // 50      continue*/
/*c        print*,' ypred and ypp=',ypred,ypp
c
c  CORRECTOR
c
C--    DEVOLATILIZATION RATES*/ 
        perks(ypred,yp,tp); //Call perks(ypred,yp,Tp)
        REAL dy1;
//C COMPONENT MASS CONSERVATION
        if(y[0]>5.e-3){//if(y(1).gt.5.e-3)then
              dy1 = dt*0.5*(yp[0]+ypp[0]);//dy1 = dt*0.5*(yp(1)+ypp(1))
        }else{
              dy1 = dt*0.5*(yp[2]+ypp[2]);//dy1 = dt*0.5*(yp(3)+ypp(3))
        }//endif
        if(fabs(dy1)<0.001){//if(abs(dy1).lt.0.001)then
              dt = dt*2.;
              if(dt<dtmax&&s2) cout << "at time=" << time//if(dt.lt.dtmax)print*,'at time=',time, 
                  << " dt changed to " << dt << endl;//'  dt changed to ',dt
        }else if(fabs(dy1)>0.02){//elseif(abs(dy1).gt.0.02)then
              dt = 0.01/fabs(dy1)*dt;//dt = 0.01/abs(dy1)*dt
              if(s2) cout << "at time="<<time<< "dt changed to "<<dt << endl;//print*,'at time=',time, 'dt changed to ',dt
        }//endif
        if(dtmax<dt) dt = dtmax;//dt = min(dt,dtmax)
        for(j=0; j<3; j++){//do 70 j=1,3
           y[j] = y[j] + dt*0.5*(yp[j]+ypp[j]);
           if(y[j]<0.0) y[j] = 0.0; //y(j) = max(zero,y(j))
        }//70      continue
//c        if(y(1).lt.1.e-5)dt = dt0*10.
        REAL fracr = 1.;
        if(fmet>1.e-5&&acr>0.){//if(fmet.gt.1.e-5.and.acr.gt.0.)then
           //c           print*,'<main>acr,ecr,rg,tp=',acr,ecr,rg,tp
           REAL ratecr = acr*exp(-ecr/rg/tp)*fmet*dt;
           fracr = 1.-ratecr/fmet;
           fmet = fmet-ratecr;
           fcross = fcross+ratecr;
           if(fmet<0.){//if(fmet.lt.0.)then
              fcross = fcross-ratecr+fmet+ratecr;
              fmet = 0.0;
              fracr = 0.;
           }//endif
        }//endif
//c        print*,' y and yp=',y,yp
//c  print out data
        if(y[0]>1.e-5)intar = true;//if(y(1).gt.1.e-5)intar = .true.
        REAL ftart, fchart;
        perkp(y,mgas,ftar,ftart,fgas,fchart,ft,mt,intar,s2);
        intar = false;
      tms = time*(REAL)1.e3;
      if(idiff){//then
          REAL fdift = ftar-ftar0;
          ftar0 = ftar;
          REAL fdifg = fgas-fgas0;
          REAL fdifc = -(fchar-fchar0);
          fchar0 = fchar;
//        write(2,2010)tms,tp,fcross,y(1),fdift,fdifg,fdifc
          if(s2) fprintf(s2,str7,tms,tp,fcross,y[0],fdift,fdifg,fdifc);
//        write(6,200)tms,tp,fcross,y(1),fdift,fdifg,fdifc
      }else{
          gasmw = rba*ma/2.;
//c           print*,'<calchi> calling flash'
          if(fgas>=1.e-5){//if(fgas.ge.1.e-5)then
//           fgasd = fgas-fgas0;
           fgas0 = fgas;
           imolw = false;
           if(!((iii+1)%iprint)) imolw=true;//if(mod(iii,iprint).eq.0)imolw=.true.
           flash(fgas,gasmw,ft,mt,fracr,ftar,fmet,
                 tp,imolw);
          }else if(fgas<1.e-5){//elseif(fgas.lt.1.e-5)then
           fmet = ftart;
           ftar = 0.;
          }//endif
//c        print*,'<calchi exiting flash'
          intar = false;
          fvol = fgas+ftar;
          fchar = 1.-fvol;
          if(iii==0){//if(iii.eq.1)then
/*      write(6,201)
      write(2,202)
201   format(' time(ms)      temp     fcross     labile    ftar    ',
     x'fgas   fsolid     ftot    fmet')
202   format('c time(ms)      temp     fcross     labile   ftar    ',
     x'fgas   fsolid   ftot   fmet')*/
           if(s2) 
            fprintf(s2,"%s\n"," time(ms)      temp     fcross     labile    ftar    fgas   fsolid     ftot    fmet");
          }//endif
          if(!(iii%iprint)){//if(mod(iii-1,iprint).eq.0)then
           if(s2)
              fprintf(s2,str9,tms,tp,fcross,y[0],ftar,fgas,fchar,fvol,fmet);
           else{
              ytime.push_back(tms*0.001);
              ytar.push_back(ftar);
              ygas.push_back(fgas);
           }
/*          write(2,200)tms,tp,fcross,y(1),ftar,fgas,fchar,fvol,
     x                fmet
c          write(6,*)fcross,fmet
          write(6,200)tms,tp,fcross,y(1),ftar,fgas,fchar,fvol,
     x                fmet*/
          }//endif
      }//endif
//200   format(' ',2(1pe10.3,2x),0pf6.4,2x,f8.5,2x,5(f6.4,2x))
//2010  format(' ',2(1pe10.3,2x),0pf6.4,2x,f8.5,2x,4(1pe10.3,2x))
      l = y[0];//l = y(1)
      REAL del = y[1];//del = y(2)
      REAL c = y[2];//c = y(3)
      REAL p = l+c;
//      tarfac = 1.;
      g1 = (2.*(1-p)-del);
      g2 = 2.*(c-c0);
      REAL g = g1+g2;
      REAL del2 = del/2.;
      REAL g12 = g1/2.;
      REAL g22 = g2/2.;
      REAL gtot = g/2.;
      if(!(iii%iprint)){//if(mod(iii-1,iprint).eq.0)then
        //write(20,220)tms,l,c,del2,g12,g22,gtot,p
      if(s3)
         fprintf(s3,str8,tms,l,c,del2,g12,g22,gtot,p);
      }//endif
//220   format(' ',1pe10.3,2x,7(0pf7.5,2x))*/
      }//100   continue
//400   continue
          intar = false;
          fvol = fgas+ftar;
          fchar = 1.-fvol;
           if(s2)
              fprintf(s2,str9,tms,tp,fcross,y[0],ftar,fgas,fchar,fvol,fmet);
          /*write(2,200)tms,tp,fcross,y(1),ftar,fgas,fchar,fvol,
     x                fmet
          write(6,200)tms,tp,fcross,y(1),ftar,fgas,fchar,fvol,
     x                fmet
      stop
      end*/
           if(s2) fclose(s2); if(s3) fclose(s3);
      }
/*c
c------------------------------------------------------------------
c
      subroutine perks(y,yp,T)
      IMPLICIT real*4 (A-H,O-Z)
c      save
c
c  this subroutine is the meat of the devolatilization model
c      
c  y1 = l	labile bridges
c  y2 = del	ends
c  y3 = c	char links
c  yp(i) = derivative of y(i) in time
c
c     nmax = number of terms in expansion for mol. wt. distribution*/
void coal_perc_devol::perks(std::vector<REAL>& y, std::vector<REAL>& yp,REAL tp)
{
      REAL l = y[0];//l = y(1)
      REAL del = y[1];//del = y(2)
      REAL c = y[2];//c = y(3)
      REAL p = l+c;
      REAL g1 = 2.*(1-p)-del;
      REAL g2 = 2.*(c-c0);
      REAL g = g1+g2, fx = 0.0, x;
//c  calculate current activation energy using error function solution
      if(c0<1.)fx = g/(1.-c0)/2.;//if(c0.lt.1.)fx = g/(1.-c0)/2.
      inverf(fx,x);//call inverf(fx,x)
      REAL eg = eg0 + x*egsig;
      if(l0>0.)fx = 1.-l/l0;//if(l0.gt.0.)fx = 1.-l/l0
      inverf(fx,x);//call inverf(fx,x)
//c      print*,'temperature=',t,'   eg=',eg,'  g/ginf=',fx
      REAL eb = eb0+x*ebsig;
      REAL ec = ec0;
//c  calculate rate constants
      REAL rt = rg*tp;
      REAL kb = ab*exp(-eb/rt);
      REAL rho = ac*exp(-ec/rt);
      REAL kg = ag*exp(-eg/rt);
//c  calculate rate of destruction of labile bridges
      yp[0] = -(kb)*l;//yp(1) = -(kb)*l
//c  calculate rate of formation of ends (danglers)
      yp[1] = 2.*rho*kb*l/(rho+1.) - kg*del;//yp(2) = 2.*rho*kb*l/(rho+1.) - kg*del
//c  calculate rate of formation of char
      yp[2] = kb*l/(rho+1.);//yp(3) = kb*l/(rho+1.)
/*      real*4 l0,l,ma,kb,kc,kg,kp,ft(35),mt(35),y(3),yp(3)
      common/cinit/l0,c0,g0,ma,rba,finf,sig,siginv,nmax,pstar
      common/rate/ab,eb0,ebsig,ac,ec0,ecsig,ag,eg0,egsig,rg
      data fx/0./
c
      return
      end*/
}

/*c------------------------------------------------------------------
c
      subroutine perkp(y,mgas,ftar,ftart,fgas,fchar,ft,mt,intar)
      IMPLICIT real*4 (A-H,O-Z)
c      save
c
c   calculates fractions of tar, gas, and char from p, sig, l, and c
c*/
void coal_perc_devol::perkp(std::vector<REAL>& y, REAL& mgas, REAL& ftar, REAL& ftart,
      REAL& fgas, REAL& fchar, std::vector<REAL>& ft, std::vector<REAL>& mt,
      bool& intar, FILE *s2)
   {
/*
      real*4 l0,l,ma,mb,kb,kc,kg,kp,y(3),yp(3),ft(35),mt(35),
     x    mgas,mtot,sumu(200)
      logical intar
      common/cinit/l0,c0,g0,ma,rba,finf,sig,siginv,nmax,pstar
      common/rate/ab,eb0,ebsig,ac,ec0,ecsig,ag,eg0,egsig,rg
      common/tarmw/u0,beta,kkk,umax,yield,press
      data pi/3.141592/,iprint/0/*/
//c
      REAL l = y[0];//l = y(1)
      REAL del = y[1];//del = y(2)
      REAL c = y[2];//c = y(3)
      REAL p = l+c;
      REAL a, b;
//c      print*,' <perk>l,del,c,g1,g2,g,p,mgas,mtot=',
//c     x               l,del,c,g1,g2,g,p,mgas,mtot
      if(intar){//then
         REAL delfac;
         if(p>0.9999){//if(p.gt.0.9999)then
          delfac = 1.;
         }else{
          delfac = del/(1.-p);
         }//endif
        a = 1.+rba*(l/p + (sig-1.)/4. *delfac);
        b = (delfac/2. - l/p);
//c  find pstar
        REAL pstar0 = pstar;
        REAL pinv = siginv+1.e-4;
        if(p>=0.9999){//if(p.ge.0.9999)then
           pstar = 0.;
        }else if(p>=pinv){//else if(p.ge.pinv)then
           int i; REAL err;
           for(i=0; i<25; i++){//do 10 i=1,25
//              f = pstar*(1-pstar)**(sig-1) - p*(1-p)**(sig-1);
              REAL f = pstar*pow(1.0-pstar,sig-1.0) - p*pow(1.0-p,sig-1.0);
//              fp = (1-pstar)**(sig-1)-pstar*(sig-1)*(1-pstar)**(sig-2);
              REAL fp = pow(1.0-pstar,sig-1.0)-pstar*(sig-1.0)*pow(1.0-pstar,sig-2.0);
              REAL ppstar = pstar - f/fp;
              err= fabs(1.-ppstar/pstar);//err= abs(1.-ppstar/pstar)
              if(err<=1.e-4) break;//if(err.le.1.e-4)go to 11
              pstar = ppstar;
           }//10         continue
           /*print*,' warning--pstar did not converge'
           print*,' p=',p,' sig=',sig,' pstar=',pstar,
     x       '  pstar0=',pstar0
11         continue*/
           if(err>1.e-4){
              cout << " warning--pstar did not converge" << endl;
              cout << " p=" << p << " sig=" << sig << " pstar=" << pstar << " pstar0=" << pstar0 << endl;
           }
        }else{
           pstar = p;
        }//endif
//c
//c  check to see if pstar is in the right range
//      if(pstar.lt.0. .or. (p.ne.pstar .and. pstar.ge.siginv))then
        if(pstar<0. || (p!=pstar && pstar>=siginv)){
           cout << " error--pstar out of acceptable ranges!" << endl;
           cout << "     pstar=" << pstar << ";  HOW DID YOU DO THAT?" << endl;
           cout << "p=" << p << " sig=" << sig << " pstar0=" << endl;
           return;
           /*print*,' error--pstar out of acceptable ranges!'
           print*,'     pstar=',pstar,';  HOW DID YOU DO THAT?'
           print*,'p=',p,'  sig=',sig,'  pstar0=',pstar0
           stop*/
        }//endif
//c
        REAL sfac = (sig+1.)/(sig-1.);
        REAL fp = pow(pstar/p,sfac);//fp = (pstar/p)**(sfac)
        REAL kp = pow(pstar/p,sfac*(1.-(sig+1)/2. *pstar));
//c  calculate wt fraction tar, gas, and char
        ftart = 2.*(a*fp+rba*b*kp)/(2.+rba*(1.-c0)*(sig+1.));
      }//endif
      REAL tarfac = 1.-ftar;
      REAL g1 = (2.*(1.0 - p) - del);
      REAL g2 = 2.*(c-c0);
      REAL g = g1+g2;
//c
      mgas = rba*ma*g*(sig+1)/4.*tarfac;
      REAL mtot = ma + rba*ma*(sig+1)/2. *(1.-c0);
      fgas = mgas/mtot;
      fchar = 1.-ftar-fgas;
//c      print*,'<perkp>ftar,fgas,fchar=',ftar,fgas,fchar
//c  calculate tar molecular weight distribution
      if(!intar)return;//if(.not.intar)return
      iprint = iprint+1;
//c      if(iprint.eq.1)
//c     x  open(unit=25,name='perktar.out',type='unknown')
      REAL ftsum = 0.0;
      int n;
      for(n=0; n<nmax; n++){//do 20 n=1,nmax
         REAL tn = (n+1)*(sig-1.)+2;
         REAL xm = (n+1)*sig+1.;
         REAL yk = n;
         REAL xm1 = xm+1.;
//c  gamln is the solution to the gamma function in the Sandia Math Library
         REAL fg1 = gamln(xm1), fgam;
         if(fg1<=1.e-10){//if(fg1.le.1.e-10) then
            fgam = 0.;
         }else{
           REAL yk1 = yk+1.;
           REAL fg2 = gamln(yk1);
           REAL xmyk = xm-yk+1.;
           REAL fg3 = gamln(xmyk);
           fgam = exp(fg1-fg2-fg3);
         }//endif
         REAL bnn = (sig+1.)/((n+1)*sig+1.)*fgam;
         REAL qn = bnn*pow(p,n)*pow(1-p,tn)/(REAL)(n+1);//qn = bnn*(p**(n-1))*((1-p)**tn)/n
//c  ft(n) = weight fraction of each tar bin
         ft[n] = 2.*((n+1)*a*qn+rba*b*qn)/(2.+rba*(1.-c0)*(sig+1.));
         ftsum = ftsum + ft[n];
//c  check to not divide by zero
         REAL fac;
         if(p<=1.e-9){//if(p.le.1.e-9)then
            fac = 0.;
         }else{
            fac = l/p;
         }//endif
         REAL tst = 1.-p, fac1;
         if(tst<=1.e-9){//if(tst.le.1.e-9)then
            fac1 = 0.;
         }else{
            fac1 = del/(1.-p);
         }//endif
//c  mt(n) = molecular weight of each tar bin
         //mt(n) = n*ma+(n-1)*rba*ma*fac+tn*rba*ma/4.*fac1
         mt[n] = REAL(n+1)*ma+REAL(n/*-1*/)*rba*ma*fac+tn*rba*ma/4.*fac1;
            if(iprint==1){//if(iprint.eq.1)then
               if(n==0){//if(n.eq.1)then
//             print*,'Initial Fragment size distribution'
  //           print*,'n   fraction      Cumul.   MW'
                  if(s2) cout << "Initial Fragment size distribution" << endl;
                  if(s2) cout << "n   fraction      Cumul.   MW" << endl; 
               }//Endif
//           print*,n,ft(n),ftsum,mt(n)
               if(s2) cout << n << " " << ft[n] << " " << ftsum << " " << mt[n] << endl;
//c           write(24,*)ft(n),ftsum,mt(n)
            }//endif
      }//20    continue
      //return
      //end
   }
//////////////////////////////////////
/*      SUBROUTINE INVERF(Y,X)
      IMPLICIT real*4 (A-H,O-Z)
c      save
c  this program calculates the inverse of the area under the normal curve.
c  if y=area(x), then given y, this program will calculate x.
c  A table lookup is performed.*/
void coal_perc_devol::inverf(REAL y, REAL& x)
{
/*
      dimension xx(18),yy(18)
      data xx/3.4,3.2,3.,2.8,2.6,2.4,2.2,2.,1.8,1.6,1.4,
     x        1.2,1.,.8,.6,.4,.2,0./
      data yy/.9997,.9993,.9987,.9974,.9953,.9918,.9861,.9772,.9641,
     x        .9452,.9192,.8849,.8413,.7881,.7257,.6554,.5793,.5/ */
   REAL xx[18] = {3.4,3.2,3.,2.8,2.6,2.4,2.2,2.,1.8,1.6,1.4,
                  1.2,1.,.8,.6,.4,.2,0.};
   REAL yy[18] = {.9997,.9993,.9987,.9974,.9953,.9918,.9861,.9772,.9641,
                  .9452,.9192,.8849,.8413,.7881,.7257,.6554,.5793,.5};
//c
      REAL fac = 1., yp;
//c  check to see if y is within range
      if(y<0.0228){//if(y.lt.0.0228)then
         x = -2.0;
         return;
      }else if(y<0.5){//elseif(y.lt.0.5)then
        yp = 1.-y;
        fac = -1.;
      }else if(y>0.9997){//elseif(y.gt.0.9997)then
        x = 3.5;
        return;
      }else{
        yp = y;
      }//endif
//c  search for range
      int i;
      for(i=16; i>-1; i--){//do 10 i=17,1,-1
         if(yp<=yy[i]){//if(yp.le.yy(i+1))then
          x = xx[i] + (yp-yy[i])*(xx[i+1]-xx[i])/(yy[i+1]-yy[i]);
          x = fac*x;
          return;
         }//endif
      }//10    continue
}
////////////////////////////////////
/*c
      function gamln(x)
c   this is a program to calculate the ln of the gamma function,
c   taken from Abramowitz, p. 257, 6.1.41
      data pi/3.141592/
         gamln = (x-.5)*alog(x)-x+.5*alog(2.*pi)+1./(12.*x)
     x        -1./(360.*x**3)+1./(1260.*x**5)-1./(1680.*x**7)
      return
      end*/
REAL coal_perc_devol::gamln(REAL x)
{
   REAL pi = 3.141592654;
   REAL x3 = x*x*x;
   REAL x5 = x3*x*x;
   REAL x7 = x5*x*x;
   REAL gamln = (x-.5)*log(x)-x+.5*log(2.*pi)+1./(12.*x)
             -1./(360.*x3)+1./(1260.*x5)-1./(1680.*x7);
   return gamln;
}
///////////////////////////////////////
/*c
      subroutine flash(fgas,gasmw,ft,mt,fracr,ftar,fmet,
     x                  temp,press,nmax,imolw)
      IMPLICIT real*4 (A-H,O-Z)
c      save
c  flash distillation of metaplast to form liquid and tar vapor*/
void coal_perc_devol::flash(REAL& fgas, REAL& gasmw, std::vector<REAL>& ft,
                            std::vector<REAL>& mt,REAL& fracr,REAL& ftar,
                            REAL& fmet,REAL& temp,bool imolw)
{
/*
      logical imolw
      real*4 k(36),l(36),Ltot,mt(35),metold(35)
      dimension f(36),ft(35),xmw(36),z(36),pv(36),x(36),y(36),
     x          v(36),ftold(35),tarold(35)*/
   std::vector<REAL> k(36,0.0),l(36,0.0),f(36,0.0),xmw(36,0.0),z(36,0.0),
      pv(36,0.0),x(36,0.0),y(36,0.0),v(36,0.0);
   REAL small = 1.e-3;
      /*common/timer/tms,time
      data small/1.e-3/,a,b,g/87058,299,0.5903/,x3,x2/.2,.3/,
     x     zero/0.0/,ip/30/,x/36*0./,y/36*0./
      data k/36*0/,l/36*0/,v/36*0/,metold/35*0./,ftold/35*0./,
     x     xmw/36*0.0/,tarold/35*0./,f/36*0./,z/36*0./,pv/36*0./
     x     ,fgas0/0./ */
   REAL a = 87058.0, b = 299.0, g = 0.5903, x3 = 0.3, x2 = 0.3;
   int ip = 30;
/*c
c  metold(i) = mass fraction of coal contained in metaplast of mer size i
c  fracr = fraction to account for reduction of metaplast by crosslinking
c          in latest time step
c  
c  renormalize in tar fragment data to molar basis
c  f(i) = moles of mer i
c  ft(i) = mass of mer i
c  */
      REAL Ftot = 0.0;
      int i, i1;
      for(i=0; i<nmax; i++){//do 10 i=1,nmax
         i1=i+1;
         xmw[i1] = mt[i];
         REAL dif = ft[i]-ftold[i];
         if(dif<0.0) dif = 0.0;//dif = max(dif,zero)
         f[i1] = (dif+metold[i]*fracr)/mt[i];
         ftold[i] = ft[i];
         Ftot = Ftot + f[i1];
//         if(f(i1).gt.1.0)print*,'f(i1),ft(i),ftold(i),metold(i)=',
//     x   i1,f(i1),ft(i),ftold(i),metold(i)
         if(f[i1]>1.0) cout << "i1,f[i1],ft[i],ftold[i],metold[i]=" << i1 << " "
            << f[i1] << " " << ft[i] << " " << ftold[i] << " " << metold[i] << endl;
      }//10    continue
      int ntot = nmax + 1;
      f[0] = (fgas-fgas0)/gasmw;//f(1) = (fgas-fgas0)/gasmw
      if(f[0]<0.0) f[0] = 0.0;//f(1) = max(f(1),0.)
      if(fgas>fgas0) fgas0 = fgas;//fgas0 = max(fgas,fgas0)
      xmw[0] = gasmw;//xmw(1) = gasmw
      Ftot += f[0];//Ftot = Ftot + f(1)
/*c  get mole fraction of components in the feed stream
c  and compute equilibrium contants k(i) from vapor pressure and
c  Raoults law expression*/
      REAL sum = 0.0; int ii;
      for(ii=0; ii<ntot; ii++){//do 30 ii=1,ntot
        sum = sum + f[ii];
        pv[ii] = a*exp(-b*pow(xmw[ii],g)/temp);
        k[ii] = pv[ii]/press;
        if(k[ii]<0.001)k[ii] = 0.0;//if(k(ii).lt.0.001)k(ii) = 0.0
      }//30    continue
      if(sum<=1.e-8)return;//if(sum.le.1.e-8)return
      for(ii=0; ii<ntot; ii++){//do 40 ii=1,ntot
        z[ii] = f[ii]/sum;
      }//40    continue
//c  use the Rachford-Rice formulation for flash distillation
//c  x = V/F, first guess
      REAL x1 = x3;
//c  calculate sum (Eq. 11-24, Separation Processes, by King, 1971)
      REAL f1 = 0.0;
      for(ii=0; ii<ntot; ii++){//do 50 ii=1,ntot
         f1 = f1 + z[ii]*(k[ii]-1)/((k[ii]-1)*(x1)+1);
      }//50    continue
//c  Secant Method for convergence
      REAL test = x2-x1;
      if(test<0.005)x2=x1+0.005;//if(test.lt.0.005)x2=x1+0.005 
      bool lconv = false;
      int iter;
      for(iter=0; iter<100; iter++){//do 70 iter=1,100
//c  calculate sum (Eq. 11-24, Separation Processes, by King, 1971)
        REAL f2 = 0.0;
        for(ii=0; ii<ntot; ii++){//do 60 ii=1,ntot
           f2 = f2 + z[ii]*(k[ii]-1)/((k[ii]-1)*(x2)+1);
        }//60      continue
//        if(abs(f2).le.small.or.abs(f2-f1).le.small**2)go to 100
        if(fabs(f2)<=small||fabs(f2-f1)<=small*small){
           lconv = true;
           break;
        }
        x3 = x2-f2*(x2-x1)/(f2-f1);
        if(x3>1.0)x3 = 1.0-small*small;//if(x3.gt.1.0)x3 = 1.0-small**2
        if(x3<0.)x3 = small*small;//if(x3.lt.0.)x3 = small**2
        if(x3==x2){//if(x3.eq.x2)then
          //print*,'problem---f(V/F) not converged, but x3=x2',x3,x2
          cout << "problem---f(V/F) not converged, but x3=x2 " <<x3<<" "<<x2<<endl;
          if(x2>=small){//if(x2.ge.small)then
            x3=x2-small;
          }else{
            x3 = x2+small;
          }//endif
        }//endif
        if(x2<=1.e-5&&x1<=1.e-5){//if(x2.le.1.e-5.and.x1.le.1.e-5)then
           x2 = 1.e-7;
           lconv = true;
           break; // go to 100
        }//endif
        if(x2>=.9999 && x1>=.9999){//if(x2.ge..9999 .and. x1.ge..9999)then
           x2 = .9999;
           lconv = true;
           break; // go to 100
        }//endif
        f1 = f2;
        x1 = x2;
//c  under-relax solution (especially near the V/F=1 limit
        x2 = 0.2*x2+0.8*x3;
//c        print*,x1,x2,x3
      }//70      continue
      if(!lconv){
      cout << "Convergence not achieved in Flash distillation" << endl;
      cout << " last two guesses for V/F were" <<x1<<" "<<x3<<endl;
      for(ii=0; ii<ntot; ii++) cout << z[ii] << " " << k[ii] <<endl;
      exit(1);
      }
/*        print*,'Convergence not achieved in Flash distillation'
        print*,' last two guesses for V/F were',x1,x3
        do 81 ii=1,ntot
          write(30,*)z(ii),k(ii)
81      continue
        stop
100     continue*/
//c  now calculate molecular weight distributions on a
//c  light-gas free basis, wt fractions
        REAL Vtot = Ftot*x2;
        REAL Ltot = Ftot-Vtot;
        REAL VoL = Vtot/Ltot;
        REAL sumx = 0.0;
        REAL sumy = 0.0;
        REAL xmwtot = 0.0;
        REAL ttot = 0.0;
        for(ii=1; ii<ntot; ii++){//do 200 ii=2,ntot
          i = ii-1;
          l[ii] = f[ii]/(1.+k[ii]*VoL);
          v[ii] = f[ii]-l[ii];
          x[ii] = l[ii]*xmw[ii];
          y[ii] = v[ii]*xmw[ii];
          if(x[ii]>0.0) metold[i] = x[ii];//metold(i) = max(x(ii),zero)
          else metold[ii] = 0.0;
          tarold[i] = tarold[i]+y[ii];
          xmwtot = xmwtot+tarold[i]*xmw[ii];
          ttot = ttot+tarold[i];
          sumx = sumx + x[ii];
          sumy = sumy + y[ii];
        }//200     continue
        if(ttot>0.)xmwtot = xmwtot/ttot;//if(ttot.gt.0.)xmwtot = xmwtot/ttot
        for(ii=1; ii<ntot; ii++){//do 250 ii=2,ntot
           if(sumx>1.e-28)x[ii] /= sumx;//if(sumx.gt.1.e-28)x(ii) = x(ii)/sumx
           if(sumy>1.e-28)y[ii] /= sumy;//if(sumy.gt.1.e-28)y(ii) = y(ii)/sumy
        }//250     continue
        ftar = ftar+sumy;
        fmet = sumx;
        if(imolw){//then
          ip = ip+1;
          //print*,'Weight Av. Molecular Wt.=',xmwtot
     //x         ,' Gas MW =',gasmw
        }//endif
/*d210     format(' ',i2,2x,f5.0,4(1pe10.3,2x))
d211     format(' ',f5.0,4(1pe10.3,2x))
        return
        end*/

}
