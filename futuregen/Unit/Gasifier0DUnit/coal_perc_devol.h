#ifndef _coal_perc_devol_h_
#define _coal_perc_devol_h_
    
#include <Therm/REAL.h>
#include <vector>

class coal_perc_devol
{

public:
   coal_perc_devol(){};
   coal_perc_devol(std::string filenam);
   coal_perc_devol(REAL carbon, REAL hydrogen, REAL oxygen,
      REAL volm, REAL pressure);
   ~coal_perc_devol(){};

   void init();
   void yield(std::vector<REAL>& tim, std::vector<REAL>& tem, FILE *s2, FILE *s3,
      REAL dt0, REAL dtmax, int iprint, REAL timax, int nmax0,
      std::vector<REAL>& ytar, std::vector<REAL>& ygas, std::vector<REAL>& ytime);
   void perks(std::vector<REAL>& y, std::vector<REAL>& ypp,REAL tp);
   void perkp(std::vector<REAL>& y, REAL& mgas, REAL& ftar, REAL& ftart,
      REAL& fgas, REAL& fchart, std::vector<REAL>& ft, std::vector<REAL>& mt,
      bool& intvar, FILE* s2);
   void inverf(REAL fx, REAL& x);
   REAL gamln(REAL x);
   void flash(REAL& fgas,REAL& gasmw,std::vector<REAL>&ft,std::vector<REAL>&mt,
      REAL& fracr,REAL& ftar,REAL& fmet,REAL& tp, bool imolw);

   void set_nmax(int n0) {nmax = n0;}
   void set_press(REAL p0) {press = p0;}
   void set_c0(REAL c1) {c0 = c1;}

private:

      //common/cinit/l0,c0,g0,ma,rba,finf,sig,siginv,nmax,pstar
   int nmax;
   REAL l0,c0,g0,ma,rba,finf,sig,siginv,pstar;
      //common/rate/ab,eb0,ebsig,ac,ec0,ecsig,ag,eg0,egsig,rg
   REAL ab,eb0,ebsig,ac,ec0,ecsig,ag,eg0,egsig,rg;
      //common/tarmw/u0,beta,kkk,umax,yield,press
   REAL press; // u0, kkk, umax, yield not used. beta set but not used
      //common/timer/tms,time
   REAL tms,time;

   REAL p0, sigp1, mw1, mdel, acr, ecr, mb, beta, fgas0;
   std::vector<REAL> tim, tem, metold, ftold, tarold;
   int iprint;   
   //std::vector<REAL> k,l,f,xmw,z,pv,x,y,v;
};

#endif
