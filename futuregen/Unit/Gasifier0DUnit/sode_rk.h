    
// class definition for sode_rk class
    
#ifndef _sode_rk_h_
#define _sode_rk_h_
    
#include <V21Helper/Therm/REAL.h>
//#include "REAL.h"
#include "part_kinetics.h"
#include <vector>
#include <cstdio>

class sode_rk
{

public:

   // mutators
   sode_rk(){};
   sode_rk(std::vector<REAL>& y0, REAL dxsav0);
   ~sode_rk(){};

   void integrate(REAL& x1, REAL& x2, REAL& eps, REAL& h1, REAL& hmin, 
                  std::vector<REAL>& yscal, model& mdl);

   // accessors
   std::vector<REAL>& get_xp(){return(xp);};
   std::vector< std::vector<REAL> >& get_yp(){return(yp);};
   std::vector<REAL>& get_ystart(){return(ystart);};

protected:

   void runk4(std::vector<REAL>& y, std::vector<REAL>& dydx, 
      REAL& x, REAL& h, std::vector<REAL>& yout, model& mdl);
   void control_step(std::vector<REAL>& y, std::vector<REAL>& dydx, REAL& x,
      REAL& htry, REAL& eps, std::vector<REAL> yscal, REAL& hnext, model& mdl);


 //      COMMON /PATH/ KMAX,KOUNT,DXSAV,XP(200),YP(10,200)
   int kmax; //, kount;
   REAL dxsav;
   std::vector<REAL> ystart;
   std::vector<REAL> xp;
   std::vector< std::vector<REAL> > yp;

};

#endif
