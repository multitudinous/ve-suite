#pragma warning(disable: 4786)

#include "sode_rk.h"
#include <iostream>
#include <cmath>

using std::cout;
using std::endl;

//////////////////
sode_rk::sode_rk(std::vector<REAL>& y0, REAL dxsav0)
{
   int nvar = y0.size(), i;
   ystart.resize(nvar);
   for(i=0; i<nvar; i++) ystart[i] = y0[i];
   yp.resize(nvar);
   dxsav = dxsav0;
}
////////////////////
void sode_rk::runk4(std::vector<REAL>& y, std::vector<REAL>& dydx,
                        REAL& x, REAL& h, std::vector<REAL>& yout, model& mdl)
{
   int n = y.size(), i, j;
   std::vector< std::vector<REAL> > dydx_(3);
   for(i=0; i<3; i++) dydx_[i].resize(n);
   std::vector<REAL> yt(n), xph(3);
   REAL hh[3] = {h*0.5, h*0.5, h};
   REAL h6 = h/6.;
   xph[0] = x + hh[0];
   for(i=0; i<n; i++) yt[i] = y[i] + hh[0]*dydx[i];
   mdl.derivs(xph[0],yt,dydx_[0]);
   for(i=1; i<3; i++){
      xph[i] = x + hh[i];
      for(j=0; j<n; j++) yt[j] = y[j] + hh[i]*dydx_[i-1][j];
      mdl.derivs(xph[i],yt,dydx_[i]);
   }
   for(i=0; i<n; i++)
      yout[i] = y[i] + h6*(dydx[i] + 2.0*(dydx_[0][i] + dydx_[1][i]) + dydx_[2][i]);
}
///////////////////////
void sode_rk::integrate(REAL& x1, REAL& x2, REAL& eps, REAL& h1, REAL& hmin, 
                        std::vector<REAL>& yscal, model& mdl)
{
   int nvar = ystart.size();
   std::vector<REAL> y, dydx;
   y.resize(nvar);
   dydx.resize(nvar);
   int i;
   for(i=0; i<nvar; i++) dydx[i] = 0.0;
   xp.clear();
   for(i=0; i<nvar; i++) yp[i].clear();
   REAL x = x1;
   REAL h = ( x2-x1>0 ? h1 : -h1 );
   for(i=0; i<nvar; i++){
      y[i] = ystart[i];
   }
   REAL xsav = x - dxsav*2.0;
   //REAL tiny = (REAL)1.0e-30;
   REAL hnext;
   if(fabs(x-xsav)>fabs(dxsav)){
      xp.push_back(x);
      for(i=0; i<nvar; i++){
         yp[i].push_back(y[i]);
      }
      mdl.push_back(x, y ,dydx);
      xsav = x;
   } // if(fabs(x-xsav)>
   mdl.update(x, x2, xsav, y, dydx);
   for(;;){
      mdl.derivs(x, y, dydx);
      if(x+h>x2) h = x2 - x;
      control_step(y, dydx, x, h, eps, yscal, hnext, mdl);
      mdl.update(x, x2, xsav, y, dydx);
      if(fabs(x-xsav)>fabs(dxsav)){
         xp.push_back(x);
         for(i=0; i<nvar; i++){
            yp[i].push_back(y[i]);
         }
         mdl.push_back(x, y ,dydx);
         xsav = x;
      } // if(fabs(x-xsav)>
      if((x-x2)*(x2-x1)>=0.0){
         /*for(i=0; i<nvar; i++){
            ystart[i] = y[i];
         }*/
         xp.push_back(x);
         for(i=0; i<nvar; i++){
            yp[i].push_back(y[i]);
         }
         mdl.update(x, x2, xsav, y, dydx);
         mdl.push_back(x, y ,dydx);
         return;
      }
      
      if(fabs(hnext)<hmin) {
         cout << "Stepsize smaller than minimum." << endl;
         //return;
      }
      h = hnext;
   } // for(;;)
}
/////////////////////////
void sode_rk::control_step(std::vector<REAL>& y, std::vector<REAL>& dydx, 
                   REAL& x, REAL& htry, REAL& eps, std::vector<REAL> yscal,
                   REAL& hnext, model& mdl)
{
   int n = y.size();
   std::vector<REAL> ytemp, ysav, dysav;
   ytemp.resize(n);
   ysav.resize(n);
   dysav.resize(n);
   REAL factor = (REAL)0.9, errcon = (REAL)6.0e-4;
   REAL grow = (REAL)-0.20;
   REAL shrnk = (REAL)-0.25;
   REAL xsav = x;
   REAL term;
   int i;
   for(i=0; i<n; i++){
      ysav[i] = y[i];
      dysav[i] = dydx[i];
   }
   REAL h = htry;
   REAL errmax = (REAL)1.0e20;
   while(errmax>1.0){
      REAL hh = 0.5*h;
      runk4(ysav, dysav, xsav, hh, ytemp, mdl);
      x = xsav + hh;
      mdl.derivs(x,ytemp,dydx);
      runk4(ytemp, dydx, x, hh, y, mdl);
      x = xsav + h;
      if(x==xsav){
         cout << "Stepsize not significant in sode_rk::control_step" << endl;
         cout << h << endl;
      }
      runk4(ysav, dysav, xsav, h, ytemp, mdl);
      errmax = 0.0;
      for(i=0; i<n; i++){
         ytemp[i] = y[i] - ytemp[i];
         term = fabs(ytemp[i]/yscal[i]);
         if(term>errmax) errmax = fabs(term);
      }
      errmax /= eps;
      if(errmax>1.0){
         h = factor*h*pow(errmax,shrnk);
      }
   }
   //cout << x << " " << h << endl;
   if(errmax>errcon){
      hnext = factor*h*pow(errmax,grow);
   }else{
      hnext = 4.0*h;
   }
}
