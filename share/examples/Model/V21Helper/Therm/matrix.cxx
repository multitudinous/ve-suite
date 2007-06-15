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

#pragma warning(disable: 4786)
#include <cstdio>
#include <cstring>
#include <string>
#include "matrix.h"
#include <cmath>

using std::cout;
using std::endl;

///////////////////////
void matrix::resize(int& nmat0)
{
   nmat = nmat0;
    mat.resize(nmat*nmat);
    vmat.resize(nmat*nmat);
    wd.resize(nmat);
    rv1.resize(100);
    tmp.resize(100);
    bb.resize(nmat);
    uu.resize(nmat);                   
}
////////////////////////
void matrix::solv_mat(REAL tol_min)
{
   svd();
   REAL wmax = 0.0;
   int j;
   for(j=0; j<nmat; j++) if(wd[j] > wmax) wmax=wd[j];
   REAL wmin = wmax*tol_min;
   for(j=0; j<nmat; j++) if(wd[j] < wmin) wd[j] = 0.0;
   back_sub();
/*   int i, j, k, imax, isw, i0, j0;
   REAL max, matij;

   for(i=0; i<nmat; i++) irow[i] = i;
   for(j=0; j<nel; j++){
      // maximum column pivoting
      max = mat[irow[j]+j*nmat];
      imax = j;
      for(i=j+1; i<nmat; i++){
         i0 = irow[i];
         matij = fabs(mat[i0+j*nmat]);
         if(matij > max){
            max = matij;
            imax = i;
         }
      }
      isw = irow[j];
      irow[j] = irow[imax];
      irow[imax] = isw;
      j0 = irow[j];
      for(i=j+1; i<nmat; i++){
         i0 = irow[i];
         mat[i0+j*nmat] /= mat[j0+j*nmat];
         for(k=j+1; k<nmat; k++) {
            mat[i0+k*nmat] -= mat[i0+j*nmat]*mat[j0+k*nmat];
         }
      }
   }
   for(j=0; j<nel; j++){
      j0 = irow[j];
      for(i=j+1; i<nmat; i++){
         i0 = irow[i];
         bb[i0] -= mat[i0+j*nmat]*bb[j0];
      }
   }
   int nmat1 = nmat-1;
   uu[nmat1] = bb[irow[nmat1]]/mat[irow[nmat1]+nmat1*nmat];
   for(i=nel-1; i>=0; i--){
      i0 = irow[i];
      uu[i] = bb[i0];
      for(j=i+1; j<nmat; j++){
         uu[i] -= mat[i0+j*nmat]*uu[j];
      }
      uu[i] /= mat[i0+i*nmat];
   }*/
}
//////////////////////////
void matrix::svd()
{
      REAL g=0.0;
      REAL scale=0.0;
      REAL anorm=0.0;
      REAL s, f, h, x, y, z, c;
      int i, l, k, j, its, nm, nm1, n = nmat;
      int flag;
      for(i=0; i<n; i++){
        l=i+1;
        rv1[i]=scale*g;
        g=0.0;
        s=0.0;
        scale=0.0;
        if(i<nmat){
           for(k=i; k<nmat; k++){
            scale+=fabs(mat[k+i*nmat]);
           }
           if(scale){
              for(k=i; k<nmat; k++){
                mat[k+i*nmat] /= scale;
                s += mat[k+i*nmat]*mat[k+i*nmat];
              }
            f=mat[i+i*nmat];
            g=-(f>=0.0 ? sqrt(s) : -sqrt(s));
            h=f*g-s;
            mat[i+i*nmat]=f-g;
            if(i!=n-1){
              for(j=l; j<n; j++){
                s=0.0;
                for(k=i; k<nmat; k++) s += mat[k+i*nmat]*mat[k+j*nmat]; //a(k,i)*a(k,j)
                f=s/h;
                for(k=i; k<nmat; k++) mat[k+j*nmat] += f*mat[k+i*nmat];
              } // for(j
            } // if(j!=n-1
            for(k=i; k<nmat; k++) mat[k+i*nmat] *= scale;
           } // if(scale)
        } // if(i<nmat);
        wd[i]=scale *g;
        g=0.0;
        s=0.0;
        scale=0.0;
        if((i<=nmat-1)&&(i!=n-1)){
          for(k=l; k<n; k++) scale += fabs(mat[i+k*nmat]);
          if (scale) {
            for(k=l; k<n; k++){
              mat[i+k*nmat] /=scale;
              s += mat[i+k*nmat]*mat[i+k*nmat];
            }
            f=mat[i+l*nmat];
            g = -( f>=0.0 ? sqrt(s) : -sqrt(s) );
            h=f*g-s;
            mat[i+l*nmat]=f-g;
            for(k=l; k<n; k++) rv1[k] = mat[i+k*nmat]/h;
            if(i!=nmat-1){
               for(j=l; j<nmat; j++){
                s=0.0;
                for(k=l; k<n; k++){
                  s += mat[j+k*nmat]*mat[i+k*nmat];
                }
                for(k=l; k<n; k++){
                  mat[j+k*nmat] += s*rv1[k];
                }
               }  // for(j=l
            } // if(i!=nmat-1
            for(k=l; k<n; k++){
              mat[i+k*nmat] *= scale;
            }
          } // if(scale
        } //if((i<=nmat-1)&&(i!=n-1)
        REAL anorm1 = fabs(wd[i])+fabs(rv1[i]);
        if(anorm1>anorm) anorm = anorm1;
      } //for(i
      for(i=n-1; i>=0; i--){
         if(i<n-1){
           if(g){
               for(j=l; j<n; j++){
                 vmat[j+i*nmat] = (mat[i+j*nmat]/mat[i+l*nmat])/g;
               }
              for(j=l; j<n; j++){
                  s=0.0;
                  for(k=l; k<n; k++){
                     s += mat[i+k*nmat]*vmat[k+j*nmat];
                  }
                  for(k=l; k<n; k++){
                     vmat[k+j*nmat] += s*vmat[k+i*nmat];
                  }
              } // for(j=l
           } // if(g)
           for(j=l; j<n; j++){
               vmat[i+j*nmat] = 0.0;
               vmat[j+i*nmat] = 0.0;
           }
        } // if(i<n-1   
        vmat[i+i*nmat] = 1.0;
        g = rv1[i];
        l=i;
      } // for(i
      for(i=n-1; i>=0; i--){
        l=i+1;
        g=wd[i];
        if(i<n-1) {
           for(j=l; j<n; j++) {
            mat[i+j*nmat] = 0.0;
           }
        } 
        if(g) {
          g=1.0/g;
          if(i!=n-1) {
             for(j=l; j<n; j++) {
              s=0.0;
              for(k=l; k<nmat; k++){
                s += mat[k+i*nmat]*mat[k+j*nmat];
              }
              f = (s/mat[i+i*nmat])*g;
              for(k=i; k<nmat; k++){
                mat[k+j*nmat] += f*mat[k+i*nmat];
              }
             }
          } // if(i!=n-1
          for(j=i; j<nmat; j++){
            mat[j+i*nmat] *= g;
          }
        }else{
           for(j=i; j<nmat; j++){
            mat[j+i*nmat] = 0.0;
           }
        }
        mat[i+i*nmat] += 1.0;
      } // for(i=n-1
      for(k=n-1; k>=0; k--){
         for(its=0; its<30; its++){
            flag=1;
            for(l=k; l>=0; l--){
               nm=l-1;
               if ((REAL)(fabs(rv1[l])+anorm) == anorm){
                  flag=0;
                  break;
               }
               if ((REAL)(fabs(wd[nm])+anorm) == anorm) break;
            } // for(l
            if(flag) {
               c=0.0;
               s=1.0;
               for(i=l; i<=k; i++){
                  f=s*rv1[i];
                  if ((REAL)(fabs(f)+anorm)!=anorm){
                     g=wd[i];
                     h=sqrt(f*f+g*g);
                     wd[i]=h;
                     h=1.0/h;
                     c= (g*h);
                     s=-(f*h);
                     for(j=0; j<nmat; j++){
                        y = mat[j+nm*nmat];
                        z = mat[j+i*nmat];
                        mat[j+nm*nmat] = (y*c)+(z*s);
                        mat[j+i*nmat] = -(y*s)+(z*c);
                     }
                  } // if((REAL)(fabs(f)+anorm)!=anorm) 
               } // for(i
            } // if(flag)
          z=wd[k];
          if(l==k){
             if(z<0.0){
              wd[k]=-z;
              for(j=0; j<n; j++){
                vmat[j+k*nmat] = -vmat[j+k*nmat];
              }
             } // if(z<0.0
            break;
          } // if(l==k
          if(its==29){
             cout<< " no convergence in 30 svd iterations" << endl;
          }
          x=wd[l];
          nm=k-1;
          y=wd[nm];
          g=rv1[nm];
          h=rv1[k];
          f=((y-z)*(y+z)+(g-h)*(g+h))/(2.0*h*y);
          g=sqrt(f*f+1.0);
          REAL sign = (f>=0 ? fabs(g) : -fabs(g));
          f=((x-z)*(x+z)+h*((y/(f+sign))-h))/x;
          c=1.0;
          s=1.0;
          for(j=l; j<=nm; j++){
            i=j+1;
            g=rv1[i];
            y=wd[i];
            h=s*g;
            g=c*g;
            z=sqrt(f*f+h*h);
            rv1[j]=z;
            c=f/z;
            s=h/z;
            f= (x*c)+(g*s);
            g=-(x*s)+(g*c);
            h=y*s;
            y=y*c;
            for(nm1=0; nm1<n; nm1++){
              x = vmat[nm1+j*nmat];
              z = vmat[nm1+i*nmat];
              vmat[nm1+j*nmat] = (x*c)+(z*s);
              vmat[nm1+i*nmat] = -(x*s)+(z*c);
            }
            z=sqrt(f*f+h*h);
            wd[j]=z;
            if(z){
              z=1.0/z;
              c=f*z;
              s=h*z;
            }
            f= (c*g)+(s*y);
            x=-(s*g)+(c*y);
            for(nm1=0; nm1<nmat; nm1++){
              y = mat[nm1+j*nmat];
              z = mat[nm1+i*nmat];
              mat[nm1+j*nmat] = (y*c)+(z*s);
              mat[nm1+i*nmat] = -(y*s)+(z*c);
            }
          } // for(j=l
          rv1[l]=0.0;
          rv1[k]=f;
          wd[k]=x;
         } // for(its
      } // for(k=n-1
}
/////////////////////////
void matrix::back_sub(){
   int n = nmat; 
   int i, j, jj;
   REAL s;
   for(j=0; j<n; j++){
        s=0.;
        if(wd[j]){
           for(i=0; i<nmat; i++){
            s += mat[i+j*nmat]*bb[i];
           }
          s=s/wd[j];
        }
        tmp[j]=s;
   } // for(j=0
   for(j=0; j<n; j++){
        s=0.;
        for(jj=0; jj<n; jj++){
          s += vmat[j+jj*nmat]*tmp[jj];
        }
        uu[j]=s;
   } // for(j
}


