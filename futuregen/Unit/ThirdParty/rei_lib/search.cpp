/*==============================================================================
 * Searching algorithm that will find the i,j,k coordinates of the cell
 * that a given target node is in. Each search will return the i,j, or k 
 * (0 base referenced) cell array index in which the cfd node lies.
 * This uses a modified bisection search algorithm
 * which uses a hunting technique based on the previous search.  Copied from
 * "Numerical Recipes in C" p. 117.  We do a search for each coordinate
 * direction, hence the 3 searching routines for i, j, and k.
 *============================================================================*/

#include <stdio.h>
#include "grid.h"
#include "scalar.h"
#include "interp.h"
#include "search.h"

/* cell - is the cell struct
   ni - is the number of nodes in the i direction
   x - is the value we are searching with
   ilo - is the lower array index that bounds the value x 

   these functions return the number of nodes in the given
   direction if the value of x is greater than grid range
   and they return -1 if the value of x is less than the grid range */

void search_i(struct src_cell **rcell,unsigned int ni,float x,int *ilo)
{

   int ascnd;
   unsigned int im,ihi,inc;
   
   /* check first to see if x is out of the rad grid range */
   
   if(x > rcell[ni-1][0].x[1]){
      *ilo = ni;
       return;
   }

   if(x < rcell[0][0].x[0]){
      *ilo = -1;
      return;
   }

   /* begin searching algorithm */

   ascnd=(rcell[ni-1][0].x[1]>rcell[0][0].x[0]);  /* true is ascending order */

   if(*ilo <= -1 || (unsigned int)*ilo > (ni-1)){  /* guess is out of range, so go to bisection */
      *ilo=-1;
      ihi=ni;
   }
   else{
      inc=1;        /* set the hunting increment */
      if(x > rcell[*ilo][0].x[1] == ascnd){     /* hunt up */
         ihi=(*ilo)+1;
         while(x >= rcell[ihi][0].x[1] == ascnd){   /* not done hunting */
            *ilo=ihi;
            inc += inc;
            ihi=(*ilo)+inc;
            if(ihi>ni-1){              /* done hunting since out of range */
               ihi=ni;
               break;
            }
         }
      }
      else{                         /* hunt down */
         if(*ilo == 0){
            if(x<rcell[*ilo][0].x[0]) 
               *ilo=-1;
            return;
         } 
         ihi=(*ilo)--;
         while(x < rcell[*ilo][0].x[0] == ascnd){
            ihi=(*ilo);
            inc <<=1;
            if(inc >= ihi){
               *ilo=-1;
                break;
            }
            else *ilo=ihi-inc;
         }
      }
   }

/* do the bisection */
   while(ihi-(*ilo) != 1){
      im=(ihi+((unsigned int)*ilo)) >> 1;
      if(x > rcell[im][0].x[1] == ascnd)
         *ilo=im;
      else
         ihi=im;
   }

/* now get the actual rad cell array location based on ilo and ihi */
   if(ihi!=ni){
      if(x >= rcell[ihi][0].x[0])
         *ilo=ihi;
   }

}

/*****************************************************************/

void search_j(struct src_cell **rcell,unsigned int nj,float y,int *jlo)
{

   unsigned int jm,jhi,inc;
   int ascnd;

/* check first to see if x is out of the rad grid range */
   if(y > rcell[0][nj-1].y[1]){
      *jlo = nj;
       return;
   }
   if(y < rcell[0][0].y[0]){
      *jlo = -1;
      return;
   }

   ascnd=(rcell[0][nj-1].y[1] > rcell[0][0].y[0]);  /* true is ascending order */
   if(*jlo <= -1 || (unsigned int)*jlo > (nj-1)){  /* guess is out of range, so go to bisection */
      *jlo=-1;
      jhi=nj;
   }
   else{
      inc=1;        /* set the hunting increment */
      if(y > rcell[0][*jlo].y[1] == ascnd){     /* hunt up */
         jhi=(*jlo)+1;
         while(y >= rcell[0][jhi].y[1] == ascnd){   /* not done hunting */
            *jlo=jhi;
            inc += inc;
            jhi=(*jlo)+inc;
            if(jhi>nj-1){              /* done hunting since out of range */
               jhi=nj;
               break;
            }
         }
      }
      else{                         /* hunt down */
         if(*jlo == 0){
            if(y<rcell[0][*jlo].y[0])     /* check if y is out of range */
               *jlo=-1;
            return;
         }
         jhi=(*jlo)--;
         while(y < rcell[0][*jlo].y[0] == ascnd){
            jhi=(*jlo);
            inc <<=1;
            if(inc >= jhi){
               *jlo=-1;
                break;
            }
            else *jlo=jhi-inc;
         }
      }
   }
   while(jhi-(*jlo) != 1){
      jm=(jhi+((unsigned int)*jlo)) >> 1;
      if(y > rcell[0][jm].y[1] == ascnd)
         *jlo=jm;
      else
         jhi=jm;
   }

/* now get the array location based on jlo and jhi */
   if(jhi!=nj){
      if(y >= rcell[0][jhi].y[0])
         *jlo=jhi;
   }

}
