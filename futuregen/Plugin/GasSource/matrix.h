
    // class definition for matrix class

#ifndef _matrix_h_
#define _matrix_h_

#include <cstdio>
#include <cstring>
#include <string>
#include <iostream>
#include <vector>
#include <map>
#include "REAL.h"
 
class matrix
{

public:

   // mutators
  	matrix(){};
   ~matrix(){};
   void resize(int& nmat0);
   void solv_mat(REAL tol_min);
   void svd();
   void back_sub();

   // accessors
   std::vector<REAL>& get_mat() {return(mat);};
   int& get_nmat() {return(nmat);};
   std::vector<REAL>& get_bb() {return(bb);};
   std::vector<REAL>& get_uu() {return(uu);};

protected:

   std::vector<REAL> mat;
   std::vector<REAL> vmat;
   std::vector<REAL> wd;
   std::vector<REAL> rv1;
   std::vector<REAL> tmp;
   int nmat;
   std::vector<REAL> bb;
   std::vector<REAL> uu;
};

//inlines
#endif
