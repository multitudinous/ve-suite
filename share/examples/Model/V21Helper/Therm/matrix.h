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
