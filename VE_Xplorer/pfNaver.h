/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: pfNaver.h,v $
 * Date modified: $Date: 2004/03/23 16:29:20 $
 * Version:       $Revision: 1.2 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef PF_NAVER_H_
#define PF_NAVER_H_

//#include <vrj/Kernel/Kernel.h>
//#include <Performer/pf/pfDCS.h>
//#include <Kernel/Pf/vjPfUtil.h>
#include <vrj/Kernel/Kernel.h>//chang  by Gengxun
#include <Performer/pf/pfDCS.h>
#include <vrj/Draw/Pf/PfApp.h> 
//#include <Kernel/Pf/vjPfUtil.h> //delete by Gengxun
#include <Performer/pfutil.h>
using namespace  vrj;//add by Gengxun

class pfNaver : public pfDCS
{
 public:
  pfNaver();

  virtual int app(pfTraverser*);
  virtual int needsApp(void) {return TRUE;}

   static void init();
   static pfType* getClassType(void){ return classType;}

 private:
  vrj::Kernel *mKern;
  static pfType *classType;
};


#endif
