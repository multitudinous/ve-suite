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
 * File:          $RCSfile: cfdGlobalBase.h,v $
 * Date modified: $Date: 2004-05-18 15:44:18 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_GLOBAL_BASE_H
#define CFD_GLOBAL_BASE_H

class cfdCommandArray;

class cfdGlobalBase
{
 public:
   cfdGlobalBase ();
   virtual ~cfdGlobalBase ();

   // this abstract base class declares some pure virtual int functions to be
   // specified in concrete implementations

   // compare VjObs_i commandArray with its child's value
   virtual bool CheckCommandId( cfdCommandArray * _cfdCommandArray ) = 0;

   // in future, multi-threaded apps will make a copy of VjObs_i commandArray
   virtual void UpdateCommand() = 0;

 protected:

   // cfdApp side variables declared in VjObs_i.h
   cfdCommandArray * _cfdCommandArray;

 private:
};

#endif

