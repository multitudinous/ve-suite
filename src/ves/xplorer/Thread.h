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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_XPLORER_THREAD_H
#define VE_XPLORER_THREAD_H

#include <ves/xplorer/ThreadPtr.h>
#include <ves/VEConfig.h>

#include <vpr/Thread/Thread.h>

namespace ves
{
namespace xplorer
{
/*!\file Thread.h
Thread API
*/
/*!\class ves::xplorer::Thread
* 
*/
class VE_XPLORER_EXPORTS Thread
{
public:
  ///Constructor
   Thread( void );
 ///Destructor
   ~Thread( void );

   //   private:
   //vpr::ThreadMemberFunctor<cfdAppWrapper>* corba_run;
   //vpr::ThreadMemberFunctor<cfdExecutive>* executive_run;
   vpr::Thread* new_thread;

};
}
}
#endif
