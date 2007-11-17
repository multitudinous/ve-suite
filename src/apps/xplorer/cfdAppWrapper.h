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
#ifndef CFD_APPWRAPPER_H
#define CFD_APPWRAPPER_H
/*!\file cfdAppWrapper.h
cfdAppWrapper API
*/

/*!\class ves::xplorer::cfdAppWrapper
*
*/
#include <vrj/vrjParam.h>

#include <ves/xplorer/ThreadPtr.h>

namespace ves
{
namespace xplorer
{
   class cfdApp;
   class cfdVjObsWrapper;
}
}

namespace ves
{
namespace xplorer
{
class cfdAppWrapper
{
public:
   ///Contructor
   cfdAppWrapper( int argc,  char* argv[], cfdVjObsWrapper* );
   ///destructor
   ~cfdAppWrapper( void );
   ///Is juggler running
   bool JugglerIsRunning( void );
   ///Initilize things in a seperate thread
#if __VJ_version > 2000003
   void init( void );
#elif __VJ_version == 2000003
   void init( void * );
#endif
   Thread* m_thread;///< the thread to run things in
private:
   cfdApp* m_cfdApp;///< the app that is running
   bool m_jugglerIsRunning;///< is juggler running
   cfdVjObsWrapper* m_vjObsWrapper;///< points to the wrapper to send to cfdapp
   int m_argc;///<command line args
   char** m_argv;///<command line args
}; 
}
}
#endif
