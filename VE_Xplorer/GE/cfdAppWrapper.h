/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
#ifndef CFD_APPWRAPPER_H
#define CFD_APPWRAPPER_H
/*!\file cfdAppWrapper.h
cfdAppWrapper API
*/

/*!\class VE_Xplorer::cfdAppWrapper
*
*/
#include <vrj/vrjParam.h>

namespace VE_Xplorer
{
   class cfdThread;
   class cfdApp;
   class cfdVjObsWrapper;
}

namespace VE_Xplorer
{
class cfdAppWrapper
{
public:
   cfdAppWrapper( int argc,  char* argv[], cfdVjObsWrapper* );
   ~cfdAppWrapper( void );
   bool JugglerIsRunning( void );
#if __VJ_version > 2000003
   void init( void );
#elif __VJ_version == 2000003
   void init( void * );
#endif
   cfdThread* _thread;
private:
   cfdApp* _cfdApp;
   bool jugglerIsRunning;
   cfdVjObsWrapper* _vjObsWrapper;
   int argc;
   char** argv;
}; 
}
#endif
