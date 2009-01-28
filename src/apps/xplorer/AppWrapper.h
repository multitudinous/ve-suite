/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef CFD_APPWRAPPER_H
#define CFD_APPWRAPPER_H

// --- VR Juggler Includes --- //
#include <vrj/vrjParam.h>

namespace ves
{
namespace xplorer
{

class App;
class VjObsWrapper;

/*!\file AppWrapper.h
 *
 */

/*!\class ves::xplorer::AppWrapper
 *
 */

/*!\namespace ves::xplorer
 *
 */

class AppWrapper
{
public:
    ///Contructor
    AppWrapper( int argc,  char* argv[], VjObsWrapper* input );

    ///destructor
    ~AppWrapper();

    ///Is juggler running
    bool JugglerIsRunning();

    ///Initilize things in a seperate thread
#if __VJ_version > 2000003
    void init();
#elif __VJ_version == 2000003
    void init( void* );
#endif

private:
    ///The app that is running
    ves::xplorer::App* m_cfdApp;

    ///Is juggler running
    bool m_jugglerIsRunning;

    ///Points to the wrapper to send to cfdapp
    ves::xplorer::VjObsWrapper* m_vjObsWrapper;

    ///Command line args
    int m_argc;

    ///Command line args
    char** m_argv;

};
} //end xplorer
} //end ves

#endif //CFD_APPWRAPPER_H
