#pragma once
/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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
 * Date modified: $Date: 2012-10-29 18:37:56 -0500 (Mon, 29 Oct 2012) $
 * Version:       $Rev: 17252 $
 * Author:        $Author: mccdo $
 * Id:            $Id: EventFactory.h 17252 2012-10-29 23:37:56Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <switchwire/Version.h>
#if SWITCHWIRE_HAVE_SQUIRREL

#include <string>

namespace Sqrat
{
 class SqratVM;
}

namespace ves
{
namespace xplorer
{
namespace eventmanager
{
class SquirrelConnection
{
public:
    SquirrelConnection();
    SquirrelConnection( const std::string& scriptText );

    void ExposeSignalTypes( Sqrat::SqratVM& vm );
    void BindSpecialClasses();
    void runScript( const std::string& scriptText );
};
}}} //ves::xplorer::eventmanager
#endif
