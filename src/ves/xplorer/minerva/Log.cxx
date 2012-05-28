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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/

#include <ves/xplorer/minerva/Log.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer::minerva;

USUL_IMPLEMENT_IUNKNOWN_MEMBERS( Log, Log::BaseClass )

///////////////////////////////////////////////////////////////////////////////
Log::Log() : BaseClass()
{
}
///////////////////////////////////////////////////////////////////////////////
Log::~Log()
{
}
///////////////////////////////////////////////////////////////////////////////
Usul::Interfaces::IUnknown* Log::queryInterface( unsigned long iid )
{
    switch( iid )
    {
    case Usul::Interfaces::IUnknown::IID:
    case Usul::Interfaces::ILog::IID:
        return static_cast<Usul::Interfaces::ILog*>( this );
    default:
        return 0x0;
    }
}
///////////////////////////////////////////////////////////////////////////////
void Log::write( const std::string& s, bool appendNewLine, bool prependEventCount )
{
    vprDEBUG( vesDBG, 1 ) << s << std::endl << vprDEBUG_FLUSH;
}
