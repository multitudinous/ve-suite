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
#include <ves/ce/Body_AMH_Executive_i.h>

#include <ace/Task.h>
#include <ace/OS.h>
#include <ves/ce/Execute_Thread.h>

#include <iostream>
using namespace ves::ce;

Execute_Thread::Execute_Thread( Body::Unit_var m, Body_AMH_Executive_i* ex ) :
        _mod( m ),
        _is_exec( false ),
        _executive( ex ),
        m_isRunning( false )
{}
////////////////////////////////////////////////////////////////////////////////
Execute_Thread::~Execute_Thread()
{
    _mod = 0;
    _executive = 0;
    m_isRunning = false;
}
////////////////////////////////////////////////////////////////////////////////
int Execute_Thread::close(u_long flags)
{
    m_isRunning = false;
    ACE_OS::sleep( 2 );
    return 0;
    return ACE_Task_Base::close(flags);
}
////////////////////////////////////////////////////////////////////////////////
int Execute_Thread::svc( void )
{
    m_isRunning = true;
    while( m_isRunning )
    {
        while( m_isRunning )
        {
            _mutex.acquire();
            if( _is_exec )
                break;

            _mutex.release();

            ACE_OS::sleep( 1 );
        }

        if( !m_isRunning )
        {
            return 0;
        }

        _mutex.release();
        try
        {
            _mod->StartCalc();
        }
        catch ( CORBA::Exception & )
        {
            std::cout << "Module Execution Messed up." << std::endl;
        }

        _mutex.acquire();
        _is_exec = false;
        _mutex.release();

        try
        {
            // This function returns the id of the currently executed module
            long id = static_cast< long >( _mod->GetCurID() );
            _executive->execute_next_mod( id );
        }
        catch ( CORBA::Exception & )
        {
            std::cout << "Module GetID Messed up." << std::endl;
        }
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int Execute_Thread::lock ()
{
    _mutex.acquire();
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int Execute_Thread::unlock()
{
    _mutex.release();
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
int Execute_Thread::needexecute()
{
    int ret = 1;
    _mutex.acquire();
    if( _is_exec == true )
    {
        ret = 0;
    }
    else
    {
        _is_exec = true;
    }
    _mutex.release();

    return ret;
}

