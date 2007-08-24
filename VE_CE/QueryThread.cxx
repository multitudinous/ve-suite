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
#include <ace/Task.h>
#include <ace/OS.h>
#include "VE_CE/QueryThread.h"

#include <iostream>

QueryThread::QueryThread (Body::Unit_var m ) :
  _mod       ( m ),
  isComplete( true )
{
   shutdown = false;
}
////////////////////////////////////////////////////////////////////////////////
QueryThread::~QueryThread ()
{
   shutdown = true;
}
////////////////////////////////////////////////////////////////////////////////
int QueryThread::svc( void )
{		  
    while( !shutdown ) 
    {
        while( isComplete ) 
        {
            ACE_OS::sleep(1); 	    
        }

        _mutex.acquire();
        try 
        {
            _mod->_non_existent();
            _mod->SetCurID( moduleId );
            queryData.assign( _mod->Query( CORBA::string_dup( queryCommand.c_str() ) ) );
        } 
        catch( CORBA::Exception &ex ) 
        {
            std::cout << "Module Query Messed up." << std::endl;
            std::cerr << "CORBA exception raised! : " << ex._name() << std::endl;
            std::cerr << ex._info().c_str() << std::endl;
            queryData = "NULL";
        }
        _mutex.release();
        isComplete = true;
    }
    return 1;
}
////////////////////////////////////////////////////////////////////////////////
void QueryThread::QueryData( std::string command, CORBA::Long modId )
{
   _mutex.acquire();
   queryCommand = command;
   queryData.erase();
   moduleId = modId;
   isComplete = false;
   _mutex.release();
}
////////////////////////////////////////////////////////////////////////////////
bool QueryThread::GettingData( void )
{
   return isComplete;
}
////////////////////////////////////////////////////////////////////////////////
std::string QueryThread::GetQueryData( void )
{
	_mutex.acquire();
	std::string tempData = queryData;
	_mutex.release();
	return tempData;
}

