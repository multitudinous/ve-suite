/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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
#ifndef QUERY_THREAD_H
#define QUERY_THREAD_H

#include <ace/Task.h>
#include <string>

#include <ves/open/moduleS.h>

class QueryThread : public ACE_Task_Base
{
public:
    QueryThread( Body::Unit_var m );
    virtual ~QueryThread();

    virtual int svc();

    void QueryData( std::string command, CORBA::Long modId );
    bool GettingData();
    std::string GetQueryData();

private:
    Body::Unit_var _mod;
    bool isComplete;
    bool shutdown;
    CORBA::Long moduleId;
    ACE_Thread_Mutex _mutex;
    std::string queryCommand;
    std::string queryData;
};
#endif
