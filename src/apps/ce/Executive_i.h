/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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
#ifndef EXECUTIVE_I_H_
#define EXECUTIVE_I_H_

#include <ves/ce/util/Network.h>
#include <ves/ce/util/Scheduler.h>
#include <ves/ce/util/Module.h>
#include <ves/ce/util/Connection.h>
#include <ves/ce/util/IPort.h>
#include <ves/ce/util/OPort.h>

#include <ves/open/moduleS.h>

#include <ves/open/idl/Body_AMI_UIHandler_i.h>

#include <string>
#include <vector>
#include <map>

#include <orbsvcs/CosNamingC.h>

//Class Body_Executive_i

class QueryThread;
class Execute_Thread;

class  Body_Executive_i : public virtual POA_Body::Executive
{
public:
    //Constructor
    Body_Executive_i( CosNaming::NamingContext_ptr nc );

    //Destructor
    virtual ~Body_Executive_i( void );

    std::string GetResults( int rt );

    void execute_next_mod( long module_id );

protected:


    void execute( std::string );

    std::map< std::string, Body::Unit_var > _mod_units;

    std::map< std::string, Execute_Thread* > _exec_thread;
    std::map< std::string, QueryThread* > queryThreads;

    CosNaming::NamingContext_var naming_context_;
    ///Map to store connections from all of the VE-Xplorer and VE-Conductor
    ///UI interfaces
    std::map<std::string, Body::UI_var> m_uiMap;
    ///AMI handler for asynchronous calls to conductor
    Body_AMI_UIHandler_i m_uiAMIHandler;

    VE_CE::Utilities::Network*   _network;
    VE_CE::Utilities::Scheduler* _scheduler;

    // For tracking power requirements/generated and plant efficiencies
    std::map<long, double> _module_powers;
    std::map<long, double> _thermal_input;

    Types::ArrayLong watch_list_;

    ACE_Thread_Mutex _mutex;
    ACE_Thread_Mutex query;

public:

    virtual void SetModuleMessage(
        CORBA::Long module_id,
        const char * msg
    );

    virtual void SetNetwork(
        const char * network
    );

    virtual char * GetNetwork( const char* moduleName
                             );

    virtual void SetModuleUI(
        CORBA::Long module_id,
        const char * ui
    );

    virtual void SetWatchList(
        const Types::ArrayLong & id
    );

    virtual ::Types::ArrayLong * GetWatchList(
    );

    virtual char * GetStatus(
    );

    virtual void StartCalc(
    );

    virtual void StopCalc(
    );

    virtual void PauseCalc(
    );

    virtual void Resume(
    );

    virtual char * Query( const char * command
                        );

    virtual void RegisterUI(
        const char * UIName,
        Body::UI_ptr ui
    );

    virtual void RegisterUnit(
        const char * UnitName,
        Body::Unit_ptr unit,
        CORBA::Long module_id
    );

    virtual void UnRegisterUI(
        const char * UIName
    );

    virtual void UnRegisterUnit(
        const char * UnitName
    );

    virtual void SetID( const char * moduleName, ::CORBA::Long id );

    virtual void DeleteModuleInstance( const char * moduleName, ::CORBA::Long module_id );

    virtual void SetParams( const char * moduleName, ::CORBA::Long module_id, const char * param );

    void ClientMessage( const char *msg );

};

#endif
