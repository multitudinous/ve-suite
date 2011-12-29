/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
#ifndef VES_CE_BODY_AMH_EXECUTIVE_I_H
#define VES_CE_BODY_AMH_EXECUTIVE_I_H

#include <ves/open/moduleC.h>
#include <ves/open/moduleS.h>

#include <string>
#include <vector>
#include <map>

#include <ves/VEConfig.h>

class Execute_Thread;

namespace VE_CE
{
namespace Utilities
{
class Network;
class Scheduler;
}
}

namespace ves
{
namespace ce
{
class VE_CE_EXPORTS Body_AMH_Executive_i 
    : 
    public virtual POA_Body::AMH_Executive
{
public:
    Body_AMH_Executive_i(PortableServer::POA_ptr poa);
    
    virtual ~Body_AMH_Executive_i(void);
    
    /*virtual void GetImportData (
                                Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                ::CORBA::Long module_id,
                                ::CORBA::Long port_id
                                ) ;*/

    virtual void SetModuleMessage (
                                   Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                   ::CORBA::Long module_id,
                                   const char * msg
                                   ) ;

    /*virtual void SetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id,
                                  const char * result
                                  ) ;

    virtual void GetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id
                                  ) ;*/
    
    virtual void SetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * network
                             ) ;
    
    virtual void SetModuleUI (
                              Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                              ::CORBA::Long module_id,
                              const char * ui
                              ) ;
    
    virtual void GetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * moduleName
                             ) ;
    
    virtual void SetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const ::Types::ArrayLong & id
                               ) ;
    
    virtual void GetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) ;
    
    virtual void GetStatus (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) ;

    virtual void StartCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) ;
 
    virtual void StopCalc (
                           Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                           ) ;
    
    virtual void PauseCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) ;
    
    virtual void Resume (
                         Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                         ) ;
    
    virtual void RegisterUI (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * UIName,
                             ::Body::UI_ptr ui
                             ) ;
    
    virtual void UnRegisterUI (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UIName
                               ) ;
    
    virtual void UnRegisterUnit (
                                 Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                 const char * UnitName
                                 ) ;
    
    virtual void RegisterUnit (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UnitName,
                               ::Body::Unit_ptr unit,
                               ::CORBA::Long flag
                               ) ;
    
    /*virtual void GetGlobalMod (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) ;*/

    virtual void Query (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * commands
                        ) ;
    
    
    virtual void SetID (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * moduleName,
                        ::CORBA::Long id
                        ) ;
    
    virtual void DeleteModuleInstance (
                                       Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                       const char * moduleName,
                                       ::CORBA::Long module_id
                                       ) ;
    
    virtual void SetParams (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                            const char * moduleName,
                            ::CORBA::Long module_id,
                            const char * param
                            ) ;

    void execute_next_mod( long module_id );

private:
    std::map< std::string, ::Body::Unit_var > m_modUnits;
    ///Map to store connections from all of the VE-Xplorer and VE-Conductor
    ///UI interfaces
    std::map<std::string, ::Body::UI_var> m_uiMap;
    
    std::map< std::string, Execute_Thread* > m_execThread;
    //std::map< std::string, QueryThread* > m_queryThreads;

    PortableServer::POA_var m_poa;
    VE_CE::Utilities::Network*   m_network;
    VE_CE::Utilities::Scheduler* m_scheduler;

    ///AMI handler for asynchronous calls to conductor
    //Body_AMI_UIHandler_i m_uiAMIHandler;
    
    Types::ArrayLong m_watchList;
    
    ACE_Thread_Mutex m_mutex;
    ACE_Thread_Mutex m_query;

    std::string GetResults( int rt );
    
    void ClientMessage( const char *msg );
    
    void execute( std::string );
};
}
}
#endif
