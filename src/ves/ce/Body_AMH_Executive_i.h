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
#ifndef BODY_AMH_EXECUTIVE_I_H
#define BODY_AMH_EXECUTIVE_I_H

#include <ves/open/moduleS.h>

#include <ves/VEConfig.h>

class VE_OPEN_MODULE_EXPORTS Body_AMH_Executive_i
: public virtual POA_Body::AMH_Executive
{
public:
    Body_AMH_Executive_i(PortableServer::POA_ptr poa, POA_Body::Unit_ptr unitModule);
    
    virtual ~Body_AMH_Executive_i(void);
    
    virtual void GetImportData (
                                Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                ::CORBA::Long module_id,
                                ::CORBA::Long port_id
                                ) = 0;

    virtual void SetModuleMessage (
                                   Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                   ::CORBA::Long module_id,
                                   const char * msg
                                   ) = 0;

    virtual void SetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id,
                                  const char * result
                                  ) = 0;

    virtual void GetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id
                                  ) = 0;
    
    virtual void SetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * network
                             ) = 0;
    
    virtual void SetModuleUI (
                              Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                              ::CORBA::Long module_id,
                              const char * ui
                              ) = 0;
    
    virtual void GetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * moduleName
                             ) = 0;
    
    virtual void SetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const ::Types::ArrayLong & id
                               ) = 0;
    
    virtual void GetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) = 0;
    
    virtual void GetStatus (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) = 0;

    virtual void StartCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) = 0;
 
    virtual void StopCalc (
                           Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                           ) = 0;
    
    virtual void PauseCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) = 0;
    
    virtual void Resume (
                         Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                         ) = 0;
    
    virtual void RegisterUI (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * UIName,
                             ::Body::UI_ptr ui
                             ) = 0;
    
    virtual void UnRegisterUI (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UIName
                               ) = 0;
    
    virtual void UnRegisterUnit (
                                 Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                 const char * UnitName
                                 ) = 0;
    
    virtual void RegisterUnit (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UnitName,
                               ::Body::Unit_ptr unit,
                               ::CORBA::Long flag
                               ) = 0;
    
    virtual void GetGlobalMod (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) = 0;

    virtual void Query (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * commands
                        ) = 0;
    
    
    virtual void SetID (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * moduleName,
                        ::CORBA::Long id
                        ) = 0;
    
    virtual void DeleteModuleInstance (
                                       Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                       const char * moduleName,
                                       ::CORBA::Long module_id
                                       ) = 0;
    
    virtual void SetParams (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                            const char * moduleName,
                            ::CORBA::Long module_id,
                            const char * param
                            ) = 0;
    
};

#endif