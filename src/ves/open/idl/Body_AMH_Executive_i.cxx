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

#include <ves/open/idl/Body_AMH_Executive_i.h>

Body_AMH_Executive_i::Body_AMH_Executive_i(PortableServer::POA_ptr poa, POA_Body::Unit_ptr unitModule)
{
    ;
}

Body_AMH_Executive_i::~Body_AMH_Executive_i()
{
    ;
}

    void Body_AMH_Executive_i::GetImportData (
                                Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                ::CORBA::Long module_id,
                                ::CORBA::Long port_id
                                ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetModuleMessage (
                                   Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                   ::CORBA::Long module_id,
                                   const char * msg
                                   ) 
{
    ;
}

    void Body_AMH_Executive_i::SetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id,
                                  const char * result
                                  ) 
{
    ;
}
    
    void Body_AMH_Executive_i::GetModuleResult (
                                  Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                  ::CORBA::Long module_id
                                  ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * network
                             ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetModuleUI (
                              Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                              ::CORBA::Long module_id,
                              const char * ui
                              ) 
{
    ;
}
    
    void Body_AMH_Executive_i::GetNetwork (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * moduleName
                             ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const ::Types::ArrayLong & id
                               ) 
{
    ;
}
    
    void Body_AMH_Executive_i::GetWatchList (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) 
{
    ;
}

    void Body_AMH_Executive_i::GetStatus (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) 
{
    ;
}
    
    
    void Body_AMH_Executive_i::StartCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) 
{
    ;
}
        
    void Body_AMH_Executive_i::StopCalc (
                           Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                           ) 
{
    ;
}
    
    
    void Body_AMH_Executive_i::PauseCalc (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                            ) 
{
    ;
}

    void Body_AMH_Executive_i::Resume (
                         Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                         ) 
{
    ;
}
    
    void Body_AMH_Executive_i::RegisterUI (
                             Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                             const char * UIName,
                             ::Body::UI_ptr ui
                             ) 
{
    ;
}
    
    void Body_AMH_Executive_i::UnRegisterUI (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UIName
                               ) 
{
    ;
}

    void Body_AMH_Executive_i::UnRegisterUnit (
                                 Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                 const char * UnitName
                                 ) 
{
    ;
}
    
    void Body_AMH_Executive_i::RegisterUnit (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                               const char * UnitName,
                               ::Body::Unit_ptr unit,
                               ::CORBA::Long flag
                               ) 
{
    ;
}
    
    void Body_AMH_Executive_i::GetGlobalMod (
                               Body::AMH_ExecutiveResponseHandler_ptr _tao_rh
                               ) 
{
    ;
}
    
    void Body_AMH_Executive_i::Query (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * commands
                        ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetID (
                        Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                        const char * moduleName,
                        ::CORBA::Long id
                        ) 
{
    ;
}
    
    void Body_AMH_Executive_i::DeleteModuleInstance (
                                       Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                                       const char * moduleName,
                                       ::CORBA::Long module_id
                                       ) 
{
    ;
}
    
    void Body_AMH_Executive_i::SetParams (
                            Body::AMH_ExecutiveResponseHandler_ptr _tao_rh,
                            const char * moduleName,
                            ::CORBA::Long module_id,
                            const char * param
                            )
{
    ;
}
