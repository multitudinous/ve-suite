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
#ifndef BODY_AMI_UNIT_HANDLER_I_H
#define BODY_AMI_UNIT_HANDLER_I_H
/*!\file Body_AMI_UnitHandler_i.h
 *Interface for sending information from Xplorer/CE Asynchronously to Condutor
 */

/*!\class Body_AMI_UnitHandler_i
 *
 */

#include <ves/open/moduleS.h>

#include <ves/VEConfig.h>

class VE_OPEN_MODULE_EXPORTS Body_AMI_UnitHandler_i
: public virtual POA_Body::AMI_UnitHandler
{
public:
    // Constructor 
    Body_AMI_UnitHandler_i();

    Body_AMI_UnitHandler_i(PortableServer::POA_ptr p,
                         Body::AMH_ExecutiveResponseHandler_ptr rh);
    
    // Destructor 
    virtual ~Body_AMI_UnitHandler_i();
    
    virtual
    void StartCalc();
    
    virtual
    void StartCalc_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void StopCalc();
    
    virtual
    void StopCalc_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void PauseCalc();
    
    virtual
    void PauseCalc_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void Resume();
    
    virtual
    void Resume_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void GetStatusMessage(const char * ami_return_val);
    
    virtual
    void GetStatusMessage_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void GetUserData(const char * ami_return_val);
    
    virtual
    void GetUserData_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void SetParams();
    
    virtual
    void SetParams_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void SetID();
    
    virtual
    void SetID_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void SetCurID();
    
    virtual
    void SetCurID_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void GetID(const ::Types::ArrayLong & ami_return_val);
    
    virtual
    void GetID_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void GetCurID(::CORBA::Long ami_return_val);
    
    virtual
    void GetCurID_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void SetName();
    
    virtual
    void SetName_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void GetName(const char * ami_return_val);
    
    virtual
    void GetName_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void Query(const char * ami_return_val);
    
    virtual
    void Query_excep(::Messaging::ExceptionHolder * excep_holder);
    
    virtual
    void DeleteModuleInstance();
    
    virtual
    void DeleteModuleInstance_excep(
        ::Messaging::ExceptionHolder * excep_holder);

private:
    PortableServer::POA_var m_poa;
    Body::AMH_ExecutiveResponseHandler_var m_responseHandler;
};
#endif

