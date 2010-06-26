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
/*!\file Body_AMH_ExecutiveResponseHandler_i.h
 *Interface for sending information from Xplorer/CE Asynchronously to Condutor
 */

/*!\class Body_AMH_ExecutiveResponseHandler_i
 *
 */

#include <ves/open/moduleS.h>

#include <ves/VEConfig.h>

class VE_OPEN_MODULE_EXPORTS Body_AMH_ExecutiveResponseHandler_i
: public virtual Body::AMH_ExecutiveResponseHandler,
public virtual TAO_Local_RefCounted_Object
{
public:
    // Constructor 
    Body_AMH_ExecutiveResponseHandler_i (void);
    
    // Destructor 
    virtual ~Body_AMH_ExecutiveResponseHandler_i (void);
    
    virtual
    void GetImportData (
                        const char * return_value) = 0;
    
    virtual
    void GetImportData_excep (
                              ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetModuleMessage (
                           void) = 0;
    
    virtual
    void SetModuleMessage_excep (
                                 ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetModuleResult (
                          void) = 0;
    
    virtual
    void SetModuleResult_excep (
                                ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void GetModuleResult (
                          const char * return_value) = 0;
    
    virtual
    void GetModuleResult_excep (
                                ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetNetwork (
                     void) = 0;
    
    virtual
    void SetNetwork_excep (
                           ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetModuleUI (
                      void) = 0;
    
    virtual
    void SetModuleUI_excep (
                            ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void GetNetwork (
                     const char * return_value) = 0;
    
    virtual
    void GetNetwork_excep (
                           ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetWatchList (
                       void) = 0;
    
    virtual
    void SetWatchList_excep (
                             ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void GetWatchList (
                       const ::Types::ArrayLong & return_value) = 0;
    
    virtual
    void GetWatchList_excep (
                             ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void GetStatus (
                    const char * return_value) = 0;
    
    virtual
    void GetStatus_excep (
                          ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void StartCalc (
                    void) = 0;
    
    virtual
    void StartCalc_excep (
                          ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void StopCalc (
                   void) = 0;
    
    virtual
    void StopCalc_excep (
                         ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void PauseCalc (
                    void) = 0;
    
    virtual
    void PauseCalc_excep (
                          ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void Resume (
                 void) = 0;
    
    virtual
    void Resume_excep (
                       ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void RegisterUI (
                     void) = 0;
    
    virtual
    void RegisterUI_excep (
                           ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void UnRegisterUI (
                       void) = 0;
    
    virtual
    void UnRegisterUI_excep (
                             ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void UnRegisterUnit (
                         void) = 0;
    
    virtual
    void UnRegisterUnit_excep (
                               ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void RegisterUnit (
                       void) = 0;
    
    virtual
    void RegisterUnit_excep (
                             ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void GetGlobalMod (
                       ::CORBA::Long return_value,
                       const ::Types::ArrayLong & ids) = 0;
    
    virtual
    void GetGlobalMod_excep (
                             ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void Query (
                const char * return_value) = 0;
    
    virtual
    void Query_excep (
                      ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetID (
                void) = 0;
    
    virtual
    void SetID_excep (
                      ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void DeleteModuleInstance (
                               void) = 0;
    
    virtual
    void DeleteModuleInstance_excep (
                                     ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
    
    virtual
    void SetParams (
                    void) = 0;
    
    virtual
    void SetParams_excep (
                          ::Body::AMH_ExecutiveExceptionHolder * holder) = 0;
};

#endif
