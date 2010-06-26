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
#ifndef BODY_AMH_VEXPLORER_I_H
#define BODY_AMH_VEXPLORER_I_H
/*!\file Body_AMH_VEXplorerResponseHandler_i.h
 *Interface for sending information from Xplorer/CE Asynchronously to Condutor
 */

/*!\class Body_AMH_VEXplorerResponseHandler_i
 *
 */

#include <ves/open/moduleS.h>

#include <ves/VEConfig.h>

class VE_OPEN_MODULE_EXPORTS Body_AMH_VEXplorerResponseHandler_i
: public virtual Body::AMH_VEXplorerResponseHandler,
public virtual TAO_Local_RefCounted_Object
{
public:
    // Constructor 
    Body_AMH_VEXplorerResponseHandler_i (void);
    
    // Destructor 
    virtual ~Body_AMH_VEXplorerResponseHandler_i (void);
    
    virtual
    void GetStatusMessage (
                           const char * return_value) = 0;
    
    virtual
    void GetStatusMessage_excep (
                                 ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void SetParams (
                    void) = 0;
    
    virtual
    void SetParams_excep (
                          ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void SetID (
                void) = 0;
    
    virtual
    void SetID_excep (
                      ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void GetID (
                ::CORBA::Long return_value) = 0;
    
    virtual
    void GetID_excep (
                      ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void SetName (
                  void) = 0;
    
    virtual
    void SetName_excep (
                        ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void GetName (
                  const char * return_value) = 0;
    
    virtual
    void GetName_excep (
                        ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void SetNetwork (
                     void) = 0;
    
    virtual
    void SetNetwork_excep (
                           ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void SetCommand (
                     void) = 0;
    
    virtual
    void SetCommand_excep (
                           ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void RegisterUI (
                     void) = 0;
    
    virtual
    void RegisterUI_excep (
                           ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
    
    virtual
    void UnRegisterUI (
                       void) = 0;
    
    virtual
    void UnRegisterUI_excep (
                             ::Body::AMH_VEXplorerExceptionHolder * holder) = 0;
};

#endif

