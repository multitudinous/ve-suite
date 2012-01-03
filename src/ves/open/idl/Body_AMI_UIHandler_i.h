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
#ifndef BODY_AMI_UI_HANDLER_I_H
#define BODY_AMI_UI_HANDLER_I_H
/*!\file Body_AMI_UIHandler_i.h
*Interface for sending information from Xplorer/CE Asynchronously to Condutor
*/

/*!\class Body_AMI_UIHandler_i
*
*/

#include <ves/open/moduleS.h>

#include <ves/VEConfig.h>

class VE_OPEN_MODULE_EXPORTS Body_AMI_UIHandler_i: public virtual POA_Body::AMI_UIHandler
{
public:
    ///Constructor
    Body_AMI_UIHandler_i( void );

    Body_AMI_UIHandler_i(PortableServer::POA_ptr p,
                      Body::AMH_ExecutiveResponseHandler_ptr rh);

    ///Destructor
    virtual ~Body_AMI_UIHandler_i( void );

    ///Update the Network from Xplorer
    virtual
    void UpdateNetwork();

    ///Update the Network from Xplorer
    virtual
    void UpdateNetwork_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );

    ///???
    virtual
    void UpdateModuleUI( );

    ///???
    virtual
    void UpdateModuleUI_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );

    ///???
    virtual
    void UpdateModuleResult( );

    ///???
    virtual
    void UpdateModuleResult_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );

    ///???
    virtual
    void UpdateLinkContent( );

    ///???
    virtual
    void UpdateLinkContent_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );

    ///???
    virtual
    void Raise( );

    ///???
    virtual
    void Raise_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );
    ///Set the Xplorer data for Conductor to retrieve
    virtual
    void SetXplorerData( );
    ///Set the Xplorer data for Conductor to retrieve
    virtual
    void SetXplorerData_excep(
        ::Messaging::ExceptionHolder * excep_holder
    );
                   
    virtual
    void SetCommand (
                     void);
    
    virtual
    void SetCommand_excep (
                           ::Messaging::ExceptionHolder * excep_holder);
    
};
#endif// BODY_AMI_UI_HANDLER_I_H
