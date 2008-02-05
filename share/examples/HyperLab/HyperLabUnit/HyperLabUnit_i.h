/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * Date modified: $Date: 2006-07-08 22:04:36 -0500 (Sat, 08 Jul 2006) $
 * Version:       $Rev: 4905 $
 * Author:        $Author: mccdo $
 * Id:            $Id: TEMPLATE_i.h 4905 2006-07-09 03:04:36Z mccdo $
 * -----------------------------------------------------------------
 *
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef UNIT_I_H
#define UNIT_I_H

// --- VE-Suite Includes --- //
#include <ves/open/moduleS.h>

// --- C/C++ Libraries --- //
#include <string>

class Body_Unit_i : public virtual POA_Body::Unit
{
public:
    Body_Unit_i( Body::Executive_ptr exec, std::string name ); 
    virtual ~Body_Unit_i();  
    std::string UnitName_;
    CORBA::Long id_;
    std::string status_;
    std::string data_;

protected:
    Body::Executive_var executive_;
    int return_state;
    void error( std::string msg );
    void warning( std::string msg );

    std::string sensorData;
  
public:
    virtual void StartCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void StopCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void PauseCalc( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void Resume( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual char* GetStatusMessage( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual char* GetUserData( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void SetParams( CORBA::Long id, const char* param ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void SetID( CORBA::Long id ACE_ENV_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void SetCurID( CORBA::Long id )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual Types::ArrayLong* GetID( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual CORBA::Long GetCurID()
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );  

    virtual void SetName( const char* name ACE_ENV_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual char* GetName( ACE_ENV_SINGLE_ARG_DECL )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual char* Query( const char* commands )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );

    virtual void DeleteModuleInstance( CORBA::Long module_id )
        ACE_THROW_SPEC( ( CORBA::SystemException, Error::EUnknown ) );
};

#endif