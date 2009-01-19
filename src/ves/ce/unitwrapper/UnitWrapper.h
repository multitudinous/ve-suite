/*************** <auto-copyright.rb BEGIN do not edit this line> *************
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
 *************** <auto-copyright.rb END do not edit this line> **************/
// TODO: Fill in documentation, document the virtual functions.
#ifndef UNIT_WRAPPER_H_
#define UNIT_WRAPPER_H_

#include <ves/open/moduleS.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/XMLObjectPtr.h>
#include <ves/VEConfig.h>
#include <vector>
#include <map>
#include <string>

namespace VE_CE
{
class EventHandler;
}

///??
class  VE_CE_UNIT_WRAPPER_EXPORTS UnitWrapper : public virtual POA_Body::Unit
{
public:
    ///Constructor
    UnitWrapper( Body::Executive_ptr exec, std::string name );
    ///Default constructor
    UnitWrapper();
    ///Destructor
    virtual ~UnitWrapper( void );

protected:
    Body::Executive_var executive_;
    unsigned int return_state;
    ///??
    std::string UnitName_;
    //::Types::ArrayLong_var id_;
    ///??
    CORBA::Long activeId;
    ///??
    std::string status_;
    ///??
    std::string data_;
    ///??
    std::map< std::string, ves::open::xml::model::ModelPtr > xmlModelMap;
    ///??
    std::map< std::string, std::vector< ves::open::xml::XMLObjectPtr > > inputsMap;
    ///??
    std::map< std::string, std::vector< ves::open::xml::XMLObjectPtr > > resultsMap;
    ///??
    std::map< std::string, VE_CE::EventHandler* > eventHandlerMap;

public:

    virtual
    void StartCalc(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void StopCalc(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void PauseCalc(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void Resume(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    char * GetStatusMessage(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    char * GetUserData(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void SetParams(
        ::CORBA::Long module_id,
        const char * param
    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void SetID(
        ::CORBA::Long id
    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void SetCurID(
        ::CORBA::Long id
    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    ::Types::ArrayLong * GetID(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    ::CORBA::Long GetCurID(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown

                   ) );
    virtual
    void SetName(
        const char * name
    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    char * GetName(

    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    char * Query( const char * command

                )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );

    virtual
    void DeleteModuleInstance(
        ::CORBA::Long module_id
    )
    ACE_THROW_SPEC((
                       ::CORBA::SystemException,
                       ::Error::EUnknown
                   ) );
};


#endif /* UNIT_WRAPPER_H_  */

