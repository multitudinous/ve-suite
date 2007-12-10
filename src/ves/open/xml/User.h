/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _XML_VE_USER_H_
#define _XML_VE_USER_H_
/*!\file User.h
  User description.
  */
/*!\class VE_XML::User
 * This class manages information describing a user.
 */
#include <ves/open/xml/StateInfoPtr.h>
#include <xercesc/dom/DOM.hpp>

#include <iostream>

#include <ves/open/xml/XMLObject.h>
namespace ves
{
namespace open
{
namespace xml
{
class VE_XML_EXPORTS User : public XMLObject
{
public:
    ///Constructor
    User();
    ///Destructor
    virtual ~User();
    ///Copy Constructor
    User( const User& );
    ///equal operator
    User& operator= ( const User& );
    ///The control status of this user
    ///Valid status
    ///MASTER == controlling changes of the main graphics state
    ///SLAVE == observing changes of the main graphics state
    /*\fn typedef VEControlStatus
     *The control status of the user.
     */
    typedef std::string VEControlStatus;

    ///Set the users ID
    ///\param id String to uniquely identify the user
    void SetUserId( std::string id );
    ///Set the control status of the user
    ///\param cs The control status.
    void SetControlStatus( VEControlStatus cs );
    ///Set the state information for this user
    ///\param userState The StateInfo for this user.
    void SetStateInfo( StateInfoWeakPtr userState );

    ///Return the user id
    std::string GetUserId();
    ///Return the VEControlStatus of this user
    VEControlStatus GetControlStatus();
    ///Return the StateInfo for this user.
    StateInfoWeakPtr GetUserStateInfo();

    ///Set the data for this object from an XML element
    ///\param xmlInput The input XML element
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

protected:
    ///Internally update this element
    ///\param tagName The tagName for this element.
    virtual void _updateVEElement( std::string tagName );
    std::string _userId;///<The users unique identification
    StateInfoPtr m_stateInfo;///<The StateInfo for this user.
    VEControlStatus _controlStatus;///<The VEControlStatus of this user.
};
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, User* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// _XML_VE_USER_H_
