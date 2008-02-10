/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2008 by Iowa State University
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

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/XMLCreator.h>
#include <ves/open/xml/XMLObjectFactory.h>

#include <sstream>
#include <iomanip>

#include <apr_uuid.h>

XERCES_CPP_NAMESPACE_USE

using namespace ves::open::xml;

//////////////////////
XMLObject::XMLObject()
{
    _veElement = 0;
    _needsUpdate = false;
    _rootDocument = 0;
    _nChildren = 0;
    _objectType = std::string( "XMLObject" );
    _objectNamespace = std::string( "XML" );

    apr_uuid_t tempUUID;
    apr_uuid_get( &tempUUID );
    char* buffer = new char[ APR_UUID_FORMATTED_LENGTH + 1 ];
    apr_uuid_format( buffer, &tempUUID );
    uuid.assign( buffer );
    delete [] buffer;
    //This may need to be somewhere else
    /*if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("XML"))
    {
       XMLObjectFactory::Instance()->RegisterObjectCreator("XML",new XMLCreator());
    }*/
}
///////////////////////////////////////////////////
XMLObject::XMLObject( const XMLObject& input )
{
    _veElement = input._veElement;
    _needsUpdate = input._needsUpdate;
    _rootDocument = input._rootDocument;
    _nChildren = input._nChildren;
    _objectType = input._objectType;
    _objectNamespace = input._objectNamespace;
    uuid = input.uuid;
}
//////////////////////////////////////////////////////////////
XMLObject& XMLObject::operator=( const XMLObject& input )
{
    if( this != &input )
    {
        _veElement = input._veElement;
        _needsUpdate = input._needsUpdate;
        _rootDocument = input._rootDocument;
        _nChildren = input._nChildren;
        _objectType = input._objectType;
        _objectNamespace = input._objectNamespace;
        uuid = input.uuid;
    }
    return *this;
}
////////////////////////////
XMLObject::~XMLObject()
{}
//////////////////////////////////////////////////
void XMLObject::SetObjectNamespace( const std::string& tagname )
{
    _objectNamespace = tagname;
}
//////////////////////////////////////////////////
void XMLObject::SetObjectType( const std::string& tagName )
{
    _objectType = tagName;
}
/////////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetOwnerDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner )
{
    _rootDocument = owner;
}
///////////////////////////////////////////
const std::string& XMLObject::GetObjectNamespace()
{
    return _objectNamespace;
}
/////////////////////////////////////////
const std::string& XMLObject::GetObjectType()
{
    return _objectType;
}
////////////////////////////////////////////////////////
DOMElement* XMLObject::GetXMLData( const std::string& input )
{
    if( _rootDocument )
    {
        //Make sure old data is cleared from the xerces side of the element
        //_clearAllChildrenFromElement();
        _veElement = _rootDocument->createElement( Convert( input ).toXMLString() );


        //update the xerces element w/ the current data in the object
        //This function should be overridden in ALL derived classes!!!!!
        _updateVEElement( input );

        //SetSubElement("objectType",_objectType);
        //SetSubElement("objectNamespace",_objectNamespace);
        return _veElement;
    }
    else
    {
        std::cout << "Root Document not set!!" << std::endl;
        return 0;
    }
}
////////////////////////////////////////////////
void XMLObject::_clearAllChildrenFromElement()
{
    if( _veElement )
    {
        _nChildren = _veElement->getChildNodes()->getLength();
        for( int i = _nChildren - 1; i > -1; i-- )
        {
            _veElement->removeChild( _veElement->getChildNodes()->item( i ) );
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
DOMElement* XMLObject::GetSubElement( DOMElement* baseElement,
                                      const std::string& subElementTagName,
                                      unsigned int itemIndex )
{
    DOMElement* foundElement = dynamic_cast<DOMElement*>( baseElement->getElementsByTagName( Convert( subElementTagName ).toXMLString() )->item( itemIndex ) );
    if( foundElement )
    {
        if( foundElement->getParentNode() != baseElement )
        {
            XMLSize_t nChildren = baseElement->getElementsByTagName( Convert( subElementTagName ).toXMLString() )->getLength();
            for( XMLSize_t i = 0; i < nChildren; i++ )
            {
                foundElement = dynamic_cast<DOMElement*>( baseElement->getElementsByTagName( Convert( subElementTagName ).toXMLString() )->item( i ) );
                if( foundElement->getParentNode() == baseElement )
                    return foundElement;
            }
        }
        else if( foundElement->getParentNode() == baseElement )
        {
            return foundElement;
        }
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
DOMDocument* XMLObject::GetRootDocument()
{
    return _rootDocument;
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetID( unsigned int idVar )
{
    std::ostringstream dirStringStream;
    dirStringStream << idVar;
    uuid = dirStringStream.str();
}
////////////////////////////////////////////////////////////////////////////////
void XMLObject::SetID( const std::string& idVar )
{
    uuid = idVar;
}
////////////////////////////////////////////////////////////////////////////////
const std::string& XMLObject::GetID( void )
{
    return uuid;
}
