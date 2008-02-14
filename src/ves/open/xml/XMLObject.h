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
#ifndef _VES_OPEN_XML_OBJECT_H_
#define _VES_OPEN_XML_OBJECT_H_

#include <ves/open/xml/XMLObjectPtr.h>

#include <ves/VEConfig.h>

#include <ves/open/xml/util/Convert.h>

#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <iostream>
#include <string>
#include <typeinfo>
#include <sstream>
#include <iomanip>


namespace ves
{
namespace open
{
namespace xml
{

/*!\file XMLObject.h
  Base XML API
  */
/*!\class ves::open::xml::XMLObject
 * This class is the base class for representing
 * XML objects.
 */
/*!\namespace ves::open::xml
 * Contains nodes for creating/managing a XML Objects.
 */
class VE_XML_EXPORTS XMLObject
{
public:
    ///Base constructor
    XMLObject( );
    ///Destructor
    virtual ~XMLObject();
    ///Copy Construstor
    XMLObject( const XMLObject& );
    ///equal operator
    ///These urls may be useful for future operations:
    ///http://xml.apache.org/xerces-c/apiDocs/classDOMNode.html#z233_1
    ///http://xml.apache.org/xerces-c/apiDocs/classDOMNode.html#z233_0
    XMLObject& operator= ( const XMLObject& );

protected:
    ///Set the XMLObject type
    ///\param veObjectType
    void SetObjectType( const std::string& veObjectType );

    ///Set the XMLObject Namespace
    ///\param veObjectNamespace
    void SetObjectNamespace( const std::string& veObjectNamespace );

public:
    ///Set the DOMDocument this object belongs to.
    void SetOwnerDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner );

    ///Populate the XMLObject data from an XML element.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput ) = 0;

    ///Return the object type. This should be set in the constructor of all derived classes
    const std::string& GetObjectType();

    ///Return the object namespace. This should be set in the constructor of all derived classes
    const std::string& GetObjectNamespace();

    ///Get an XML element from the current data in the string.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* GetXMLData( const std::string& tagName );

    ///Return the root document of this element.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* GetRootDocument();

    ///Get an attribute by name
    ///\param baseElement The element to extract it from
    ///\param attributeName The name of the attribute
    ///\param attribute The attribute to retrive.
    template<class T>
    inline void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                              const std::string& attributeName, T& attribute )
    {
        std::cout << "GetAttribute(" << attributeName << ")" << std::endl;
        try
        {
            char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( baseElement->getAttribute( Convert(attributeName).toXMLString() ) );
            if( !fUnicodeForm )
            {
                std::cout << "    attr: FAILED!!!" << std::endl;
                return;
            }

            std::stringstream float2string( fUnicodeForm );
            float2string >> attribute;
            std::cout << "    attr: " << float2string.str().c_str() << std::endl;
            delete fUnicodeForm;
        }
        catch ( ... )
        {
            std::cout << "Invalid element!!" << std::endl;
            std::cout << "XMLObject::GetAttribute()" << std::endl;
        }
    }

    ///utility functions for extracting subElement itemIndex from a complex element.
    ///\param baseElement The XML complexElement to extract a subelement from of type subElementTagName.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param itemIndex The index of the subElement to extract from the complex element.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* GetSubElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, const std::string& subElementTagName, unsigned int itemIndex );

    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    ///\param attribName The name of the atrribute to be set
    ///\param attrib The attribute value
    ///playing with templates
    template<class T>
    inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( const std::string& subElementTagName, T val )
    {
        std::cout << "SetSubElement(T val) for " << subElementTagName.c_str() <<std::endl;
        std::string xmlTag( "xs:undefined" );
        if( typeid( double ) == typeid( val ) )
        {
            xmlTag = "xs:double";
        }
        else if( typeid( std::string ) == typeid( val ) )
        {
            xmlTag = "xs:string";
        }
        else if( typeid( unsigned int ) == typeid( val ) )
        {
            xmlTag = "xs:unsignedInt";
        }
        else if( typeid( long int ) == typeid( val ) )
        {
            xmlTag = "xs:integer";
        }
        XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* dataValueNumElement =
            _rootDocument->createElement( Convert( subElementTagName ).toXMLString() );

        dataValueNumElement->setAttribute( Convert( "type" ).toXMLString(),
                                           Convert( xmlTag ).toXMLString() );

        XERCES_CPP_NAMESPACE_QUALIFIER DOMText* dataValueText =
            _rootDocument->createTextNode( Convert( val ).toXMLString() );

        dataValueNumElement->appendChild( dataValueText );
        _veElement->appendChild( dataValueNumElement );
        return dataValueNumElement;
    }

    ///This is the special case for any xmlobject
    template<class T>
    inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( const std::string& subElementTagName, T* val )
    {
        std::cout << "SetSubElement(T* ) for " << subElementTagName.c_str() <<std::endl;
        val->SetOwnerDocument( _rootDocument );
        XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
        _veElement->appendChild( childElement );
        return childElement;
    }

    ///utility functions for creating attribute on _veElement by default
    ///\param attirbuteName The name of the atrribute to be set
    ///\param attribute The attribute value
    ///\param element The element to add an attribute to
    template<class T>
    inline void SetAttribute( const std::string& attributeName, T attribute,
                              XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element = 0 )
    {
        if( element == 0 )
        {
            element = _veElement;
        }
        element->setAttribute( Convert( attributeName ).toXMLString(),
                               Convert( attribute ).toXMLString() );
    }

    ///Method to set id object
    ///\param idVar new id
    void SetID( unsigned int idVar );
    ///Method to set id object
    ///\param idVar new id
    void SetID( const std::string& idVar );
    ///Method to get id for object
    const std::string& GetID( void );



protected:
    ///Internally update the XML data.
    virtual void _updateVEElement( const std::string& ) = 0;

    ///Clear all the children from the element.
    void _clearAllChildrenFromElement();
    bool mNeedsUpdate;///<Determines whether the internal data has changed.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* mVeElement;///<The XML element.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* mRootDocument;///<The owning document for this element.
    std::string mObjectType;///<The type of object;
    std::string mObjectNamespace;///<The namespace for this object;
    std::string mUuid;///<Data holder for id
private:
    //unsigned int mNChildren;///<The number of childern for this element.
};
///Special case for bools
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string& subElementTagName, bool val )
{
    std::cout << "SetSubElement(bool) for " << subElementTagName.c_str() <<std::endl;
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* dataValueStringElement = _rootDocument->createElement( Convert( subElementTagName ).toXMLString() );
    dataValueStringElement->setAttribute( Convert( "type" ).toXMLString(),
                                          Convert( "xs:boolean" ).toXMLString() );
    std::string boolValue( "true" );
    if( !val )
    {
        boolValue = "false";
    }
    XERCES_CPP_NAMESPACE_QUALIFIER DOMText* dataValueString =
                        _rootDocument->createTextNode( Convert( boolValue ).toXMLString() );
    dataValueStringElement->appendChild( dataValueString );
    _veElement->appendChild( dataValueStringElement );
    return dataValueStringElement;
}
///Another special case for bools
template<>
inline void XMLObject::SetAttribute( const std::string& attributeName, bool attribute,
                                     XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element )
{
    if( element == 0 )
    {
        element = _veElement;
    }

    std::string bool2String( "false" );

    if( attribute )
    {
        bool2String = "true";
    }
    element->setAttribute( Convert( attributeName ).toXMLString(),
                           Convert( bool2String ).toXMLString() );
}
///Yet another special case for bools
template<>
inline void XMLObject::GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                                     const std::string& attributeName, bool& attribute )
{
    std::cout << "GetAttribute(" << attributeName << ") bool" << std::endl;
    try
    {
        char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode(
                  baseElement->getAttribute( Convert( attributeName ).toXMLString() ) );
        if( !fUnicodeForm )
        {
            return;
        }

        std::string value( fUnicodeForm );
        delete fUnicodeForm;
        if( value == "true" )
        {
            attribute = true;
        }
        else
        {
            attribute = false;
        }
        std::cout << "   attr: " << value.c_str() << std::endl;
    }
    catch( ... )
    {
        std::cout << "Invalid element!!" << std::endl;
        std::cout << "XMLObject::GetAttribute()" << std::endl;
    }
}
///Special method for a string
template<>
inline void XMLObject::GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                                     const std::string& attributeName, std::string& attribute )
{
    std::cout << "GetAttribute(" << attributeName << ") string" << std::endl;
    try
    {
        char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode(
               baseElement->getAttribute( Convert( attributeName ).toXMLString() ) );
        if( !fUnicodeForm )
        {
            return;
        }

        std::stringstream float2string( fUnicodeForm );
        float2string.str( fUnicodeForm );
        attribute = float2string.str();
        std::cout << "   attr: " << attribute.c_str() << std::endl;
        delete fUnicodeForm;
    }
    catch( ... )
    {
        std::cout << "Invalid element!!" << std::endl;
        std::cout << "XMLObject::GetAttribute()" << std::endl;
    }
}

}
}
}
#endif// _VE_XML_OBJECT_H_
