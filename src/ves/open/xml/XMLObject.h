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

#ifndef _VE_XML_OBJECT_H_
#define _VE_XML_OBJECT_H_

#include <xercesc/util/XMLString.hpp>
#include <xercesc/dom/DOM.hpp>

#include <iostream>
#include <string>
#include <typeinfo>
#include <sstream>
#include <iomanip>

#include <ves/VEConfig.h>
/*!\file XMLObject.h
  Base XML API
  */
/*!\class VE_XML::XMLObject
 * This class is the base class for representing
 * XML objects.
 */
/*!\namespace VE_XML
 * Contains nodes for creating/managing a XML Objects.
 */

namespace ves
{
namespace open
{
namespace xml
{
class XMLObject;
class VEStr;
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
///Utility function to convert strings to Xerces compatible strings
#define xercesString(str) ves::open::xml::XMLObject::VEStr(str).unicodeForm()

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
    void SetObjectType( std::string veObjectType );

    ///Set the XMLObject Namespace
    ///\param veObjectNamespace
    void SetObjectNamespace( std::string veObjectNamespace );

public:
    ///Set the DOMDocument this object belongs to.
    void SetOwnerDocument( XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* owner );

    ///Populate the XMLObject data from an XML element.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput ) = 0;

    ///Return the object type. This should be set in the constructor of all derived classes
    std::string GetObjectType();

    ///Return the object namespace. This should be set in the constructor of all derived classes
    std::string GetObjectNamespace();

    ///Get an XML element from the current data in the string.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* GetXMLData( std::string tagName );

    ///Return the root document of this element.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* GetRootDocument();

    ///utility functions for reading data from an element
    ///\param element Element to extract string from.
    //bool ExtractBooleanFromSimpleElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element );

    ///utility functions for reading data from an element
    ///\param element Element to extract string from.
    //std::string ExtractDataStringFromSimpleElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element );

    ///utility functions for reading data from an element
    ///\param element Element to extract double from.
    //double ExtractDataNumberFromSimpleElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element);

    ///utility functions for reading data from an element
    ///\param element Element to extract unsigned integer from.
    //unsigned int ExtractIntegerDataNumberFromSimpleElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element);

    ///utility functions for reading data from an element
    ///\param element Element to extract long integer from.
    //long int ExtractLongIntegerDataNumberFromSimpleElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element);

    ///Usage is: unsigned int data = ExtractFromSimpleElement< unsigned int >( element );
    ///Not sure how to document this
    template<class T>
    inline T ExtractFromSimpleElement( const XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element )
    {
        T ret_val = T();
        try
        {
            std::istringstream iss;
            // in case the element does not contain data
            XERCES_CPP_NAMESPACE_QUALIFIER DOMText* rawText =
                dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >( element->getFirstChild() );
            if( rawText )
            {
                char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( rawText->getData() );
                iss.str( fUnicodeForm );
                delete fUnicodeForm;
            }
            iss >> ret_val;
        }
        catch ( ... )
        {
            std::cout << "ERROR : ExtractFromSimpleElement " << std::endl;
        }
        return ret_val;
    }

    ///Get a string attribute by name
    ///\param baseElement The element to extract it from
    ///\param attributeName The name of the attribute
    ///\param attribute The attribute to retrive.
    //void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string attributeName, std::string& attribute);

    ///Get a bool attribute by name
    ///\param baseElement The element to extract it from
    ///\param attributeName The name of the attribute
    ///\param attribute The attribute to retrive.
    //void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string attributeName, bool& attribute);

    ///Get an unsigned int attribute by name
    ///\param baseElement The element to extract it from
    ///\param attributeName The name of the attribute
    ///\param attribute The attribute to retrive.
    //void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string attributeName, unsigned int& attribute);

    ///Get an attribute by name
    ///\param baseElement The element to extract it from
    ///\param attributeName The name of the attribute
    ///\param attribute The attribute to retrive.
    //void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string attributeName, float& attribute);
    //void XMLObject::GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
    //                              std::string attributeName, float& attribute)
    template<class T>
    inline void GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                              const std::string attributeName, T& attribute )
    {
        try
        {
            char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( baseElement->getAttribute( xercesString( attributeName.c_str() ) ) );
            if( !fUnicodeForm )
            {
                return;
            }

            std::stringstream float2string( fUnicodeForm );
            float2string >> attribute;
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
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* GetSubElement( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement, std::string subElementTagName, unsigned int itemIndex );

    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( std::string subElementTagName, bool dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( std::string subElementTagName, std::string dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( std::string subElementTagName, unsigned int dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( std::string subElementTagName, long int dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( std::string subElementTagName, double dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    //void SetSubElement( std::string subElementTagName, XMLObject* dataValue );
    ///utility functions for creating subElements for _veElement.
    ///\param subElementTagName The subelement tagname to extract from baseElement.
    ///\param dataValue The data to be stored.
    ///\param attribName The name of the atrribute to be set
    ///\param attrib The attribute value
    ///playing with templates
    template<class T>
    inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* SetSubElement( const std::string subElementTagName, T val )
    {
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
            _rootDocument->createElement( xercesString( subElementTagName ) );

        dataValueNumElement->setAttribute( xercesString( "type" ),
                                           xercesString( xmlTag.c_str() ) );
        std::stringstream float2string;
        float2string << val;

        XERCES_CPP_NAMESPACE_QUALIFIER DOMText* dataValueText =
            _rootDocument->createTextNode(
                xercesString( float2string.str().c_str() ) );

        dataValueNumElement->appendChild( dataValueText );
        _veElement->appendChild( dataValueNumElement );
        return dataValueNumElement;
    }

    ///utility functions for creating attribute on _veElement by default
    ///\param attirbuteName The name of the atrribute to be set
    ///\param attribute The attribute value
    ///\param element The element to add an attribute to
    //void SetAttribute( std::string attirbuteName, std::string attribute,
    //                   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element = 0);

    ///utility functions for creating attribute on _veElement.
    ///\param attirbuteName The name of the atrribute to be set
    ///\param attribute The attribute value
    ///\param element The element to add an attribute to
    //void SetAttribute( std::string attirbuteName,unsigned int attribute,
    //                   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element = 0);

    ///utility functions for creating attribute on _veElement.
    ///\param attirbuteName The name of the atrribute to be set
    ///\param attribute The attribute value
    ///\param element The element to add an attribute to
    //void SetAttribute( std::string attirbuteName,bool attribute,
    //                   XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element = 0);

    ///utility functions for creating attribute on _veElement by default
    ///\param attirbuteName The name of the atrribute to be set
    ///\param attribute The attribute value
    ///\param element The element to add an attribute to
    template<class T>
    inline void SetAttribute( const std::string attirbuteName, T attribute,
                              XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element = 0 )
    {
        if( element == 0 )
        {
            element = _veElement;
        }
        std::stringstream int2string;
        int2string << attribute;
        element->setAttribute( xercesString( attirbuteName ), xercesString( int2string.str().c_str() ) );
    }

    ///Method to set id object
    ///\param idVar new id
    void SetID( unsigned int idVar );
    ///Method to set id object
    ///\param idVar new id
    void SetID( std::string idVar );
    ///Method to get id for object
    std::string GetID( void );

    class VE_XML_EXPORTS VEStr
    {
    public:
        ///Constructor
        ///\param toTranscode The input to translate.
        VEStr( const char* const toTranscode );
        ///Constructor
        ///\param input The input to translate.
        VEStr( int input );
        ///Constructor
        ///\param input The input to translate.
        VEStr( unsigned int input );
        ///Constructor
        ///\param input The input to translate.
        VEStr( long int input );
        ///Constructor
        ///\param input The input to translate.
        VEStr( double input );
        ///Constructor
        ///\param input The input to translate.
        VEStr( std::string input );

        ///Destructor
        virtual ~VEStr();

        ///Get the char for the string.
        const XMLCh* unicodeForm( void ) const;

    private:
        // -----------------------------------------------------------------------
        //  Private data members
        //
        //  fUnicodeForm
        //      This is the Unicode XMLCh format of the string.
        // -----------------------------------------------------------------------
        XMLCh*   fUnicodeForm;///< The raw unicode string.
    };


protected:
    ///Internally update the XML data.
    virtual void _updateVEElement( std::string ) = 0;

    ///Clear all the children from the element.
    void _clearAllChildrenFromElement();
    bool _needsUpdate;///<Determines whether the internal data has changed.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* _veElement;///<The XML element.
    XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* _rootDocument;///<The owning document for this element.
    std::string _objectType;///<The type of object;
    std::string _objectNamespace;///<The namespace for this object;
    std::string uuid;///<Data holder for id
private:
    unsigned int _nChildren;///<The number of childern for this element.
};
///This is the special case for any xmlobject
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, XMLObject* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
///Special case for bools
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, bool val )
{
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* dataValueStringElement = _rootDocument->createElement( xercesString( subElementTagName ) );
    dataValueStringElement->setAttribute( xercesString( "type" ), xercesString( "xs:boolean" ) );
    std::string boolValue( "true" );
    if( !val )
    {
        boolValue = "false";
    }
    XERCES_CPP_NAMESPACE_QUALIFIER DOMText* dataValueString = _rootDocument->createTextNode( xercesString( boolValue ) );
    dataValueStringElement->appendChild( dataValueString );
    _veElement->appendChild( dataValueStringElement );
    return dataValueStringElement;
}
///Another special case for bools
template<>
inline void XMLObject::SetAttribute( const std::string attirbuteName, bool attribute,
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
    element->setAttribute( xercesString( attirbuteName ), xercesString( bool2String.c_str() ) );
}
///Yet another special case for bools
template<>
inline void XMLObject::GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                                     const std::string attributeName, bool& attribute )
{
    try
    {
        char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( baseElement->getAttribute( xercesString( attributeName.c_str() ) ) );
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
    }
    catch ( ... )
    {
        std::cout << "Invalid element!!" << std::endl;
        std::cout << "XMLObject::GetAttribute()" << std::endl;
    }
}
///Special method for a string
template<>
inline void XMLObject::GetAttribute( XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* baseElement,
                                     const std::string attributeName, std::string& attribute )
{
    try
    {
        char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( baseElement->getAttribute( xercesString( attributeName.c_str() ) ) );
        if( !fUnicodeForm )
        {
            return;
        }

        std::stringstream float2string( fUnicodeForm );
        float2string.str( fUnicodeForm );
        attribute = float2string.str();
        delete fUnicodeForm;
    }
    catch ( ... )
    {
        std::cout << "Invalid element!!" << std::endl;
        std::cout << "XMLObject::GetAttribute()" << std::endl;
    }
}
///this is for the special case where bools are stored as strings in
///the elements because 0 or 1 is not stored
template<>
inline bool XMLObject::ExtractFromSimpleElement< bool >( const XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element )
{
    XERCES_CPP_NAMESPACE_QUALIFIER DOMText* rawText = dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >( element->getFirstChild() );
    std::string tmp;
    char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( rawText->getData() );
    tmp.assign( fUnicodeForm );
    delete fUnicodeForm;

    if( tmp == "true" )
        return true;
    else
        return false;
}
///This is to account for spaces in element values because just reading a string
/// does not get the whole line from the istringstream
template<>
inline std::string XMLObject::ExtractFromSimpleElement< std::string >( const XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* element )
{
    std::string ret_val = std::string();
    try
    {
        std::istringstream iss;
        // in case the element does not contain data
        XERCES_CPP_NAMESPACE_QUALIFIER DOMText* rawText =
            dynamic_cast< XERCES_CPP_NAMESPACE_QUALIFIER DOMText* >( element->getFirstChild() );
        if( rawText )
        {
            char* fUnicodeForm = XERCES_CPP_NAMESPACE_QUALIFIER XMLString::transcode( rawText->getData() );
            iss.str( fUnicodeForm );
            delete fUnicodeForm;
        }
        else
        {
            std::cout << "Failed to ExtractFromSimpleElement std::string" << std::endl;
        ret_val = iss.str();
    }
    catch ( ... )
    {
        std::cout << "ERROR : ExtractFromSimpleElement " << std::endl;
    }
    return ret_val;
}
}
}
}
#endif// _VE_XML_OBJECT_H_
