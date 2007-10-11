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

#ifndef _VE_LINK_H_
#define _VE_LINK_H_
/*!\file Link.h
  *Data ports API
  */

/*!\class VE_XML::VE_Model::Link
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
#include <string>
#include <vector>
#include <utility>
#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
   class DataValuePair;
   namespace VE_Model
   {
      class Point;
   }
}

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS Link : public VE_XML::XMLObject
{
public:
    ///Constructor
    Link( );
    ///Destructor
    virtual ~Link();
    ///Copy Constructor
    Link( const Link& );
    ///equal operator
    Link& operator= ( const Link& );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( 
        XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);

    ///Get the portInfo for the fromPort.
    ///\return The element with the information about the module being linked to
    VE_XML::DataValuePair* GetFromModule( void );
    ///Get the portInfo for the toPort.
    ///\return The element with the information about the module being linked to
    VE_XML::DataValuePair* GetToModule( void );
    ///Get the portInfo for the fromPort.
    ///\return The from port for the link
    long int* GetFromPort( void );
    ///Get the portInfo for the toPort.
    ///\return The to port for the link
    long int* GetToPort( void );
    ///Get the i'th point for a link.
    ///\param i The i'th point you are after.
    Point* GetLinkPoint( unsigned int i );
    ///Get the number of points used to define a link.
    ///\return The number of points making up the link
    size_t GetNumberOfLinkPoints( void );
    ///Set the link name
    ///\param name The name for the link
    void SetLinkName( std::string name );
    ///Get the link name
    ///\return The link name in a string
    std::string GetLinkName( void );
    ///Set the link type
    ///\param type The type of linke
    void SetLinkType( std::string type );
    ///Get the link type
    ///\return The link type in a string
    std::string GetLinkType();

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( std::string tagName );

private:
       ///raw datatypes of Link that are specified in the verg_model.xsd file
    std::vector< Point* > linkPoints;///<Vector of Points.
    ///The data value pair will contain the model and port 
    ///number of the appropriate port to be linked
    ///The classes hold the fromPort in first and the toPort in second.
    std::pair< VE_XML::DataValuePair*, VE_XML::DataValuePair* > moduleInfo;
    ///The classes hold the fromPort in first and the toPort in second.
    std::pair< long int, long int > portInfo;
    ///The name of the link.
    std::string linkName;
    ///The link type
    std::string m_type;
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(
    const std::string subElementTagName, VE_Model::Link* val)
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = 
        val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}

#endif// _VE_LINK_H_
