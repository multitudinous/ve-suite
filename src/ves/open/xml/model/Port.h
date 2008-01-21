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

#ifndef PORT_H_
#define PORT_H_
/*!\file Port.h
  *Data ports API
  */

/*!\class VE_XML::VE_Model::Port
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
#include <string>
#include <vector>
#include <ves/open/xml/XMLObject.h>

//#include <xercesc/dom/DOM.hpp>

namespace ves
{
namespace open
{
namespace xml
{
class DataValuePair;
namespace model
{
class Point;
}
}
}
}

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
class VE_MODEL_EXPORTS Port : public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Port( );
    ///Destructor
    virtual ~Port();
    ///Copy Constructor
    Port( const Port& );
    ///equal operator
    Port& operator= ( const Port& );

    /// Set the number
    ///\param number The number of the port
    void SetPortNumber( unsigned int number );

    ///Set the string type
    ///\param name The name of the model that this port is tied to.
    void SetModelName( std::string name );

    ///Set the string data
    ///\param direction The data flow direction either input or output.
    void SetDataFlowDirection( std::string direction );

    ///Set the point data
    ///\param location The location of the port in wx terms.
    void SetPortLocation( Point* location );

    ///Set the DataValuePair data
    ///\param data The DataValuePair vector of data.
    void SetPortData( std::vector< ves::open::xml::DataValuePair* > data );

    ///Set the string data
    ///\param porttype The name of the data type that this port is tied to
    void SetPortType( std::string porttype );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the port number.
    unsigned int GetPortNumber( void );
    ///Get the model name.
    std::string GetModelName( void );
    ///Get the data flow direction.
    std::string GetDataFlowDirection( void );
    ///Get the port location.
    Point* GetPortLocation( void );
    ///Get the port data.
    std::string GetPortType( void );
    ///Get the port type.
    std::vector< ves::open::xml::DataValuePair* > GetPortData( void );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( std::string tagName );

    ///raw datatypes of Port that are specified in the verg_model.xsd file
    std::vector< ves::open::xml::DataValuePair* > portData;///<Vector of DataValuePairs.
    unsigned int portNumber;///<Number of the port. Generated by wx.
    std::string modelName;///<Name of the model that the port belongs to.
    std::string dataFlow;///<Direction of the data flow, either input or output.
    Point* portLocation;///<Physical location of the port on the wx design canvas.
    std::string portType;///<DataType of the port, in case the data is strong Typed
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::model::Port* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// PORT_H_
