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
#ifndef _VES_OPEN_XML_MODEL_PORT_H_
#define _VES_OPEN_XML_MODEL_PORT_H_

#include <ves/open/xml/PortPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/DataValuePairPtr.h>
#include <ves/open/xml/model/PointPtr.h>

#include <string>
#include <vector>


namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
/*!\file Port.h
  *Data ports API
  */

/*!\class ves::open::xml::model::Port
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
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
    void SetModelName( const std::string& name );

    ///Set the string data
    ///\param direction The data flow direction either input or output.
    void SetDataFlowDirection( const std::string& direction );

    ///Set the point data
    ///\param location The location of the port in wx terms.
    void SetPortLocation( PointPtr location );

    ///Set the DataValuePair data
    ///\param data The DataValuePair vector of data.
    void SetPortData( const std::vector< ves::open::xml::DataValuePairPtr >& data );

    ///Set the string data
    ///\param porttype The name of the data type that this port is tied to
    void SetPortType( const std::string& porttype );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the port number.
    unsigned int GetPortNumber( void );

    ///Get the model name.
    const std::string& GetModelName( void );

    ///Get the data flow direction.
    const std::string& GetDataFlowDirection( void );

    ///Get the port location.
    PointPtr GetPortLocation( void );

    ///Get the port data.
    const std::string& GetPortType( void );

    ///Get the port type.
    const std::vector< ves::open::xml::DataValuePairPtr >& GetPortData( void );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

    ///raw datatypes of Port that are specified in the verg_model.xsd file
    std::vector< ves::open::xml::DataValuePairPtr > mPortData;///<Vector of DataValuePairs.
    unsigned int mPortNumber;///<Number of the port. Generated by wx.
    std::string mModelName;///<Name of the model that the port belongs to.
    std::string mDataFlow;///<Direction of the data flow, either input or output.
    PointPtr mPortLocation;///<Physical location of the port on the wx design canvas.
    std::string mPortType;///<DataType of the port, in case the data is strong Typed
};

}
}
}
}
#endif// PORT_H_
