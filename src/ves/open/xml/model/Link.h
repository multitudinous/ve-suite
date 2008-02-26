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

#ifndef VES_OPEN_XML_MODEL_LINK_H_
#define VES_OPEN_XML_MODEL_LINK_H_

#include <ves/open/xml/model/LinkPtr.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/model/PointPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <xercesc/dom/DOM.hpp>

#include <string>
#include <vector>
#include <utility>

namespace ves
{
namespace open
{
namespace xml
{
namespace model
{
/*!\file Link.h
  *Data ports API
  */

/*!\class ves::open::xml::model::Link
 *Class that manages the port data for a specific model.
 *These class holds the raw data and the necessary info to draw the port
 *as well as the port direction (input or output) data
 */
class VE_MODEL_EXPORTS Link : public ves::open::xml::XMLObject
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
        XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the portInfo for the fromPort.
    ///\return The element with the information about the module being linked to
    ves::open::xml::DataValuePairPtr GetFromModule( void );

    ///Get the portInfo for the toPort.
    ///\return The element with the information about the module being linked to
    ves::open::xml::DataValuePairPtr GetToModule( void );

    ///Get the portInfo for the fromPort.
    ///\return The from port for the link
    long int* GetFromPort( void );

    ///Get the portInfo for the toPort.
    ///\return The to port for the link
    long int* GetToPort( void );

    ///Get the i'th point for a link.
    ///\param i The i'th point you are after.
    PointPtr GetLinkPoint( unsigned int i );

    ///Get the number of points used to define a link.
    ///\return The number of points making up the link
    size_t GetNumberOfLinkPoints( void );

    ///Set the link name
    ///\param name The name for the link
    void SetLinkName( const std::string& name );

    ///Get the link name
    ///\return The link name in a string
    const std::string& GetLinkName( void );

    ///Set the link type
    ///\param type The type of linke
    void SetLinkType( int );

    ///Get the link type
    ///\return The link type in a string
    int GetLinkType();

    void SetParentModel( ModelPtr parent );

    ModelPtr GetParentModel();

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

private:
    ///raw datatypes of Link that are specified in the verg_model.xsd file
    std::vector< PointPtr > mLinkPoints;///<Vector of Points.

    ///The data value pair will contain the model and port
    ///number of the appropriate port to be linked
    ///The classes hold the fromPort in first and the toPort in second.
    std::pair< ves::open::xml::DataValuePairPtr,
               ves::open::xml::DataValuePairPtr > mModuleInfo;

    ///The classes hold the fromPort in first and the toPort in second.
    std::pair< long int, long int > mPortInfo;

    ///The name of the link.
    std::string mLinkName;

    ///The link type
    int mType;

    ModelWeakPtr mParentModel;
};

}
}
}
}
#endif// LINK_H_
