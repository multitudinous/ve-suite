/*************** <auto-copyright.pl BEGIN do not edit this line> **************
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
 *************** <auto-copyright.pl END do not edit this line> ***************/

#ifndef NETWORK_H_
#define NETWORK_H_
/*!\file Network.h
  *System Network API
  */

/*!\class VE_XML::VE_Model::Network
 *Class that manages the system network for conductor.
 */
#include <string>
#include <vector>

#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/model/LinkPtr.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/model/Model.h>

#include <xercesc/dom/DOM.hpp>

namespace ves
{
namespace open
{
namespace xml
{
class DataValuePair;
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
class VE_MODEL_EXPORTS Network : public ves::open::xml::XMLObject
{
public:
    ///Constructor
    Network( );
    ///Destructor
    virtual ~Network();
    ///Copy Constructor
    Network( const Network& );
    ///equal operator
    Network& operator= ( const Network& );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the i'th link from the Network.
    ///\param i The i'th link you are after.
    LinkWeakPtr GetLink( int i );
    ///Get the total number of links
    size_t GetNumberOfLinks( void );
    ///Get the i'th link from the Network.
    ///\param i The i'th link you are after.
    ves::open::xml::DataValuePair* GetDataValuePair( int i );
    ///Get the i'th tag from the Network.
    ///\param i The i'th tag you are after.
    TagPtr GetTag( size_t i );
    ///Get the total number of links
    size_t GetNumberOfTags( void );
    ///Add a tag to the network
    ///\param newTag The new tag to be added
    void AddTag( TagPtr newTag );
    ///Add a Link to the network
    ///\param newLink The new Link to be added
    void AddLink( LinkWeakPtr newLink );
    ///Remove the tag from the network
    ///\param oldTag The tag to be removed
    void RemoveTag( TagPtr oldTag );
    ///Remove a link from the network
    ///\param oldLink The link to be removed
    void RemoveLink( LinkWeakPtr oldLink );
    void SetParentModel( ModelSharedPtr parent );
    ModelSharedPtr GetParentModel( );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( std::string tagName );

private:
    ///raw datatypes of Network that are specified in the verg_model.xsd file
    std::vector< LinkSharedPtr > links;///<Vector of Links.
    std::vector< ves::open::xml::DataValuePair* > conductorState;///<Vector of data value pairs that hold conductor info.
    ///The vector of tags for this network
    std::vector< TagPtr > tags;
    ModelSharedPtr parentModel;
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement( const std::string subElementTagName, ves::open::xml::model::Network* val )
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}
}
}
#endif// NETWORK_H_
