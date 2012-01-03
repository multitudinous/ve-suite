/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
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

#ifndef _VES_OPEN_XML_MODEL_NETWORK_H_
#define _VES_OPEN_XML_MODEL_NETWORK_H_

#include <ves/open/xml/model/NetworkPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/model/TagPtr.h>
#include <ves/open/xml/model/LinkPtr.h>
#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/DataValuePairPtr.h>

#include <xercesc/dom/DOM.hpp>

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
/*!\file Network.h
  *System Network API
  */

/*!\class VE_XML::VE_Model::Network
 *Class that manages the system network for conductor.
 */
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

    ///Add a data value pair for the network.
    ///\param dataValuePair
    void AddDataValuePair( ves::open::xml::DataValuePairPtr dataValuePair );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///Get the i'th link from the Network.
    ///\param i The i'th link you are after.
    LinkPtr GetLink( int i );

    ///Get the total number of links
    size_t GetNumberOfLinks( void );

    ///Get the i'th link from the Network.
    ///\param i The i'th link you are after.
    ves::open::xml::DataValuePairPtr GetDataValuePair( size_t index );

    ///Get the number of states for the network
    size_t GetNumberOfNetworkStates();

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
    void AddLink( LinkPtr newLink );

    ///Remove the tag from the network
    ///\param oldTag The tag to be removed
    void RemoveTag( TagPtr oldTag );

    ///Remove a link from the network
    ///\param oldLink The link to be removed
    void RemoveLink( LinkPtr oldLink );

    void SetParentModel( ModelPtr parent );

    ModelPtr GetParentModel( );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

private:
    ///raw datatypes of Network that are specified in the verg_model.xsd file
    std::vector< LinkPtr > mLinks;///<Vector of Links.
    std::vector< ves::open::xml::DataValuePairPtr > mConductorState;///<Vector of data value pairs that hold conductor info.
    ///The vector of tags for this network
    std::vector< TagPtr > mTags;
    ///This variable is needed to enable the hiearchy data to be accessed 
    ///from any level in a sub system configuration
    ModelWeakPtr mParentModel;
};

}
}
}
}
#endif// NETWORK_H_
