/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#ifndef VE_OPEN_XML_MODEL_SYSTEM_H
#define VE_OPEN_XML_MODEL_SYSTEM_H

#include <ves/open/xml/model/SystemPtr.h>

#include <ves/open/xml/XMLObject.h>

#include <ves/open/xml/model/ModelPtr.h>
#include <ves/open/xml/model/NetworkPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <boost/enable_shared_from_this.hpp>

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
/*!\file System.h
  *System System API
  */

/*!\class ves::open::xml::model::System
 *Class that manages the system network for conductor.
 */
class VE_MODEL_EXPORTS System : public ves::open::xml::XMLObject,
                               public boost::enable_shared_from_this<System>
{
public:
    ///Constructor
    System();
    ///Destructor
    virtual ~System();
    ///Copy Constructor
    System( const System& );
    ///equal operator
    System& operator= ( const System& );

    ///Get the i'th model from the system
    ///\param i The i'th model you are after
    ///\return The model requested by the user
    ModelPtr GetModel( size_t i );

    ///Get all the models for this system
    ///\return The vector of all the models
    std::vector< ModelPtr > GetModels();

    ///Get the total number of models
    ///\return The number of models
    size_t GetNumberOfModels( void );

    ///Add a model to the system
    ///\param inputModel The model to be added to the system
    void AddModel( ModelPtr inputModel );

    ///Get the network for the system
    ///\return The respective network for the system
    NetworkPtr GetNetwork();

    ///Add a network to the system
    ///\param inputNetwork The new network to be added
    void AddNetwork( NetworkPtr inputNetwork );

    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData(
        XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );
    ///Set the model that is above this model
    void SetParentModel( ModelPtr parent );
    ///Get the model that is above this one
    ModelPtr GetParentModel();
    ///Remove the model from the system
    bool RemoveModel( ModelPtr parent );

protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( const std::string& tagName );

private:
    ///The systems network
    NetworkPtr mNetwork;
    ///The vector of models for this system
    std::vector< ModelPtr > mModels;
    ///The model that is above this system
    ModelSharedPtr mParentModel;
};

}
}
}
}
#endif// SYSTEM_H
