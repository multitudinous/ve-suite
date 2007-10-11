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
 * Date modified: $Date: 2007-08-24 11:53:30 -0500 (Fri, 24 Aug 2007) $
 * Version:       $Rev: 8827 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef VE_SYSTEM_H
#define VE_SYSTEM_H
/*!\file System.h
  *System System API
  */

/*!\class VE_XML::VE_Model::System
 *Class that manages the system network for conductor.
 */
#include <string>
#include <vector>

#include "ves/open/xml/XMLObject.h"

#include "ves/open/xml/model/ModelStrongPtr.h"
#include "ves/open/xml/model/ModelWeakPtr.h"
#include "ves/open/xml/model/Model.h"
#include "ves/open/xml/model/NetworkStrongPtr.h"
#include "ves/open/xml/model/NetworkWeakPtr.h"
#include "ves/open/xml/model/Network.h"

#include <xercesc/dom/DOM.hpp>

namespace VE_XML
{
namespace VE_Model
{
class VE_MODEL_EXPORTS System : public VE_XML::XMLObject
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
    ModelWeakPtr GetModel( size_t i );
    ///Get all the models for this system
    ///\return The vector of all the models
    std::vector< ModelWeakPtr > GetModels();
    ///Get the total number of models
    ///\return The number of models
    size_t GetNumberOfModels( void );
    ///Add a model to the system
    ///\param inputModel The model to be added to the system
    void AddModel( ModelWeakPtr inputModel );
    ///Get the network for the system
    ///\return The respective network for the system
    NetworkWeakPtr GetNetwork();
    ///Add a network to the system
    ///\param inputNetwork The new network to be added
    void AddNetwork( NetworkWeakPtr inputNetwork );
    ///set the data from an string representing the xml
    ///\param xmlInput The input XML data.
    virtual void SetObjectFromXMLData( 
        XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput);
    
protected:
    ///Internally update the data.
    ///\param tagName The tag name of this element.
    virtual void _updateVEElement( std::string tagName );

private:
    ///The systems network
    NetworkStrongPtr m_network;
    ///The vector of models for this system
    std::vector< ModelStrongPtr > m_models;
};
}
template<>
inline XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* XMLObject::SetSubElement(
    const std::string subElementTagName, VE_Model::System* val)
{
    val->SetOwnerDocument( _rootDocument );
    XERCES_CPP_NAMESPACE_QUALIFIER DOMElement* childElement = 
        val->GetXMLData( subElementTagName );
    _veElement->appendChild( childElement );
    return childElement;
}
}

#endif// VE_SYSTEM_H
