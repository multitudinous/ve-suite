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
#include <ves/open/xml/model/System.h>
#include <ves/open/xml/model/Network.h>
#include <ves/open/xml/model/Model.h>

#include <ves/open/xml/DataValuePair.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml;
using namespace ves::open::xml::model;
////////////////////////////////////////////////////////////////////////////////
//Constructor                             //
////////////////////////////////////////////////////////////////////////////////
System::System()
        : XMLObject()
{
    SetObjectType( "System" );
    SetObjectNamespace( "Model" );
    parentModel = NULL;
}
////////////////////////////////////////////////////////////////////////////////
System::~System()
{
    m_models.clear();
}
////////////////////////////////////////////////////////////////////////////////
System::System( const System& input )
        : XMLObject( input )
{
    m_network = new Network( *( input.m_network ) );

    for( size_t i = 0; i < input.m_models.size(); ++i )
    {
        m_models.push_back( new Model( *input.m_models.at( i ) ) );
    }
    parentModel = input.parentModel;
}
////////////////////////////////////////////////////////////////////////////////
System& System::operator=( const System& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        m_network = new Network( *input.m_network );

        m_models.clear();
        for( size_t i = 0; i < input.m_models.size(); ++i )
        {
            m_models.push_back( new Model( *input.m_models.at( i ) ) );
        }
        parentModel = input.parentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void System::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetAttribute( "id", uuid );
    SetSubElement( "network", &( *m_network ) );

    for( size_t i = 0; i < m_models.size(); ++i )
    {
        SetSubElement( "model", &( *m_models.at( i ) ) );
    }
}
////////////////////////////////////////////////////////////////////////////////
void System::AddNetwork( NetworkWeakPtr inputNetwork )
{
    m_network = inputNetwork;
}
////////////////////////////////////////////////////////////////////////////////
NetworkWeakPtr System::GetNetwork()
{
    return m_network;
}
////////////////////////////////////////////////////////////////////////////////
void System::SetObjectFromXMLData( DOMNode* element )
{
    DOMElement* currentElement = 0;
    if( element->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast< DOMElement* >( element );
    }

    if( !currentElement )
    {
        return;
    }

    //Setup uuid for model element
    {
        ves::open::xml::XMLObject::GetAttribute( currentElement, "id", uuid );
    }

    //get variables by tags
    DOMElement* dataValueStringName = 0;
    // for network
    {
        dataValueStringName = GetSubElement( currentElement, "network", 0 );
        m_network = new Network();
        m_network->SetParentModel( parentModel );
        m_network->SetObjectFromXMLData( dataValueStringName );
        dataValueStringName = 0;
    }
    // for models
    {
        DOMNodeList* subElements = currentElement->
                                   getElementsByTagName( ves::open::xml::XMLObject::Convert( "model" ).toXMLString() );
        unsigned int numberOfModels = subElements->getLength();

        for( unsigned int i = 0; i < numberOfModels; ++i )
        {
            if( subElements->item( i )->getParentNode() == currentElement )
            {
                dataValueStringName =
                    static_cast< DOMElement* >( subElements->item( i ) );
                ves::open::xml::model::ModelSharedPtr newModel = new Model();
                newModel->SetParentModel( parentModel );
                m_models.push_back( newModel );
                m_models.back()->SetObjectFromXMLData( dataValueStringName );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
ModelWeakPtr System::GetModel( size_t i )
{
    try
    {
        return m_models.at( i );
    }
    catch ( ... )
    {
        std::cerr << "System::GetModel value greater than number of tags present"
        << std::endl;
        return 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t System::GetNumberOfModels( void )
{
    return m_models.size();
}
////////////////////////////////////////////////////////////////////////////////
void System::AddModel( ModelWeakPtr inputModel )
{
    m_models.push_back( inputModel );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< ModelWeakPtr > System::GetModels()
{
    std::vector< ModelWeakPtr > tempModels;
    std::copy( m_models.begin(),
               m_models.end(),
               std::back_inserter( tempModels ) );
    return tempModels;
}
////////////////////////////////////////////////////////////////////////////////
void System::SetParentModel( ModelSharedPtr parent )
{
    parentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelSharedPtr System::GetParentModel( )
{
    return parentModel;
}