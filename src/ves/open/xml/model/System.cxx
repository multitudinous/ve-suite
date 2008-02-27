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
    mParentModel = ModelPtr();
}
////////////////////////////////////////////////////////////////////////////////
System::~System()
{
    if( mNetwork )
    {
        mNetwork->SetParentModel( ModelPtr() );
    }

    for( size_t i = 0; i < mModels.size(); ++i )
    {
        mModels.at( i )->SetParentModel( ModelPtr() );
    }
    mModels.clear();
}
////////////////////////////////////////////////////////////////////////////////
System::System( const System& input )
        : XMLObject( input )
{
    mNetwork = NetworkPtr( new Network( *(  input.mNetwork ) ) );

    for( size_t i = 0; i < input.mModels.size(); ++i )
    {
        mModels.push_back( ModelPtr( new Model( *input.mModels.at( i ) ) ) );
    }
    mParentModel = input.mParentModel;
}
////////////////////////////////////////////////////////////////////////////////
System& System::operator=( const System& input )
{
    if( this != &input )
    {
        //biv-- make sure to call the parent =
        XMLObject::operator =( input );
        mNetwork = NetworkPtr( new Network(  *input.mNetwork ) );

        mModels.clear();
        for( size_t i = 0; i < input.mModels.size(); ++i )
        {
            mModels.push_back( ModelPtr( new Model( *input.mModels.at( i ) ) ) );
        }
        mParentModel = input.mParentModel;
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
void System::_updateVEElement( const std::string& input )
{
    // write all the elements according to verg_model.xsd
    SetAttribute( "id", mUuid );
    SetSubElement<ves::open::xml::XMLObjectPtr>( "network", mNetwork );
    SetSubElements( "model", mModels );
}
////////////////////////////////////////////////////////////////////////////////
void System::AddNetwork( NetworkPtr inputNetwork )
{
    mNetwork = inputNetwork;
}
////////////////////////////////////////////////////////////////////////////////
NetworkPtr System::GetNetwork()
{
    return mNetwork;
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
        ves::open::xml::XMLObject::GetAttribute( currentElement, "id", mUuid );
    }

    //get variables by tags
    DOMElement* dataValueStringName = 0;
    // for network
    {
        dataValueStringName = GetSubElement( currentElement, "network", 0 );
        mNetwork = NetworkPtr( new Network() );
        mNetwork->SetParentModel( mParentModel );
        mNetwork->SetObjectFromXMLData( dataValueStringName );
        dataValueStringName = 0;
    }
    // for models
    {
        DOMNodeList* subElements = currentElement->getElementsByTagName(
                                   Convert( "model" ).toXMLString() );

        unsigned int numberOfModels = subElements->getLength();

        for( unsigned int i = 0; i < numberOfModels; ++i )
        {
            if( subElements->item( i )->getParentNode() == currentElement )
            {
                dataValueStringName =
                    static_cast< DOMElement* >( subElements->item( i ) );
                ves::open::xml::model::ModelSharedPtr newModel( new Model() );
                newModel->SetParentModel( mParentModel );
                mModels.push_back( newModel );
                mModels.back()->SetObjectFromXMLData( dataValueStringName );
            }
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
ModelPtr System::GetModel( size_t i )
{
    try
    {
        return mModels.at( i );
    }
    catch ( ... )
    {
        std::cerr << "System::GetModel value greater than number of tags present"
        << std::endl;
        return ModelPtr();
    }
}
////////////////////////////////////////////////////////////////////////////////
size_t System::GetNumberOfModels( void )
{
    return mModels.size();
}
////////////////////////////////////////////////////////////////////////////////
void System::AddModel( ModelPtr inputModel )
{
    mModels.push_back( inputModel );
}
////////////////////////////////////////////////////////////////////////////////
std::vector< ModelPtr > System::GetModels()
{
    std::vector< ModelPtr > tempModels;
    std::copy( mModels.begin(),
               mModels.end(),
               std::back_inserter( tempModels ) );
    return tempModels;
}
////////////////////////////////////////////////////////////////////////////////
void System::SetParentModel( ModelSharedPtr parent )
{
    mParentModel = parent;
}
////////////////////////////////////////////////////////////////////////////////
ModelSharedPtr System::GetParentModel( )
{
    return mParentModel;
}
