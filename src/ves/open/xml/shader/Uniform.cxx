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
#include <ves/open/xml/shader/Uniform.h>

#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/XMLObjectFactory.h>


XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml::shader;
using namespace ves::open::xml;
//////////////////////////////////////////////////////////////////////////
///Constructor                                                          //
//////////////////////////////////////////////////////////////////////////
Uniform::Uniform()
        : XMLObject()
{
    mType = std::string( "float" );
    mVariableSize = 0;
    mName = std::string( "" );
    mTextureUnit = 0;
    SetObjectType( "Uniform" );
    SetObjectNamespace( "Shader" );

    if( !XMLObjectFactory::Instance()->ObjectCreatorIsRegistered( "Shader" ) )
    {
        XMLObjectFactory::Instance()->RegisterObjectCreator( "Shader", new ShaderCreator() );
    }
}
///////////////////
Uniform::~Uniform()
{
    mType.clear();
    mName.clear();
    mValues.clear();
}
/////////////////////////////////////
//Copy constructor                 //
/////////////////////////////////////
Uniform:: Uniform( const Uniform& rhs )
        : XMLObject( rhs )
{
    mType = std::string( "float" );
    mVariableSize = 0;
    mName = std::string( "" );
    mTextureUnit = 0;

    mType = rhs.mType;
    mVariableSize = rhs.mVariableSize;
    mName = rhs.mName;
    mTextureUnit = rhs.mTextureUnit;
    for( unsigned int i = 0; i < rhs.mValues.size(); i++ )
    {
        mValues.push_back( rhs.mValues.at( i ) );
    }
}
///////////////////////////////////////////////
//Equal operator                             //
///////////////////////////////////////////////
Uniform& Uniform::operator=( const Uniform& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        mType = rhs.mType;
        mVariableSize = rhs.mVariableSize;
        mName = rhs.mName;
        mTextureUnit = rhs.mTextureUnit;
        mValues.clear();
        for( unsigned int i = 0; i < rhs.mValues.size(); i++ )
        {
            mValues.push_back( rhs.mValues.at( i ) );
        }
    }
    return *this;
}
///////////////////////////////////////
void Uniform::SetType( const std::string& type )
{
    mType = type;
}
///////////////////////////////////////////////
void Uniform::SetSize( unsigned int uniformSize )
{
    mVariableSize = uniformSize;
}
///////////////////////////////////////
void Uniform::SetName( const std::string& name )
{
    mName = name;
}
////////////////////////////////////////////////
void Uniform::SetTextureUnit( unsigned int tUnit )
{
    mTextureUnit = tUnit;
}
/////////////////////////////////////////////////////
void Uniform::SetValues( const std::vector<float>& newValues )
{
    mValues.clear();
    for( unsigned int i = 0; i < newValues.size(); i++ )
    {
        mValues.push_back( newValues.at( i ) );
    }
    mVariableSize = mValues.size();
}
//////////////////////////////
const std::string& Uniform::GetType()
{
    return mType;
}
//////////////////////////////
const std::string& Uniform::GetName()
{
    return mName;
}
///////////////////////////////
size_t Uniform::GetSize()
{
    return mVariableSize;
}
//////////////////////////////////////
unsigned int Uniform::GetTextureUnit()
{
    return mTextureUnit;
}
///////////////////////////////////////
const std::vector<float>& Uniform::GetValues()
{
    return mValues;
}
//////////////////////////////////////////////////
void Uniform::_updateVEElement( const std::string& input )
{
    _updateUniformName();
    _updateUniformType();
    _updateSize();
    _updateTextureUnit();
    _updateValues();
}
///////////////////////////
void Uniform::_updateSize()
{
    DOMElement* nodeSizeElement = _rootDocument->createElement(
                                  Convert( "size" ).toXMLString() );

    DOMText* nodeSize = _rootDocument->createTextNode(
                        Convert( static_cast<int>( mVariableSize) ).toXMLString() );

    nodeSizeElement->appendChild( nodeSize );
    _veElement->appendChild( nodeSizeElement );
}
//////////////////////////////////
void Uniform::_updateUniformName()
{
    DOMElement* nodeNameElement = _rootDocument->createElement(
                                  Convert( "name" ).toXMLString() );

    DOMText* nodeName = _rootDocument->createTextNode(
                        Convert( mName ).toXMLString() );

    nodeNameElement->appendChild( nodeName );
    _veElement->appendChild( nodeNameElement );
}
//////////////////////////////////
void Uniform::_updateUniformType()
{
    DOMElement* nodeTypeElement = _rootDocument->createElement(
                                  Convert( "type" ).toXMLString() );

    DOMText* nodeType = _rootDocument->createTextNode(
                        Convert( mType ).toXMLString() );

    nodeTypeElement->appendChild( nodeType );
    _veElement->appendChild( nodeTypeElement );
}
/////////////////////////////
void Uniform::_updateValues()
{
    for( unsigned int i = 0; i < mValues.size(); ++i )
    {
        // name comes from verg.xsd
        DOMElement* valueTag  = _rootDocument->createElement(
                                Convert( "value" ).toXMLString() );

        _veElement->appendChild( valueTag );
        DOMText* valueNum = _rootDocument->createTextNode(
                            Convert( mValues.at( i ) ).toXMLString() );

        valueTag->appendChild( valueNum );
    }
}
//////////////////////////////////
void Uniform::_updateTextureUnit()
{
    DOMElement* tUnitElement = _rootDocument->createElement(
                               Convert( "textureUnit" ).toXMLString() );

    DOMText* nodeTUnit = _rootDocument->createTextNode(
                         Convert( static_cast<int>( mTextureUnit ) ).toXMLString() );

    tUnitElement->appendChild( nodeTUnit );
    _veElement->appendChild( tUnitElement );
}
////////////////////////////////////////////////////
void Uniform::SetObjectFromXMLData( DOMNode* xmlNode )
{
    DOMElement* currentElement = 0;
    const XMLCh* name;
    if( xmlNode->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        name = xmlNode->getNodeName();
        currentElement = dynamic_cast<DOMElement*>( xmlNode );
    }

    if( currentElement )
    {
        //break down the element
        {
            if( currentElement->hasChildNodes() )
            {
                //Get the name of the uniform
                DOMElement* nameNode = GetSubElement( currentElement, std::string( "name" ), 0 );
                if( nameNode )
                {
                    GetAttribute( nameNode, "name", mName );
                }
                //get the type
                DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
                if( typeNode )
                {
                    GetAttribute( typeNode, "type", mType );
                    //if it is a texture get the texture unit
                    if( _type == std::string( "Sampler" ) )
                    {
                        DOMElement* tUnitNode = GetSubElement( currentElement, std::string( "textureUnit" ), 0 );
                        if( tUnitNode )
                        {
                            GetAttribute( tUnitNode, "textureUnit", mTextureUnit );
                        }
                    }
                }
                //Get the size of this uniform
                DOMElement* lengthNode = GetSubElement( currentElement, std::string( "size" ), 0 );
                if( lengthNode )
                {
                     GetAttribute( lengthNode, "size", mVariableSize );
                }
                //get the values for this uniform
                if( _type != std::string( "Sampler" ) )
                {
                    DOMElement* uniformValue = 0;
                    mValues.clear();
                    for( unsigned int i = 0; i < mVariableSize; i++ )
                    {
                        uniformValue = GetSubElement( currentElement, std::string( "value" ), i );
                        if( uniformValue )
                        {
                            double tmp_float;
                            GetAttribute( uniformValue, "value", tmp_float);
                            mValues.push_back( static_cast<float>( tmp_float ) );
                        }
                    }
                }
            }
        }
    }
}
