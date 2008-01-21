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
        : ves::open::xml::XMLObject()
{
    _type = std::string( "float" );
    _variableSize = 0;
    _name = std::string( "" );
    _textureUnit = 0;
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
    _type.clear();
    _name.clear();
    _values.clear();
}
/////////////////////////////////////
//Copy constructor                 //
/////////////////////////////////////
Uniform:: Uniform( const Uniform& rhs )
        : ves::open::xml::XMLObject( rhs )
{
    _type = std::string( "float" );
    _variableSize = 0;
    _name = std::string( "" );
    _textureUnit = 0;

    _type = rhs._type;
    _variableSize = rhs._variableSize;
    _name = rhs._name;
    _textureUnit = rhs._textureUnit;
    for( unsigned int i = 0; i < rhs._values.size(); i++ )
    {
        _values.push_back( rhs._values.at( i ) );
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
        _type = rhs._type;
        _variableSize = rhs._variableSize;
        _name = rhs._name;
        _textureUnit = rhs._textureUnit;
        _values.clear();
        for( unsigned int i = 0; i < rhs._values.size(); i++ )
        {
            _values.push_back( rhs._values.at( i ) );
        }
    }
    return *this;
}
///////////////////////////////////////
void Uniform::SetType( std::string type )
{
    _type = type;
}
///////////////////////////////////////////////
void Uniform::SetSize( unsigned int uniformSize )
{
    _variableSize = uniformSize;
}
///////////////////////////////////////
void Uniform::SetName( std::string name )
{
    _name = name;
}
////////////////////////////////////////////////
void Uniform::SetTextureUnit( unsigned int tUnit )
{
    _textureUnit = tUnit;
}
/////////////////////////////////////////////////////
void Uniform::SetValues( std::vector<float> newValues )
{
    _values.clear();
    for( unsigned int i = 0; i < newValues.size(); i++ )
    {
        _values.push_back( newValues.at( i ) );
    }
    _variableSize = _values.size();
}
//////////////////////////////
std::string Uniform::GetType()
{
    return _type;
}
//////////////////////////////
std::string Uniform::GetName()
{
    return _name;
}
///////////////////////////////
size_t Uniform::GetSize()
{
    return _variableSize;
}
//////////////////////////////////////
unsigned int Uniform::GetTextureUnit()
{
    return _textureUnit;
}
///////////////////////////////////////
std::vector<float> Uniform::GetValues()
{
    return _values;
}
//////////////////////////////////////////////////
void Uniform::_updateVEElement( std::string input )
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
    DOMElement* nodeSizeElement = _rootDocument->createElement( xercesString( "size" ) );
    DOMText* nodeSize = _rootDocument->createTextNode( xercesString( static_cast<int>( _variableSize ) ) );
    nodeSizeElement->appendChild( nodeSize );
    _veElement->appendChild( nodeSizeElement );
}
//////////////////////////////////
void Uniform::_updateUniformName()
{
    DOMElement* nodeNameElement = _rootDocument->createElement( xercesString( "name" ) );
    DOMText* nodeName = _rootDocument->createTextNode( xercesString( _name.c_str() ) );
    nodeNameElement->appendChild( nodeName );
    _veElement->appendChild( nodeNameElement );
}
//////////////////////////////////
void Uniform::_updateUniformType()
{
    DOMElement* nodeTypeElement = _rootDocument->createElement( xercesString( "type" ) );
    DOMText* nodeType = _rootDocument->createTextNode( xercesString( _type ) );
    nodeTypeElement->appendChild( nodeType );
    _veElement->appendChild( nodeTypeElement );
}
/////////////////////////////
void Uniform::_updateValues()
{
    for( unsigned int i = 0; i < _values.size(); ++i )
    {
        // name comes from verg.xsd
        DOMElement* valueTag  = _rootDocument->createElement( xercesString( "value" ) );
        _veElement->appendChild( valueTag );
        DOMText* valueNum = _rootDocument->createTextNode( xercesString( _values.at( i ) ) );
        valueTag->appendChild( valueNum );
    }
}
//////////////////////////////////
void Uniform::_updateTextureUnit()
{
    DOMElement* tUnitElement = _rootDocument->createElement( xercesString( "textureUnit" ) );
    DOMText* nodeTUnit = _rootDocument->createTextNode( xercesString( static_cast<int>( _textureUnit ) ) );
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
                    _name = ExtractFromSimpleElement< std::string >( nameNode );
                }
                //get the type
                DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
                if( typeNode )
                {
                    _type = ExtractFromSimpleElement< std::string >( typeNode );
                    //if it is a texture get the texture unit
                    if( _type == std::string( "Sampler" ) )
                    {
                        DOMElement* tUnitNode = GetSubElement( currentElement, std::string( "textureUnit" ), 0 );
                        if( tUnitNode )
                        {
                            _textureUnit = ExtractFromSimpleElement< unsigned int >( tUnitNode );
                        }
                    }
                }
                //Get the size of this uniform
                DOMElement* lengthNode = GetSubElement( currentElement, std::string( "size" ), 0 );
                if( lengthNode )
                {
                    _variableSize = ExtractFromSimpleElement< unsigned int >( lengthNode );
                }
                //get the values for this uniform
                if( _type != std::string( "Sampler" ) )
                {
                    DOMElement* uniformValue = 0;
                    _values.clear();
                    for( unsigned int i = 0; i < _variableSize; i++ )
                    {
                        uniformValue = GetSubElement( currentElement, std::string( "value" ), i );
                        if( uniformValue )
                        {
                            _values.push_back( static_cast<float>( ExtractFromSimpleElement< double >( uniformValue ) ) );
                        }
                    }
                }
            }
        }
    }
}
