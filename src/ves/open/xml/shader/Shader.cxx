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
#include <ves/open/xml/shader/Shader.h>

#include <ves/open/xml/shader/Uniform.h>
#include <ves/open/xml/shader/TextureImage.h>
#include <ves/open/xml/XMLObjectFactory.h>

#include <ves/open/xml/shader/ShaderCreator.h>


using namespace ves::open::xml::shader;
using namespace ves::open::xml;
XERCES_CPP_NAMESPACE_USE
////////////////////////////////////////////////////////////////////////
//Constructor                                                         //
////////////////////////////////////////////////////////////////////////
Shader::Shader()
        : ves::open::xml::XMLObject()
{
    _shaderType = std::string( "Vertex" );
    _shaderSource = std::string( "" );
    SetObjectType( "Shader" );
    SetObjectNamespace( "Shader" );
}
/////////////////
//Destructor   //
/////////////////
Shader::~Shader()
{
    _shaderSource.clear();
    _uniformList.clear();
    _textureImages.clear();
}
/////////////////////////////////
///Copy constructor            //
/////////////////////////////////
Shader::Shader( const Shader& rhs )
        : XMLObject( rhs )
{
    _shaderSource = std::string( "" );
    for( size_t i = 0; i < rhs._uniformList.size(); i++ )
    {
        _uniformList.push_back( rhs._uniformList.at( i ) );
    }

    _textureImages = rhs._textureImages;
    _shaderType = rhs._shaderType;
    _shaderSource = rhs._shaderSource;
}
////////////////////////////////////////////////////
void Shader::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;

    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( currentElement )
    {
        //break down the element
        {
            if( currentElement->hasChildNodes() )
            {
                //Get the type of shader
                DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
                if( typeNode )
                {
                    GetAttribute( typeNode, "type", _shaderType );
                }

                //Get the source
                DOMElement* sourceNode = GetSubElement( currentElement, std::string( "shaderCode" ), 0 );
                if( sourceNode )
                {
                    GetAttribute( sourceNode, "type", _shaderSource );
                }
                //clear out the current list of uniforms
                if( _uniformList.size() )
                {
                    _uniformList.clear();
                }
                //clear out the current list of texture images
                if( _textureImages.size() )
                {
                    _textureImages.clear();
                }

                //populate the uniforms
                {
                    DOMNodeList* uniformList = currentElement->getElementsByTagName( Convert( "uniform" ).toXMLString() );
                    unsigned int nUniforms = uniformList->getLength();
                    for( unsigned int i = 0; i < nUniforms; i++ )
                    {
                        Uniform newUniform;
                        newUniform.SetObjectFromXMLData( uniformList->item( i ) );
                        _uniformList.push_back( newUniform );
                    }
                }
                //populate the texture images
                {
                    DOMNodeList* textureList = currentElement->getElementsByTagName( Convert( "textureImage" ).toXMLString() );
                    unsigned int nTextures = textureList->getLength();
                    //std::cout<<"Texture Images in shader: "<<nTextures<<std::endl;
                    for( unsigned int i = 0; i < nTextures; i++ )
                    {
                        std::cout << "Adding texture image." << std::endl;
                        TextureImage newTexture;
                        newTexture.SetObjectFromXMLData( textureList->item( i ) );
                        AddTextureImage( newTexture );
                    }
                }
            }
        }
    }
}
////////////////////////////////////////////
void Shader::AddUniform( Uniform newUniform )
{
    _uniformList.push_back( newUniform );
}
///////////////////////////////////////////////////////////
void Shader::AddTextureImage( TextureImage newTextureImage )
{
    _textureImages.insert( std::pair<unsigned int, TextureImage>( newTextureImage.GetTextureUnit(), newTextureImage ) );
}
//////////////////////////////////////////////////
void Shader::SetShaderType( std::string fragOrVert )
{
    _shaderType = fragOrVert;
}
//////////////////////////////////////////////////////////
void Shader::SetShaderSource( std::string shaderSourceCode )
{
    _shaderSource = shaderSourceCode;
}
/////////////////////////////////////
std::string Shader::GetShaderSource()
{
    return _shaderSource;
}
///////////////////////////////////
std::string Shader::GetShaderType()
{
    return _shaderType;
}
////////////////////////////////////////////////////////////////
TextureImage& Shader::GetTextureImage( unsigned int textureUnit )
{
    try
    {
        if( !_textureImages.size() )
            throw( "No textures present in shader!!" );
        return _textureImages[textureUnit];
    }
    catch ( const char* msg )
    {
        std::cout << "ERROR: " << msg << std::endl;
    }
    catch ( ... )
    {
        std::cout << "Texture Unit: " << textureUnit << " doesn't exist!" << std::endl;
        std::cout << "Shader::GetTextureImage() " << std::endl;
    }
}
////////////////////////////////////////////////////
Uniform& Shader::GetUniform( std::string uniformName )
{
    size_t nUniforms = _uniformList.size();
    for( size_t i = 0; i < nUniforms; i++ )
    {
        if( _uniformList.at( i ).GetName() == uniformName )
        {
            return _uniformList.at( i );
        }
    }
    //return 0x0000000;
}
////////////////////////////////////////////////
Uniform& Shader::GetUniform( unsigned int index )
{
    return _uniformList.at( index );
}
////////////////////////////////////////////////
void Shader::_updateVEElement( const std::string& input )
{
    _updateShaderType();
    _updateTextureImages();
    _updateUniforms();

    _updateShaderSource();
}
///////////////////////////////////
void Shader::_updateTextureImages()
{
    //add the children nodes to the list
    std::map<unsigned int, TextureImage>::iterator textures;
    //for(size_t i = 0; i < _textureImages.size(); i++)
    for( textures = _textureImages.begin();
            textures != _textureImages.end();
            textures++ )
    {
        textures->second.SetOwnerDocument( _rootDocument );
        _veElement->appendChild( textures->second.GetXMLData( "textureImage" ) );
    }
}
//////////////////////////////
void Shader::_updateUniforms()
{
    //add the children nodes to the list
    for( size_t i = 0; i < _uniformList.size(); i++ )
    {
        _uniformList.at( i ).SetOwnerDocument( _rootDocument );
        _veElement->appendChild( _uniformList.at( i ).GetXMLData( "uniform" ) );
    }
}
////////////////////////////////
void Shader::_updateShaderType()
{
    DOMElement* typeElement = _rootDocument->createElement(
                              Convert( "type" ).toXMLString() );

    DOMText* type = _rootDocument->createTextNode(
                    Convert( _shaderType ).toXMLString() );

    typeElement->appendChild( type );
    _veElement->appendChild( typeElement );
}
//////////////////////////////////
void Shader::_updateShaderSource()
{
    DOMElement* sourceElement = _rootDocument->createElement(
                                Convert( "shaderCode" ).toXMLString() );

    DOMText* source = _rootDocument->createTextNode(
                      Convert( _shaderSource ).toXMLString() );

    sourceElement->appendChild( source );
    _veElement->appendChild( sourceElement );
}
////////////////////////////////////
size_t Shader::GetNumberOfUniforms()
{
    return _uniformList.size();
}
/////////////////////////////////////////
size_t Shader::GetNumberOfTextureImages()
{
    return _textureImages.size();
}
////////////////////////////////////////////
Shader& Shader::operator=( const Shader& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        /*size_t nUniforms = _uniformList.size();
        for(size_t i = nUniforms -1; i >=0; i--)
        {
           delete _uniformList.at(i);
        }*/
        _uniformList.clear();

        for( size_t i = 0; i < rhs._uniformList.size(); i++ )
        {
            _uniformList.push_back( rhs._uniformList.at( i ) );
        }
        /*size_t nTextures = _textureImages.size();
        for(size_t i = nTextures-1; i >=0; i--)
        {
           delete _textureImages.at(i);
        }*/
        _textureImages.clear();

        //for(size_t i = 0; i < rhs._textureImages.size(); i++)
        //{
        //is this correct for maps?
        _textureImages = rhs._textureImages;
        //}
        _shaderType = rhs._shaderType;
        _shaderSource = rhs._shaderSource;
    }
    return *this;
}
