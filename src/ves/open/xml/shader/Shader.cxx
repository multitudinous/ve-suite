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
    mShaderType = std::string( "Vertex" );
    mShaderSource = std::string( "" );
    SetObjectType( "Shader" );
    SetObjectNamespace( "Shader" );
}
/////////////////
//Destructor   //
/////////////////
Shader::~Shader()
{
    mShaderSource.clear();
    mUniformList.clear();
    mTextureImages.clear();
}
/////////////////////////////////
///Copy constructor            //
/////////////////////////////////
Shader::Shader( const Shader& rhs )
        : XMLObject( rhs )
{
    mShaderSource = std::string( "" );
    for( size_t i = 0; i < rhs.mUniformList.size(); i++ )
    {
        mUniformList.push_back( rhs.mUniformList.at( i ) );
    }

    mTextureImages = rhs.mTextureImages;
    mShaderType = rhs.mShaderType;
    mShaderSource = rhs.mShaderSource;
}
////////////////////////////////////////////////////
void Shader::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;

    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( !currentElement )
    {
        return;
    }

    if( !currentElement->hasChildNodes() )
    {
        return;
    }
    //Get the type of shader
    DOMElement* typeNode = GetSubElement( currentElement, std::string( "type" ), 0 );
    if( typeNode )
    {
        GetDataFromElement( typeNode, mShaderType );
    }

    //Get the source
    DOMElement* sourceNode = GetSubElement( currentElement, std::string( "shaderCode" ), 0 );
    if( sourceNode )
    {
        GetDataFromElement( sourceNode, mShaderSource );
    }
    //clear out the current list of uniforms
    if( mUniformList.size() )
    {
        mUniformList.clear();
    }
    //clear out the current list of texture images
    if( mTextureImages.size() )
    {
        mTextureImages.clear();
    }

    //populate the uniforms
    {
        DOMNodeList* uniformList = currentElement->getElementsByTagName( Convert( "uniform" ).toXMLString() );
        unsigned int nUniforms = uniformList->getLength();
        for( unsigned int i = 0; i < nUniforms; i++ )
        {
            Uniform newUniform;
            newUniform.SetObjectFromXMLData( uniformList->item( i ) );
            mUniformList.push_back( newUniform );
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
////////////////////////////////////////////
void Shader::AddUniform( Uniform newUniform )
{
    mUniformList.push_back( newUniform );
}
///////////////////////////////////////////////////////////
void Shader::AddTextureImage( TextureImage newTextureImage )
{
    mTextureImages.insert( std::pair<unsigned int, TextureImage>( newTextureImage.GetTextureUnit(), newTextureImage ) );
}
//////////////////////////////////////////////////
void Shader::SetShaderType( const std::string& fragOrVert )
{
    mShaderType = fragOrVert;
}
//////////////////////////////////////////////////////////
void Shader::SetShaderSource( const std::string& shaderSourceCode )
{
    mShaderSource = shaderSourceCode;
}
/////////////////////////////////////
const std::string& Shader::GetShaderSource()
{
    return mShaderSource;
}
///////////////////////////////////
const std::string& Shader::GetShaderType()
{
    return mShaderType;
}
////////////////////////////////////////////////////////////////
const TextureImage& Shader::GetTextureImage( unsigned int textureUnit )
{
    try
    {
        if( !mTextureImages.size() )
            throw( "No textures present in shader!!" );
        return mTextureImages[textureUnit];
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
const Uniform& Shader::GetUniform( const std::string& uniformName )
{
    size_t nUniforms = mUniformList.size();
    for( size_t i = 0; i < nUniforms; i++ )
    {
        if( mUniformList.at( i ).GetName() == uniformName )
        {
            return mUniformList.at( i );
        }
    }
    //return 0x0000000;
}
////////////////////////////////////////////////
const Uniform& Shader::GetUniform( unsigned int index )
{
    return mUniformList.at( index );
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
    //for(size_t i = 0; i < mTextureImages.size(); i++)
    for( textures = mTextureImages.begin();
            textures != mTextureImages.end();
            textures++ )
    {
        textures->second.SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( textures->second.GetXMLData( "textureImage" ) );
    }
}
//////////////////////////////
void Shader::_updateUniforms()
{
    //add the children nodes to the list
    for( size_t i = 0; i < mUniformList.size(); i++ )
    {
        mUniformList.at( i ).SetOwnerDocument( mRootDocument );
        mVeElement->appendChild( mUniformList.at( i ).GetXMLData( "uniform" ) );
    }
}
////////////////////////////////
void Shader::_updateShaderType()
{
    DOMElement* typeElement = mRootDocument->createElement(
                              Convert( "type" ).toXMLString() );

    DOMText* type = mRootDocument->createTextNode(
                    Convert( mShaderType ).toXMLString() );

    typeElement->appendChild( type );
    mVeElement->appendChild( typeElement );
}
//////////////////////////////////
void Shader::_updateShaderSource()
{
    DOMElement* sourceElement = mRootDocument->createElement(
                                Convert( "shaderCode" ).toXMLString() );

    DOMText* source = mRootDocument->createTextNode(
                      Convert( mShaderSource ).toXMLString() );

    sourceElement->appendChild( source );
    mVeElement->appendChild( sourceElement );
}
////////////////////////////////////
size_t Shader::GetNumberOfUniforms()
{
    return mUniformList.size();
}
/////////////////////////////////////////
size_t Shader::GetNumberOfTextureImages()
{
    return mTextureImages.size();
}
////////////////////////////////////////////
Shader& Shader::operator=( const Shader& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        /*size_t nUniforms = mUniformList.size();
        for(size_t i = nUniforms -1; i >=0; i--)
        {
           delete mUniformList.at(i);
        }*/
        mUniformList.clear();

        for( size_t i = 0; i < rhs.mUniformList.size(); i++ )
        {
            mUniformList.push_back( rhs.mUniformList.at( i ) );
        }
        /*size_t nTextures = mTextureImages.size();
        for(size_t i = nTextures-1; i >=0; i--)
        {
           delete mTextureImages.at(i);
        }*/
        mTextureImages.clear();

        //for(size_t i = 0; i < rhs.mTextureImages.size(); i++)
        //{
        //is this correct for maps?
        mTextureImages = rhs.mTextureImages;
        //}
        mShaderType = rhs.mShaderType;
        mShaderSource = rhs.mShaderSource;
    }
    return *this;
}
