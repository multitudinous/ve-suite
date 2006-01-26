/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Open/XML/Shader/Shader.h"

#include "VE_Open/XML/Shader/Uniform.h"
#include "VE_Open/XML/Shader/TextureImage.h"

using namespace VE_Shader;
using namespace VE_XML;
XERCES_CPP_NAMESPACE_USE
////////////////////////////////////////////////////////////////////////
//Constructor                                                         //
////////////////////////////////////////////////////////////////////////
Shader::Shader()
:VE_XML::XMLObject()
{
   _shaderType = std::string("Vertex");
   _shaderSource = std::string("");
}
/////////////////
//Destructor   //
/////////////////
Shader::~Shader()
{
   size_t nUniforms = _uniformList.size();
   for(size_t i = nUniforms -1; i >=0; i--)
   {
      delete _uniformList.at(i);
   }
   _uniformList.clear();
   
   size_t nTextures = _textureImages.size();
   for(size_t i = nTextures-1; i >=0; i--)
   {
      delete _textureImages.at(i);
   }
   _textureImages.clear();
}
/////////////////////////////////
///Copy constructor            //
/////////////////////////////////
Shader::Shader(const Shader& rhs)
:XMLObject(rhs)
{
   for(size_t i = 0; i < rhs._uniformList.size(); i++)
   {
      _uniformList.push_back(rhs._uniformList.at(i));
   }

   for(size_t i = 0; i < rhs._textureImages.size(); i++)
   {
      _textureImages.push_back(rhs._textureImages.at(i));
   }
   _shaderType = rhs._shaderType;
   _shaderSource = rhs._shaderSource;
}
////////////////////////////////////////////////////
void Shader::SetObjectFromXMLData(DOMNode* xmlInput)
{
   DOMElement* currentElement = 0;

   if(xmlInput->getNodeType() == DOMNode::ELEMENT_NODE)
   {
      currentElement = dynamic_cast<DOMElement*>(xmlInput);
   }
   
   if(currentElement)
   {
      //break down the element
      {
         if(currentElement->hasChildNodes())
         {
            //Get the type of shader
            DOMElement* typeNode = GetSubElement(currentElement,std::string("type"),0);
            if(typeNode)
            {
                _shaderType = ExtractDataStringFromSimpleElement( typeNode );
            }

            //Get the source
            DOMElement* sourceNode = GetSubElement(currentElement,std::string("shaderCode"),0);
            if(sourceNode)
            {
                _shaderSource = ExtractDataStringFromSimpleElement( sourceNode );
            }
            //clear out the current list of uniforms
            if(_uniformList.size()){
               size_t nUniforms = _uniformList.size();
               for(size_t i = nUniforms -1; i >=0; i--)
               {
                  delete _uniformList.at(i);
               }
               _uniformList.clear();
            }
            //clear out the current list of texture images
            if(_textureImages.size()){
               size_t nTextureImages = _textureImages.size();
               for(size_t i = nTextureImages-1; i >=0; i--)
               {
                  delete _textureImages.at(i);
               }
               _textureImages.clear();
            }

            //populate the uniforms
            {
               DOMNodeList* uniformList = currentElement->getElementsByTagName(xercesString("uniform"));
               unsigned int nUniforms = uniformList->getLength();
               for(unsigned int i = 0; i < nUniforms; i++)
               {
                  Uniform* newUniform = new Uniform();
                  newUniform->SetObjectFromXMLData(uniformList->item(i));
                  _uniformList.push_back(newUniform);
               }
            }
            //populate the texture images
            {
               DOMNodeList* textureList = currentElement->getElementsByTagName(xercesString("texture"));
               unsigned int nTextures = textureList->getLength();
               for(unsigned int i = 0; i < nTextures; i++)
               {
                  TextureImage* newTexture= new TextureImage();
                  newTexture->SetObjectFromXMLData(textureList->item(i));
                  _textureImages.push_back(newTexture);
               }
            }
         }
      }
   }
}
////////////////////////////////////////////
void Shader::AddUniform(Uniform* newUniform)
{
   _uniformList.push_back(newUniform);
}
///////////////////////////////////////////////////////////
void Shader::AddTextureImage(TextureImage* newTextureImage)
{
   _textureImages.push_back(newTextureImage);
}
//////////////////////////////////////////////////
void Shader::SetShaderType(std::string fragOrVert)
{
   _shaderType = fragOrVert;
}
//////////////////////////////////////////////////////////
void Shader::SetShaderSource(std::string shaderSourceCode)
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
TextureImage* Shader::GetTextureImage(unsigned int textureUnit)
{
   return _textureImages.at(textureUnit);
}
////////////////////////////////////////////////////
Uniform* Shader::GetUniform(std::string uniformName)
{
   size_t nUniforms = _uniformList.size();
   for(size_t i = 0; i < nUniforms; i++)
   {
      if(_uniformList.at(i)->GetName() == uniformName)
      {
         return _uniformList.at(i);
      }
   }
   return 0;
}
////////////////////////////////////////////////
Uniform* Shader::GetUniform(unsigned int index)
{
   return _uniformList.at(index);
}
////////////////////////////////////////////////
void Shader::_updateVEElement(std::string input)
{
   if( !_veElement )
   {
      // name comes from verg.xsd
      _veElement = _rootDocument->createElement( xercesString( input ) );
   }
   _updateTextureImages();
   _updateUniforms();
   _updateShaderType();
   _updateShaderSource();
}
///////////////////////////////////
void Shader::_updateTextureImages()
{
   //add the children nodes to the list
   for(size_t i = 0; i < _textureImages.size(); i++)
   {
      _textureImages.at(i)->SetOwnerDocument(_rootDocument);
      _veElement->appendChild(_textureImages.at(i)->GetXMLData("texture"));
   }
}
//////////////////////////////
void Shader::_updateUniforms()
{
   //add the children nodes to the list
   for(size_t i = 0; i < _uniformList.size(); i++)
   {
      _uniformList.at(i)->SetOwnerDocument(_rootDocument);
      _veElement->appendChild(_uniformList.at(i)->GetXMLData("uniform"));
   }
}
////////////////////////////////
void Shader::_updateShaderType()
{
   DOMElement* typeElement = _rootDocument->createElement(xercesString("type"));
   DOMText* type = _rootDocument->createTextNode(xercesString(_shaderType));
   typeElement->appendChild(type);
   _veElement->appendChild(typeElement);
}
//////////////////////////////////
void Shader::_updateShaderSource()
{
   DOMElement* sourceElement = _rootDocument->createElement(xercesString("shaderSource"));
   DOMText* source = _rootDocument->createTextNode(xercesString(_shaderSource));
   sourceElement->appendChild(source);
   _veElement->appendChild(sourceElement);
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
Shader& Shader::operator=(const Shader& rhs)
{
   if(this != &rhs)
   {
      XMLObject::operator =(rhs);
      size_t nUniforms = _uniformList.size();
      for(size_t i = nUniforms -1; i >=0; i--)
      {
         delete _uniformList.at(i);
      }
      _uniformList.clear();
   
      for(size_t i = 0; i < rhs._uniformList.size(); i++)
      {
         _uniformList.push_back(rhs._uniformList.at(i));
      }
      size_t nTextures = _textureImages.size();
      for(size_t i = nTextures-1; i >=0; i--)
      {
         delete _textureImages.at(i);
      }
      _textureImages.clear();

      for(size_t i = 0; i < rhs._textureImages.size(); i++)
      {
         _textureImages.push_back(rhs._textureImages.at(i));
      }
      _shaderType = rhs._shaderType;
      _shaderSource = rhs._shaderSource;
   }
   return *this;
}
