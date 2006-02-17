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
#include "VE_Open/XML/Shader/TextureImage.h"
#include "VE_Open/XML/Shader/ShaderCreator.h"
#include "VE_Open/XML/XMLObjectFactory.h"
using namespace VE_Shader;
using namespace VE_XML;
////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                     //
////////////////////////////////////////////////////////////////////////////////////
TextureImage::TextureImage()
:VE_XML::XMLObject()
{
   _textureType = "2D";
   _textureUnit = 0;
   _dimension = 2;
   SetObjectType("TextureImage");
   SetObjectNamespace("Shader");
 
   if(!XMLObjectFactory::Instance()->ObjectCreatorIsRegistered("Shader"))
   {
      XMLObjectFactory::Instance()->RegisterObjectCreator("Shader",new ShaderCreator());
   }
}
/////////////////////////////
//Destructor               //
/////////////////////////////
TextureImage::~TextureImage()
{
   _imageFiles.clear();
   _textureType.clear();
}
///////////////////////////////////////////////////
TextureImage::TextureImage(const TextureImage& rhs)
:VE_XML::XMLObject(rhs)
{
   _imageFiles = rhs._imageFiles;
   _textureType = rhs._textureType;
   _textureUnit = rhs._textureUnit;
   _dimension = rhs._dimension;
}
///////////////////////////////////////////////////////
void TextureImage::SetDimension(unsigned int dimension)
{
   _dimension = dimension;
}
///////////////////////////////////////////////////////////////////////////
void TextureImage::SetImageFile(std::string face,std::string imageFileName)
{
   if(face != "FRONT"||
      face != "Positive X"||
      face != "Negative X"||
      face != "Positive Y"||
      face != "Negative Y"||
      face != "Positive Z"||
      face != "Negative Z") 
   {
      _imageFiles[face] = imageFileName;
   }
   else
   {
      std::cout<<"Invalid Face: "<<face<<std::endl;
      std::cout<<"TextureImage::SetImageFile(): "<<face<<std::endl;
   } 
}
////////////////////////////////////////////////////
void TextureImage::SetTextureUnit(unsigned int tUnit)
{
   _textureUnit = tUnit;
}
///////////////////////////////////////////
unsigned int TextureImage::GetTextureUnit()
{
   return _textureUnit;
}
/////////////////////////////////////////
unsigned int TextureImage::GetDimension()
{
   return _dimension;
}
/////////////////////////////////////////////////////////
std::string TextureImage::GetImageFile(std::string face)
{
   try 
   {
      return _imageFiles[face];
   } 
   catch(...)
   {
      std::cout<<"Invalid Face: "<<face<<std::endl;
      std::cout<<"TextureImage::GetImageFile(): "<<face<<std::endl;
   }
}
//////////////////////////////////////////////////////
void TextureImage::_updateVEElement(std::string input)
{
   if(!_veElement)
   {
      _veElement = _rootDocument->createElement(xercesString(input));
   }
   _updateImageFileName();
   _updateDataDimension();
   _updateTextureUnit();
}
/////////////////////////////////////////
void TextureImage::_updateDataDimension()
{
   DOMElement* dimensionTag  = _rootDocument->createElement(xercesString("dimension"));
   _veElement->appendChild(dimensionTag);      
   DOMText* dimension = _rootDocument->createTextNode(xercesString(static_cast<int>(_dimension)));
   dimensionTag->appendChild( dimension );
}	
///////////////////////////////////////
void TextureImage::_updateTextureUnit()
{
   DOMElement* tUnitTag  = _rootDocument->createElement(xercesString("textureUnit"));
   _veElement->appendChild(tUnitTag);      
   DOMText* tUnit = _rootDocument->createTextNode(xercesString( static_cast<int>(_textureUnit)));
   tUnitTag->appendChild( tUnit );
}	
//////////////////////////////////////
void TextureImage::_updateImageFileName()
{
   /*This needs to be re-implemented!!
   DOMElement* imageNameElement = _rootDocument->createElement(xercesString("imageFile"));
   DOMText* imageName = _rootDocument->createTextNode(xercesString(_imageFile.c_str()));
   imageNameElement->appendChild(imageName);
   _veElement->appendChild(imageNameElement);
   */
}
///////////////////////////////////////////
void TextureImage::_updateTextureDataType()
{
   DOMElement* dataTypeElement = _rootDocument->createElement(xercesString("textureType"));
   DOMText* dataType = _rootDocument->createTextNode(xercesString(_textureType.c_str()));
   dataTypeElement->appendChild(dataType);
   _veElement->appendChild(dataTypeElement);
}
//////////////////////////////////////////////////////////
void TextureImage::SetObjectFromXMLData(DOMNode* xmlInput)
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
	 //get the image file  name
         {
            DOMElement* nChildrenElement = GetSubElement(currentElement,std::string("imageFile"),0);
            if(nChildrenElement)
            {
               /*This needs to be re implemented
               _imageFile = ExtractDataStringFromSimpleElement(nChildrenElement);
               */
            }
         }
	 //get the texture data type 
         {
            DOMElement* dataType = GetSubElement(currentElement,std::string("textureType"),0);
            if(dataType)
            {
               _textureType = ExtractDataStringFromSimpleElement(dataType);
            }
         }
	 //get the texture unit 
         {
            DOMElement* tUnitElement = GetSubElement(currentElement,std::string("textureUnit"),0);
            if(tUnitElement)
            {
               _textureUnit = static_cast<int>(ExtractDataNumberFromSimpleElement(tUnitElement));
            }
         }
	 //get the data dimension 
         {
            DOMElement* dimensionElement = GetSubElement(currentElement,std::string("dimension"),0);
            if(dimensionElement)
            {
               _dimension = static_cast<int>(ExtractDataNumberFromSimpleElement(dimensionElement));
            }
         }
      }
   }
}
//////////////////////////////////////////////////////////////
TextureImage& TextureImage::operator=(const TextureImage& rhs)
{
   if(this != &rhs)
   {
      XMLObject::operator =(rhs);
      _imageFiles = rhs._imageFiles;
      _textureType = rhs._textureType;
      _textureUnit = rhs._textureUnit;
      _dimension = rhs._dimension;
   }
   return *this;
}
