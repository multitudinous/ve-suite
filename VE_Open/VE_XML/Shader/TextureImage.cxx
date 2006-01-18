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
#include "VE_Open/VE_XML/Shader/TextureImage.h"

using namespace VE_Shader;
////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                     //
////////////////////////////////////////////////////////////////////////////////////
TextureImage::TextureImage(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument)
{
   _imageFile = std::string("");
   _textureUnit = 0;
   _dimension = 2;
}
/////////////////////////////
//Destructor               //
/////////////////////////////
TextureImage::~TextureImage()
{
}
///////////////////////////////////////////////////
TextureImage::TextureImage(const TextureImage& rhs)
{
   _imageFile = rhs._imageFile;
   _textureUnit = rhs._textureUnit;
}
///////////////////////////////////////////////////////
void TextureImage::SetDimension(unsigned int dimension)
{
   _dimension = dimension;
}
//////////////////////////////////////////////////////////
void TextureImage::SetImageFile(std::string imageFileName)
{
   _imageFile = imageFileName;
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
////////////////////////////////////////
std::string TextureImage::GetImageFile()
{
   return _imageFile;
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
   DOMText* dimension = _rootDocument->createTextNode(xercesString( _dimension));
   dimensionTag->appendChild( dimension );
}	
///////////////////////////////////////
void TextureImage::_updateTextureUnit()
{
   DOMElement* tUnitTag  = _rootDocument->createElement(xercesString("textureUnit"));
   _veElement->appendChild(tUnitTag);      
   DOMText* tUnit = _rootDocument->createTextNode(xercesString( _textureUnit));
   tUnitTag->appendChild( tUnit );
}	
//////////////////////////////////////
void TextureImage::_updateImageFileName()
{
   DOMElement* imageNameElement = _rootDocument->createElement(xercesString("imageFile"));
   DOMText* imageName = _rootDocument->createTextNode(xercesString(_imageFile.c_str()));
   imageNameElement->appendChild(imageName);
   _veElement->appendChild(imageNameElement);
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
            _numChildren = static_cast<int>(ExtractDataStringFromSimpleElement(nChildrenElement));
	 } 
	 //get the texture unit 
         {
            DOMElement* tUnitElement = GetSubElement(currentElement,std::string("textureUnit"),0);
            _textureUnit = static_cast<int>(ExtractDataNumberFromSimpleElement(tUnitElement));
	 } 
	 //get the data dimension 
         {
            DOMElement* dimensionElement = GetSubElement(currentElement,std::string("dimension"),0);
            _dimension = static_cast<int>(ExtractDataNumberFromSimpleElement(dimensionElement));
	 } 
      }
   }
}
