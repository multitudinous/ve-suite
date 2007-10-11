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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "ves/open/xml/shader/TextureImage.h"
#include "ves/open/xml/shader/ShaderCreator.h"
#include "ves/open/xml/XMLObjectFactory.h"
#include "ves/open/xml/Command.h"
#include "ves/open/xml/DataValuePair.h"
#include "ves/open/xml/DataValuePairPtr.h"
XERCES_CPP_NAMESPACE_USE
using namespace VE_XML::VE_Shader;
using namespace VE_XML;
////////////////////////////////////////////////////////////////////////////////////
//Constructor                                                                     //
////////////////////////////////////////////////////////////////////////////////////
TextureImage::TextureImage()
:VE_XML::XMLObject()
{
    _textureDescription = new VE_XML::Command();
    //std::cout<<"New texture image"<<std::endl;
    SetObjectType("TextureImage");
    SetObjectNamespace("Shader");
    _textureDescription->SetCommandName("Texture Image Data");

    DataValuePairWeakPtr storedDimension = new VE_XML::DataValuePair();
    storedDimension->SetDataType("UNSIGNED INT");
    storedDimension->SetDataName("Dimension");
    storedDimension->SetDataValue(static_cast<unsigned int>(2));
    _textureDescription->AddDataValuePair(storedDimension);

    DataValuePairWeakPtr storedUnit = new VE_XML::DataValuePair(); 
    storedUnit->SetDataType("UNSIGNED INT");
    storedUnit->SetDataName("Unit");
    storedUnit->SetDataValue(static_cast<unsigned int>(0));
    _textureDescription->AddDataValuePair(storedUnit);

    DataValuePairWeakPtr typeData = new VE_XML::DataValuePair();
    typeData->SetDataType("STRING");
    typeData->SetDataName("Type");
    typeData->SetDataString("2D");
    _textureDescription->AddDataValuePair(typeData);

    DataValuePairWeakPtr minification = new VE_XML::DataValuePair();
    minification->SetDataType("STRING");
    minification->SetDataName("Minification");
    minification->SetDataString("Linear");
    _textureDescription->AddDataValuePair(minification);

    DataValuePairWeakPtr magnification = new VE_XML::DataValuePair();
    magnification ->SetDataType("STRING");
    magnification ->SetDataName("Magnification");
    magnification ->SetDataString("Linear");
    _textureDescription->AddDataValuePair(magnification );

    DataValuePairWeakPtr wrapS = new VE_XML::DataValuePair();
    wrapS->SetDataType("STRING");
    wrapS->SetDataName("Wrap S");
    wrapS->SetDataString("Clamp");
    _textureDescription->AddDataValuePair(wrapS);

    DataValuePairWeakPtr wrapT = new VE_XML::DataValuePair();
    wrapT->SetDataType("STRING");
    wrapT->SetDataName("Wrap T");
    wrapT->SetDataString("Clamp");
    _textureDescription->AddDataValuePair(wrapT);

    DataValuePairWeakPtr wrapR = new VE_XML::DataValuePair();
    wrapR->SetDataType("STRING");
    wrapR->SetDataName("Wrap R");
    wrapR->SetDataString("Clamp");
    _textureDescription->AddDataValuePair(wrapR);
}
/////////////////////////////
//Destructor               //
/////////////////////////////
TextureImage::~TextureImage()
{
   ;
}
///////////////////////////////////////////////////
TextureImage::TextureImage(const TextureImage& rhs)
:VE_XML::XMLObject(rhs)
{
   _textureDescription = new VE_XML::Command( (*rhs._textureDescription) );
}
////////////////////////////////////////////////////////////////////////////
void TextureImage::SetWrapMode(std::string direction, std::string wrapMode)
{
   if(direction == "Wrap S" ||
      direction == "Wrap T" ||
      direction == "Wrap R")
   {
      VE_XML::DataValuePairWeakPtr wrapModeData = _textureDescription->GetDataValuePair(direction);
      if(wrapModeData)
      {
         wrapModeData->SetDataName(direction);
         wrapModeData->SetDataString(wrapMode);
      }
   }
}
///////////////////////////////////////////////////////////////////////////
void TextureImage::SetFilterMode(std::string minMagFilter,std::string mode)
{
   if(minMagFilter == "Minification" ||
      minMagFilter == "Magnification")
   {
      VE_XML::DataValuePairWeakPtr filterModeData = _textureDescription->GetDataValuePair(minMagFilter);
      if(filterModeData)
      {
         filterModeData->SetDataName(minMagFilter);
         filterModeData->SetDataString(mode);
      }
   }
}
///////////////////////////////////////////////////////////////////////////
bool TextureImage::GetWrapMode(std::string direction,std::string& wrapMode)
{
   VE_XML::DataValuePairWeakPtr wrapModeData = _textureDescription->GetDataValuePair(direction);
   if(wrapModeData)
   {
      wrapMode = wrapModeData->GetDataString();
      return true;
   }
   std::cout<<"===Wrap mode not found: "<<direction<<" ==="<<std::endl;
   return false;
}
/////////////////////////////////////////////
bool TextureImage::GetType(std::string& type)
{
   VE_XML::DataValuePairWeakPtr textureType = _textureDescription->GetDataValuePair("Type");
   if(textureType)
   {
      type = textureType->GetDataString();
      return true;
   }
   return false;
}
///////////////////////////////////////////////////////////////////////////
bool TextureImage::GetFilterMode(std::string minMagFilter,std::string& mode)
{
   VE_XML::DataValuePairWeakPtr filterModeData = _textureDescription->GetDataValuePair(minMagFilter);
   if(filterModeData)
   {
      mode = filterModeData->GetDataString();
      return true;
   }
   return false;
}
///////////////////////////////////////////////////////
void TextureImage::SetDimension(unsigned int dimension)
{
   VE_XML::DataValuePairWeakPtr storedDimension = _textureDescription->GetDataValuePair("Dimension");
   if(storedDimension)
   {
      storedDimension->SetDataName("Dimension");
      storedDimension->SetDataValue(dimension);
   }
}
///////////////////////////////////////////////////////////////////////////
void TextureImage::SetImageFile(std::string face,std::string imageFileName)
{
   if(face == "FRONT"||
      face == "Positive X"||
      face == "Negative X"||
      face == "Positive Y"||
      face == "Negative Y"||
      face == "Positive Z"||
      face == "Negative Z") 
   {
      VE_XML::DataValuePairWeakPtr faceImageData = _textureDescription->GetDataValuePair(face);
      if(!faceImageData)
      {
         faceImageData = new VE_XML::DataValuePair();
         faceImageData->SetData(face,imageFileName);
         _textureDescription->AddDataValuePair(faceImageData);
      }
      else
      {
         faceImageData->SetData(face,imageFileName);
      }
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
    VE_XML::DataValuePairWeakPtr storedUnit = _textureDescription->GetDataValuePair("Unit");
   if(storedUnit)
   {
      storedUnit->SetDataValue(tUnit);
   }
}
///////////////////////////////////////////
unsigned int TextureImage::GetTextureUnit()
{
   VE_XML::DataValuePairWeakPtr storedUnit = _textureDescription->GetDataValuePair("Unit");
   if(storedUnit)
   {
      return storedUnit->GetUIntData();
   }
   return 0;
}
/////////////////////////////////////////
unsigned int TextureImage::GetDimension()
{
   VE_XML::DataValuePairWeakPtr storedDimension = _textureDescription->GetDataValuePair("Dimension");
   if(storedDimension)
   {
      return storedDimension->GetUIntData();
   }
   return 0;
}
/////////////////////////////////////////////////////////
std::string TextureImage::GetImageFile(std::string face)
{
   try 
   {
      VE_XML::DataValuePairWeakPtr faceImageData = _textureDescription->GetDataValuePair(face);
      if(!faceImageData)
      {
         throw "Invalid Texture Face";
      }
      return faceImageData->GetDataString();

   } 
   catch(char* msg)
   {
      std::cout<<"TextureImage::GetImageFile() Error: "<<msg<<": "<<face<<std::endl;
   }
   catch(...)
   {
      std::cout<<"No image available: TextureImage::GetImageFile()"<<std::endl;
   }
   return std::string("");
}
////////////////////////////////////////////////////////////////
void TextureImage::SetTextureImageType(std::string textureType)
{
   if(textureType == "1D" ||
                   "2D" ||
                   "3D" ||
                   "Cube" ||
                   "Environment" ||
				   "Perlin Noise")
   {
      VE_XML::DataValuePairWeakPtr typeData = _textureDescription->GetDataValuePair(textureType);
      if(!typeData)
      {
         typeData = new VE_XML::DataValuePair();
         _textureDescription->AddDataValuePair(typeData);
      }
      typeData->SetData("Type",textureType);
      if(textureType == "1D")
         SetDimension(1);
      else if(textureType == "2D")
         SetDimension(2);
      else if(textureType == "3D" ||
             textureType == "Cube" ||
             textureType == "Environment" ||
			 textureType == "Perlin Noise" )
      {
         SetDimension(3);
      }
   }
   else
   {
      std::cout<<"Invalid TextureType: "<<textureType<<std::endl;
      std::cout<<"TextureImage::SetTextureImageType(): "<<std::endl;
   }
}
//////////////////////////////////////////////////////
void TextureImage::_updateVEElement(std::string input)
{
   SetSubElement("textureDescriptionData", &(*_textureDescription) );
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
     //std::cout<<"Getting textureDescriptionData"<<std::endl;
     DOMElement* descriptionData = GetSubElement(currentElement,std::string("textureDescriptionData"),0);
     if(descriptionData)
     {
        //std::cout<<"Found textureDescriptionData"<<std::endl;
        _textureDescription->SetObjectFromXMLData(descriptionData);
     }
   }
}
//////////////////////////////////////////////////////////////
TextureImage& TextureImage::operator=(const TextureImage& rhs)
{
   if(this != &rhs)
   {
      XMLObject::operator =(rhs);
      _textureDescription = new VE_XML::Command( (*rhs._textureDescription) );
   }
   return *this;
}
