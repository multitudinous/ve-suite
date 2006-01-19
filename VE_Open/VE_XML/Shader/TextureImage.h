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
#ifndef VE_TEXTURE_IMAGE_H
#define VE_TEXTURE_IMAGE_H

#include "VE_Open/VE_XML/VEXMLObject.h"
#include "VE_Installer/include/VEConfig.h"
#include <xercesc/dom/DOM.hpp>
#include <string>

/*!\file TextureImage.h
  Texture Image 
  */
/*!\class VE_Shader::TextureImage
 * Class that stores an image in texture data.
 */

namespace VE_Shader{
class TextureImage:public VE_XML::VEXMLObject{
public:
   ///Constructor
   ///\param rootDocument The xerces document for this node.
   TextureImage(XERCES_CPP_NAMESPACE_QUALIFIER DOMDocument* rootDocument);
   ///Destructor
   virtual ~TextureImage();
   ///Copy constructor
   TextureImage(const TextureImage& rhs);

   ///Set the dimensions of the data stored in here.
   void SetDimension(unsigned int dimension);

   ///Set the image file representing the texture data.
   void SetImageFile(std::string imageFileName);

   ///Set the texture unit of this data.
   ///\todo May not be necessary.
   void SetTextureUnit(unsigned int Unit);

   ///Get the texture unit
   ///\todo May not be necessary.
   unsigned int GetTextureUnit();

   ///Get the dimension of the texture data.
   unsigned int GetDimension();

   ///Get the name and location of image file that this data represents.
   std::string GetImageFile();
   
   ///Set the object from input XML data
   ///\param xmlInput The input xml data.
   void SetObjectFromXMLData(DOMNode* xmlInput);
protected:
   ///Internally update the XML data for this element.
   ///\param input The XML element information
   virtual void _updateVEElement(std::string input);

   ///Internally update the texture unit.
   void _updateTextureUnit();
   ///Internally update the image file.
   void _updateImageFileName();
   ///Internally update the data dimensions.
   void _updateDataDimension();

   std::string _imageFile;///<The image file to create texture data from.
   unsigned int _textureUnit;///<This may not be needed but will store a texture unit for use in GL apps.
   unsigned int _dimension;///<The dimension of this texture data.
};
}
#endif//VE_TEXTURE_IMAGE_H
