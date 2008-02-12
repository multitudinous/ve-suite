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
#ifndef _VES_OPEN_XML_SHADER_TEXTURE_IMAGE_H_
#define _VES_OPEN_XML_SHADER_TEXTURE_IMAGE_H_

#include <ves/open/xml/shader/TextureImagePtr.h>

#include <ves/open/xml/XMLObject.h>
#include <ves/open/xml/CommandPtr.h>

#include <xercesc/dom/DOM.hpp>

#include <string>
#include <map>


namespace ves
{
namespace open
{
namespace xml
{
namespace shader
{
/*!\file TextureImage.h
  Texture Image
  */
/*!\class ves::open::xml::shader::TextureImage
 * Class that stores an image in texture data.
 */
class VE_SHADER_EXPORTS TextureImage: public ves::open::xml::XMLObject
{
public:
    ///Constructor
    TextureImage();
    ///Destructor
    virtual ~TextureImage();
    ///Copy constructor
    TextureImage( const TextureImage& rhs );

    ///Set the type of texture
    ///\param type The type of texture.
    ///Valid types are:
    ///1D == one dimensional texture data
    ///2D == two dimensional texture data
    ///3D == three dimensional texture data
    ///Cube == Cube map texture data
    ///Environment == Environment map texture data
    void SetTextureImageType( const std::string& type );

    ///Set the dimensions of the data stored in here.
    void SetDimension( unsigned int dimension );

    ///Set the image file representing the texture data.
    ///\param face The face this image  applies to.
    ///\param imageFileName The name of the file storing the image data
    ///If type is Cube then the faces apply to which face of the cube:\n
    ///Positive X == positive x face\n
    ///Negative X == negative x face\n
    ///Positive Y == positive y face\n
    ///Negative Y == negative y face\n
    ///Positive Z == positive z face\n
    ///Negative Z == negative z face\n
    void SetImageFile( const std::string& imageFileName, const std::string& face = "FRONT" );

    ///Set the texture unit of this data.
    ///\todo May not be necessary.
    void SetTextureUnit( unsigned int Unit );

    ///Set the GL_WRAP_MODE\n
    ///\param direction S,T,R direction this wrap mode applies to.
    ///WRAP_S\n
    ///WRAP_T\n
    ///WRAP_R\n
    ///\param wrapMode the GL_WRAP_MODE\n
    ///Clamp\n
    ///Clamp to Edge\n
    ///Clamp to Border\n
    ///Repeat\n
    ///Mirror\n
    void SetWrapMode( const std::string& direction, const std::string& wrapMode );

    ///Set the minification or magnification filter mode
    ///\param minMagFilter The filter to set\n
    ///MIN or MAG
    ///\param mode The filter mode\n
    ///Linear\n
    ///Nearest\n
    ///\todo Mipmapping not implemented yet!!
    void SetFilterMode( const std::string& minMagFilter, const std::string& mode );

    ///Get the GL_WRAP_MODE\n
    ///\param direction S,T,R direction this wrap mode applies to.
    ///\param wrapMode The wrap mode
    bool GetWrapMode( const std::string& direction, std::string& wrapMode );

    ///Set the minification or magnification filter mode
    ///\param minMagFilter The filter to set\n
    ///\param mode The filter mode\n
    ///\todo Mipmapping not implemented yet!!
    bool GetFilterMode( const std::string& minMagFilter, std::string& mode );

    ///Get the texture unit
    ///\todo May not be necessary.
    unsigned int GetTextureUnit();

    ///The texture type.
    ///\param type The texture image type.
    bool GetType( std::string& type );

    ///Get the dimension of the texture data.
    unsigned int GetDimension();

    ///Get the name and location of image file that this data represents.
    ///\param face The face.\n The default is the FRONT.\n
    ///If type is Cube then the faces apply to which face of the cube:\n
    ///Positive X == positive x face\n
    ///Negative X == negative x face\n
    ///Positive Y == positive y face\n
    ///Negative Y == negative y face\n
    ///Positive Z == positive z face\n
    ///Negative Z == negative z face\n
    const std::string GetImageFile( const std::string& face = "FRONT" );

    ///Set the object from input XML data
    ///\param xmlInput The input xml data.
    void SetObjectFromXMLData( XERCES_CPP_NAMESPACE_QUALIFIER DOMNode* xmlInput );

    ///equal operator
    TextureImage& operator=( const TextureImage& rhs );
protected:
    ///Internally update the XML data for this element.
    ///\param input The XML element information
    virtual void _updateVEElement( const std::string& input );

    ves::open::xml::CommandPtr mTextureDescription;///<Data package containing the information about the texture map.

};

}
}
}
}
#endif//TEXTURE_IMAGE_H
