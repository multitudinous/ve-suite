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
#include <ves/open/xml/shader/TextureImage.h>

#include <ves/open/xml/shader/ShaderCreator.h>
#include <ves/open/xml/XMLObjectFactory.h>
#include <ves/open/xml/Command.h>
#include <ves/open/xml/DataValuePair.h>
#include <ves/open/xml/DataValuePairPtr.h>

XERCES_CPP_NAMESPACE_USE
using namespace ves::open::xml::shader;
using namespace ves::open::xml;
/////////////////////////////////////////////////////////////////////////////////
//Constructor
/////////////////////////////////////////////////////////////////////////////////
TextureImage::TextureImage()
        : ves::open::xml::XMLObject()
{
    mTextureDescription = new ves::open::xml::Command();
    //std::cout<<"New texture image"<<std::endl;
    SetObjectType( "TextureImage" );
    SetObjectNamespace( "Shader" );
    mTextureDescription->SetCommandName( "Texture Image Data" );

    DataValuePairWeakPtr storedDimension = new ves::open::xml::DataValuePair();
    storedDimension->SetDataType( "UNSIGNED INT" );
    storedDimension->SetDataName( "Dimension" );
    storedDimension->SetDataValue( static_cast<unsigned int>( 2 ) );
    mTextureDescription->AddDataValuePair( storedDimension );

    DataValuePairWeakPtr storedUnit = new ves::open::xml::DataValuePair();
    storedUnit->SetDataType( "UNSIGNED INT" );
    storedUnit->SetDataName( "Unit" );
    storedUnit->SetDataValue( static_cast<unsigned int>( 0 ) );
    mTextureDescription->AddDataValuePair( storedUnit );

    DataValuePairWeakPtr typeData = new ves::open::xml::DataValuePair();
    typeData->SetDataType( "STRING" );
    typeData->SetDataName( "Type" );
    typeData->SetDataString( "2D" );
    mTextureDescription->AddDataValuePair( typeData );

    DataValuePairWeakPtr minification = new ves::open::xml::DataValuePair();
    minification->SetDataType( "STRING" );
    minification->SetDataName( "Minification" );
    minification->SetDataString( "Linear" );
    mTextureDescription->AddDataValuePair( minification );

    DataValuePairWeakPtr magnification = new ves::open::xml::DataValuePair();
    magnification ->SetDataType( "STRING" );
    magnification ->SetDataName( "Magnification" );
    magnification ->SetDataString( "Linear" );
    mTextureDescription->AddDataValuePair( magnification );

    DataValuePairWeakPtr wrapS = new ves::open::xml::DataValuePair();
    wrapS->SetDataType( "STRING" );
    wrapS->SetDataName( "Wrap S" );
    wrapS->SetDataString( "Clamp" );
    mTextureDescription->AddDataValuePair( wrapS );

    DataValuePairWeakPtr wrapT = new ves::open::xml::DataValuePair();
    wrapT->SetDataType( "STRING" );
    wrapT->SetDataName( "Wrap T" );
    wrapT->SetDataString( "Clamp" );
    mTextureDescription->AddDataValuePair( wrapT );

    DataValuePairWeakPtr wrapR = new ves::open::xml::DataValuePair();
    wrapR->SetDataType( "STRING" );
    wrapR->SetDataName( "Wrap R" );
    wrapR->SetDataString( "Clamp" );
    mTextureDescription->AddDataValuePair( wrapR );
}
/////////////////////////////
//Destructor               //
/////////////////////////////
TextureImage::~TextureImage()
{
    ;
}
///////////////////////////////////////////////////
TextureImage::TextureImage( const TextureImage& rhs )
        : ves::open::xml::XMLObject( rhs )
{
    mTextureDescription = new ves::open::xml::Command(( *rhs.mTextureDescription ) );
}
////////////////////////////////////////////////////////////////////////////
void TextureImage::SetWrapMode( const std::string& direction, const std::string& wrapMode )
{
    if( direction == "Wrap S" ||
            direction == "Wrap T" ||
            direction == "Wrap R" )
    {
        ves::open::xml::DataValuePairWeakPtr wrapModeData = mTextureDescription->GetDataValuePair( direction );
        if( wrapModeData )
        {
            wrapModeData->SetDataName( direction );
            wrapModeData->SetDataString( wrapMode );
        }
    }
}
///////////////////////////////////////////////////////////////////////////
void TextureImage::SetFilterMode( const std::string& minMagFilter, const std::string& mode )
{
    if( minMagFilter == "Minification" ||
            minMagFilter == "Magnification" )
    {
        ves::open::xml::DataValuePairWeakPtr filterModeData = mTextureDescription->GetDataValuePair( minMagFilter );
        if( filterModeData )
        {
            filterModeData->SetDataName( minMagFilter );
            filterModeData->SetDataString( mode );
        }
    }
}
///////////////////////////////////////////////////////////////////////////
bool TextureImage::GetWrapMode( const std::string& direction, std::string& wrapMode )
{
    ves::open::xml::DataValuePairWeakPtr wrapModeData = mTextureDescription->GetDataValuePair( direction );
    if( wrapModeData )
    {
        wrapMode = wrapModeData->GetDataString();
        return true;
    }
    std::cout << "===Wrap mode not found: " << direction << " ===" << std::endl;
    return false;
}
/////////////////////////////////////////////
bool TextureImage::GetType( std::string& type )
{
    ves::open::xml::DataValuePairWeakPtr textureType = mTextureDescription->GetDataValuePair( "Type" );
    if( textureType )
    {
        type = textureType->GetDataString();
        return true;
    }
    return false;
}
///////////////////////////////////////////////////////////////////////////
bool TextureImage::GetFilterMode( const std::string& minMagFilter, std::string& mode )
{
    ves::open::xml::DataValuePairWeakPtr filterModeData = mTextureDescription->GetDataValuePair( minMagFilter );
    if( filterModeData )
    {
        mode = filterModeData->GetDataString();
        return true;
    }
    return false;
}
///////////////////////////////////////////////////////
void TextureImage::SetDimension( unsigned int dimension )
{
    ves::open::xml::DataValuePairWeakPtr storedDimension = mTextureDescription->GetDataValuePair( "Dimension" );
    if( storedDimension )
    {
        storedDimension->SetDataName( "Dimension" );
        storedDimension->SetDataValue( dimension );
    }
}
///////////////////////////////////////////////////////////////////////////
void TextureImage::SetImageFile( const std::string& face, const std::string& imageFileName )
{
    if( face == "FRONT" ||
            face == "Positive X" ||
            face == "Negative X" ||
            face == "Positive Y" ||
            face == "Negative Y" ||
            face == "Positive Z" ||
            face == "Negative Z" )
    {
        ves::open::xml::DataValuePairWeakPtr faceImageData = mTextureDescription->GetDataValuePair( face );
        if( !faceImageData )
        {
            faceImageData = new ves::open::xml::DataValuePair();
            faceImageData->SetData( face, imageFileName );
            mTextureDescription->AddDataValuePair( faceImageData );
        }
        else
        {
            faceImageData->SetData( face, imageFileName );
        }
    }
    else
    {
        std::cout << "Invalid Face: " << face << std::endl;
        std::cout << "TextureImage::SetImageFile(): " << face << std::endl;
    }
}
////////////////////////////////////////////////////
void TextureImage::SetTextureUnit( unsigned int tUnit )
{
    ves::open::xml::DataValuePairWeakPtr storedUnit = mTextureDescription->GetDataValuePair( "Unit" );
    if( storedUnit )
    {
        storedUnit->SetDataValue( tUnit );
    }
}
///////////////////////////////////////////
unsigned int TextureImage::GetTextureUnit()
{
    ves::open::xml::DataValuePairWeakPtr storedUnit = mTextureDescription->GetDataValuePair( "Unit" );
    if( storedUnit )
    {
        return storedUnit->GetUIntData();
    }
    return 0;
}
/////////////////////////////////////////
unsigned int TextureImage::GetDimension()
{
    ves::open::xml::DataValuePairWeakPtr storedDimension = mTextureDescription->GetDataValuePair( "Dimension" );
    if( storedDimension )
    {
        return storedDimension->GetUIntData();
    }
    return 0;
}
/////////////////////////////////////////////////////////
const std::string TextureImage::GetImageFile( const std::string& face )
{
    try
    {
        ves::open::xml::DataValuePairWeakPtr faceImageData = mTextureDescription->GetDataValuePair( face );
        if( !faceImageData )
        {
            throw "Invalid Texture Face";
        }
        return faceImageData->GetDataString();

    }
    catch ( char* msg )
    {
        std::cout << "TextureImage::GetImageFile() Error: " << msg << ": " << face << std::endl;
    }
    catch ( ... )
    {
        std::cout << "No image available: TextureImage::GetImageFile()" << std::endl;
    }
    return std::string( "" );
}
////////////////////////////////////////////////////////////////
void TextureImage::SetTextureImageType( const std::string& textureType )
{
    if( textureType == "1D" ||
            "2D" ||
            "3D" ||
            "Cube" ||
            "Environment" ||
            "Perlin Noise" )
    {
        ves::open::xml::DataValuePairWeakPtr typeData = mTextureDescription->GetDataValuePair( textureType );
        if( !typeData )
        {
            typeData = new ves::open::xml::DataValuePair();
            mTextureDescription->AddDataValuePair( typeData );
        }
        typeData->SetData( "Type", textureType );
        if( textureType == "1D" )
            SetDimension( 1 );
        else if( textureType == "2D" )
            SetDimension( 2 );
        else if( textureType == "3D" ||
                  textureType == "Cube" ||
                  textureType == "Environment" ||
                  textureType == "Perlin Noise" )
        {
            SetDimension( 3 );
        }
    }
    else
    {
        std::cout << "Invalid TextureType: " << textureType << std::endl;
        std::cout << "TextureImage::SetTextureImageType(): " << std::endl;
    }
}
//////////////////////////////////////////////////////
void TextureImage::_updateVEElement( const std::string& input )
{
    SetSubElement( "textureDescriptionData", &( *mTextureDescription ) );
}
//////////////////////////////////////////////////////////
void TextureImage::SetObjectFromXMLData( DOMNode* xmlInput )
{
    DOMElement* currentElement = 0;
    if( xmlInput->getNodeType() == DOMNode::ELEMENT_NODE )
    {
        currentElement = dynamic_cast<DOMElement*>( xmlInput );
    }

    if( currentElement )
    {
        //std::cout<<"Getting textureDescriptionData"<<std::endl;
        DOMElement* descriptionData = GetSubElement( currentElement, std::string( "textureDescriptionData" ), 0 );
        if( descriptionData )
        {
            //std::cout<<"Found textureDescriptionData"<<std::endl;
            mTextureDescription->SetObjectFromXMLData( descriptionData );
        }
    }
}
//////////////////////////////////////////////////////////////
TextureImage& TextureImage::operator=( const TextureImage& rhs )
{
    if( this != &rhs )
    {
        XMLObject::operator =( rhs );
        mTextureDescription = new ves::open::xml::Command(( *rhs.mTextureDescription ) );
    }
    return *this;
}
