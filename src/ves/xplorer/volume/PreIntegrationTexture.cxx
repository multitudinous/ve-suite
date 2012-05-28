/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2012 by Iowa State University
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

// --- VE-Suite Includes --- //
#include <ves/xplorer/volume/PreIntegrationTexture.h>
#include <ves/xplorer/volume/TransferFunction.h>

// --- OSG Includes --- //
#include <osg/Texture2D>

// --- C/C++ Includes --- //
#include <iostream>
#include <fstream>

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
unsigned char clamp( unsigned char lower, unsigned char upper, unsigned char value )
{
    return ( value < lower ) ? lower : ( value > upper ) ? upper : value;
}
////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D::PreIntegrationTexture2D()
{
    _tf = 0;
    _sliceIntegrationValues = 0;
    _rawData = 0;
    _preIntegratedTexture = new osg::Texture2D();
    _preIntegratedTexture->setDataVariance( osg::Object::DYNAMIC );
    _preIntegratedTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
    _preIntegratedTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
    _preIntegratedTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
    _preIntegratedTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
    _preIntegratedTexture->setInternalFormat( GL_RGBA );
    _imageData = new osg::Image();
}
////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D::PreIntegrationTexture2D( const PreIntegrationTexture2D& rhs )
{
    _tf = rhs._tf;
    _sliceIntegrationValues = new float[_tf->GetResolution( 0 ) * 4];
    for( unsigned int i = 0; i < _tf->GetResolution( 0 ); ++i )
    {
        _sliceIntegrationValues[i * 4] = rhs._sliceIntegrationValues[i * 4];
        _sliceIntegrationValues[i * 4 + 1] = rhs._sliceIntegrationValues[i * 4 + 1];
        _sliceIntegrationValues[i * 4 + 2] = rhs._sliceIntegrationValues[i * 4 + 2];
        _sliceIntegrationValues[i * 4 + 3] = rhs._sliceIntegrationValues[i * 4 + 3];
    }
    _rawData = new unsigned char[_tf->GetResolution( 0 )*_tf->GetResolution( 0 ) * 4];
    for( unsigned int i = 0; i < _tf->GetResolution( 0 )*_tf->GetResolution( 0 ); ++i )
    {
        _rawData[i * 4] = rhs._rawData[i * 4];
        _rawData[i * 4 + 1] = rhs._rawData[i * 4 + 1];
        _rawData[i * 4 + 2] = rhs._rawData[i * 4 + 2];
        _rawData[i * 4 + 3] = rhs._rawData[i * 4 + 3];
    }
    _preIntegratedTexture = new osg::Texture2D( *rhs._preIntegratedTexture.get() );
    _imageData = new osg::Image( *rhs._imageData.get() );
}
////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D& PreIntegrationTexture2D::operator=( const PreIntegrationTexture2D& rhs )
{
    if( this != &rhs )
    {
        _tf = rhs._tf;
        if( _sliceIntegrationValues )
        {
            delete [] _sliceIntegrationValues;
            _sliceIntegrationValues = 0;
        }
        _sliceIntegrationValues = new float[_tf->GetResolution( 0 ) * 4];
        for( unsigned int i = 0; i < _tf->GetResolution( 0 ); ++i )
        {
            _sliceIntegrationValues[i * 4] = rhs._sliceIntegrationValues[i * 4];
            _sliceIntegrationValues[i * 4 + 1] = rhs._sliceIntegrationValues[i * 4 + 1];
            _sliceIntegrationValues[i * 4 + 2] = rhs._sliceIntegrationValues[i * 4 + 2];
            _sliceIntegrationValues[i * 4 + 3] = rhs._sliceIntegrationValues[i * 4 + 3];
        }
        if( _rawData )
        {
            delete []_rawData;
            _rawData = 0;
        }
        _rawData = new unsigned char[_tf->GetResolution( 0 )*_tf->GetResolution( 0 ) * 4];
        for( unsigned int i = 0; i < _tf->GetResolution( 0 )*_tf->GetResolution( 0 ); ++i )
        {
            _rawData[i * 4] = rhs._rawData[i * 4];
            _rawData[i * 4 + 1] = rhs._rawData[i * 4 + 1];
            _rawData[i * 4 + 2] = rhs._rawData[i * 4 + 2];
            _rawData[i * 4 + 3] = rhs._rawData[i * 4 + 3];
        }
        _preIntegratedTexture = const_cast< osg::Texture2D* >( rhs._preIntegratedTexture.get() );
        _imageData = const_cast< osg::Image* >( rhs._imageData.get() );
    }
    return *this;
}
////////////////////////////////////////////////////////////////////////////////
PreIntegrationTexture2D::~PreIntegrationTexture2D()
{
    if( _sliceIntegrationValues )
    {
        delete [] _sliceIntegrationValues;
        _sliceIntegrationValues = 0;
    }
}
////////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::SetTransferFunction( TransferFunction* tf )
{
    if( tf->GetDimension() != 1 )
    {
        std::cout << "Error!" << std::endl;
        std::cout << "Only 1D transfer functions are supported for PreIntegration!" << std::endl;
        return;
    }

    _tf = tf;
    ///transfer function resolution shouldn't change but just in case
    if( _sliceIntegrationValues )
    {
        delete []  _sliceIntegrationValues;
        _sliceIntegrationValues = 0;
    }

    _sliceIntegrationValues = new float[_tf->GetResolution( 0 ) * 4];
    _rawData = new unsigned char[_tf->GetResolution( 0 )*_tf->GetResolution( 0 ) * 4];

    _imageData->setImage( _tf->GetResolution( 0 ), _tf->GetResolution( 0 ),
                          1,
                          GL_RGBA,
                          GL_RGBA,
                          GL_UNSIGNED_BYTE,
                          _rawData,
                          osg::Image::USE_NEW_DELETE );
    _preIntegratedTexture->setImage( _imageData.get() );
}
////////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::FullUpdate()
{
    if( !_tf )
    {
        std::cout << "Error!!!" << std::endl;
        std::cout << "Transfer function not set!!" << std::endl;
        std::cout << "PreIntegrationTexture2D::Update()" << std::endl;

    }
    //std::ofstream fout("./diagonal.txt");
    //make sure we have a texture
    if( !_preIntegratedTexture.valid() )
    {
        _preIntegratedTexture = new osg::Texture2D;
        _preIntegratedTexture->setDataVariance( osg::Object::DYNAMIC );
        _preIntegratedTexture->setFilter( osg::Texture2D::MIN_FILTER, osg::Texture2D::LINEAR );
        _preIntegratedTexture->setFilter( osg::Texture2D::MAG_FILTER, osg::Texture2D::LINEAR );
        _preIntegratedTexture->setWrap( osg::Texture2D::WRAP_T, osg::Texture2D::CLAMP_TO_EDGE );
        _preIntegratedTexture->setWrap( osg::Texture2D::WRAP_S, osg::Texture2D::CLAMP_TO_EDGE );
        _preIntegratedTexture->setInternalFormat( GL_RGBA );
    }
    //Initialize the slice integration table
    _initializeSliceIntegrationValues();

    unsigned int dataDims = _tf->GetResolution( 0 );
    //integrate simpsons rule for each sf,sb combination
    int row = -1;
    int col = 0;
    //unsigned char data[4] = {0, 0, 0, 0};
    unsigned int index = 0;
    int sliceMin = 0;
    int sliceMax = 0;
    //unsigned char* rawData = _preIntegratedTexture->getImage()->data();
    float deltaSlice = 0;
    for( unsigned int i = 0; i < dataDims * dataDims; i++ )
    {
        col = i % dataDims;
        row = ( i % dataDims == 0 ) ? row + 1 : row;
        sliceMin = col;
        sliceMax = row;
        if( row < col )
        {
            sliceMin = row;
            sliceMax = col;
        }
        deltaSlice = 1. / ( float )( sliceMax - sliceMin );

        if( sliceMin != sliceMax )
        {
            _rawData[index++] = clamp( 0, 255, _calculateComponent( deltaSlice, 0, sliceMin, sliceMax ) );
            _rawData[index++] = clamp( 0, 255, _calculateComponent( deltaSlice, 1, sliceMin, sliceMax ) );
            _rawData[index++] = clamp( 0, 255, _calculateComponent( deltaSlice, 2, sliceMin, sliceMax ) );
            _rawData[index++] = clamp( 0, 255, _calculateComponent( deltaSlice, 3, sliceMin, sliceMax ) );
        }
        else
        {
            _rawData[index++] = _tf->unsignedByteDataAt( sliceMin * 4 );
            _rawData[index++] = _tf->unsignedByteDataAt( sliceMin * 4 + 1 );
            _rawData[index++] = _tf->unsignedByteDataAt( sliceMin * 4 + 2 );
            _rawData[index++] = _tf->unsignedByteDataAt( sliceMin * 4 +  3 );
        }
        if( col == row )
        {
            /*fout<<"("<<(unsigned int)_rawData[i*4]<<","
                  <<(unsigned int)_rawData[i*4 + 1]<<","
                  <<(unsigned int)_rawData[i*4 + 2]<<","
                  <<(unsigned int)_rawData[i*4 + 3]<<")";
            fout<<std::endl;*/
        }

    }
    _preIntegratedTexture->dirtyTextureObject();
    _preIntegratedTexture->dirtyTextureParameters();
}
////////////////////////////////////////////////////////////////////////////////
osg::Texture2D* PreIntegrationTexture2D::GetPreIntegratedTexture()
{
    if( _preIntegratedTexture.valid() )
    {
        return _preIntegratedTexture.get();
    }
    return 0;
}
////////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::_initializeSliceIntegrationValues()
{
    //std::fstream fout("./interpo.txt",std::ios::out);
    float* curRGBA = 0;//[4]={0,0,0,0};
    float* prevRGBA = 0;//[4]={0,0,0,0};
    float totalRGBA[4] = {0, 0, 0, 0};
    _sliceIntegrationValues[0] = 0;//_tf->EvaluateAt(0)[0];
    _sliceIntegrationValues[1] = 0;//_tf->EvaluateAt(0)[1];
    _sliceIntegrationValues[2] = 0;//_tf->EvaluateAt(0)[2];
    _sliceIntegrationValues[3] = 0;//_tf->EvaluateAt(0)[3];
    for( unsigned int i = 1; i < _tf->GetResolution( 0 ); ++i )
    {
        curRGBA = _tf->EvaluateAt( i * 4 );
        prevRGBA = _tf->EvaluateAt( ( i - 1 ) * 4 );

        totalRGBA[0] = totalRGBA[0] + .5 * ( curRGBA[0] + prevRGBA[0] );
        totalRGBA[1] = totalRGBA[1] + .5 * ( curRGBA[1] + prevRGBA[1] );
        totalRGBA[2] = totalRGBA[2] + .5 * ( curRGBA[2] + prevRGBA[2] );
        totalRGBA[3] = totalRGBA[3] + .5 * ( curRGBA[3] + prevRGBA[3] );
        _sliceIntegrationValues[i * 4] = totalRGBA[0];
        _sliceIntegrationValues[i * 4 + 1] = totalRGBA[1];
        _sliceIntegrationValues[i * 4 + 2] = totalRGBA[2];
        _sliceIntegrationValues[i * 4 + 3] = totalRGBA[3];
        /*fout<<"("<<sliceIntegrationValues[i*4]<<","
                   <<sliceIntegrationValues[i*4 + 1]<<","
                   <<sliceIntegrationValues[i*4 + 2]<<","
                   <<sliceIntegrationValues[i*4 + 3]<<")";
          fout<<std::endl;*/
    }
}
////////////////////////////////////////////////////////////////////////////////
unsigned char PreIntegrationTexture2D::_calculateComponent( float ds, unsigned int component,
        unsigned int sliceMin, unsigned int sliceMax )
{
    float frontData = 0;
    float backData = 0;
    //front slice
    frontData = _sliceIntegrationValues[sliceMin * 4 + component];
    //back slice
    backData = _sliceIntegrationValues[sliceMax * 4 + component];

    ///Klaus. eq. 8
    //alpha
    if( component == 3 )
    {
        return static_cast<unsigned char>( 255.0 * ( 1.0 - exp( -ds * ( backData - frontData ) / 255. ) ) );
    }
    return static_cast<unsigned char>( ds * ( backData - frontData ) );
}
////////////////////////////////////////////////////////////////////////////////
void PreIntegrationTexture2D::FastUpdate()
{
    if( !_tf )
    {
        std::cout << "Invalid transfer function!!" << std::endl;
        std::cout << "PreIntegrationTexture2D::FastUpdate" << std::endl;
        return;
    }

    unsigned int row = 0;
    unsigned col = 0;
    ///our texture size is fixed for now at 256*256;
    ///update the diagonal values
    for( unsigned int i = 0; i < 256; ++i )
    {
        row  = i * ( 256 * 4 );
        col = i * 4;
        _rawData[row + col    ] = _tf->unsignedByteDataAt( i * 4 );
        _rawData[row + col + 1] = _tf->unsignedByteDataAt( i * 4 + 1 );
        _rawData[row + col + 2] = _tf->unsignedByteDataAt( i * 4 + 2 );
        _rawData[row + col + 3] = _tf->unsignedByteDataAt( i * 4 + 3 );
    }
    _preIntegratedTexture->dirtyTextureObject();
    _preIntegratedTexture->dirtyTextureParameters();
}
////////////////////////////////////////////////////////////////////////////////
