/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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
#include <ves/xplorer/volume/cfdTextureManager.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

// --- VTK Includes --- //
//#include <vtkZLibDataCompressor.h>
#include <vtkImageData.h>
//#include <vtkXMLImageDataReader.h>
#include <vtkFloatArray.h>
#include <vtkIntArray.h>
#include <vtkUnsignedIntArray.h>
#include <vtkUnsignedCharArray.h>
//#include <vtkDataArray.h>
#include <vtkPointData.h>

// --- C/C++ Includes --- //
#include <iostream>
#include <sstream>
#include <fstream>
#include <cmath>

using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////////
cfdTextureManager::cfdTextureManager()
{
    _timeToUpdate = false;
    _curField = 0;
    _resolution = 0;
    _range[0] = 0;
    _range[1] = 0;
    _prevTime = 0;
    _transientRange[0] = 1000000;
    _transientRange[1] = -1000000;
    _direction = 1;
    _mode = PLAY;
    _useShaders = false;
    _bbox[0] = 0;
    _bbox[1] = 0;
    _bbox[2] = 0;
    _bbox[3] = 0;
    _bbox[4] = 0;
    _bbox[5] = 0;
    m_isSlave = false;
}
////////////////////////////////////////////////////////////////////////////////
cfdTextureManager::cfdTextureManager( const cfdTextureManager& tm )
{
    if( _dataFields.size() )
    {
        _dataFields.clear();
    }
    if( _types.size() )
    {
        _types.clear();
    }
    if( _ranges.size() )
    {
        _ranges.clear();
    }
    size_t nFields = tm._dataFields.size();

    for( size_t i = 0; i < nFields; i++ )
    {
        _dataFields.push_back( tm._dataFields[i] );
        _types.push_back( tm._types.at( i ) );
        _ranges.push_back( tm._ranges.at( i ) );
    }
    _timeToUpdate = tm._timeToUpdate;
    _mode = tm._mode;
    _curField = tm._curField;
    _prevTime = tm._prevTime;
    _resolution = new int[3];
    _resolution[0] = tm._resolution[0];
    _resolution[1] = tm._resolution[1];
    _resolution[2] = tm._resolution[2];

    _bbox[0] = tm._bbox[0];
    _bbox[1] = tm._bbox[1];
    _bbox[2] = tm._bbox[2];
    _bbox[3] = tm._bbox[3];
    _bbox[4] = tm._bbox[4];
    _bbox[5] = tm._bbox[5];

    _transientRange[0] = tm._transientRange[0];
    _transientRange[1] = tm._transientRange[1];
    _range[0] = tm._range[0];
    _range[1] = tm._range[1];
    _direction = tm._direction;
    _useShaders =  tm._useShaders;
    m_isSlave = tm.m_isSlave;
}
////////////////////////////////////////////////////////////////////////////////
cfdTextureManager::~cfdTextureManager()
{
    if( _dataFields.size() > 0 )
    {
        size_t nFields = _dataFields.size();
        for( size_t i = 0; i < nFields; i++ )
        {
            if( _dataFields.at( i ) )
            {
                delete [] _dataFields.at( i );
                _dataFields.at( i ) = 0;
            }
        }
        _dataFields.clear();
    }

    if( _types.size() )
    {
        _types.clear();
    }

    if( _resolution )
    {
        delete [] _resolution;
        _resolution = 0;
    }

    if( _ranges.size() )
    {
        _ranges.clear();
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::SetPlayMode( std::string mode )
{
    if( mode == "Play" )
    {
        setPlayMode( PLAY );
    }
    /* else if(mode == "Step")
     {
        setPlayMode(STEP);
     }*/
    else if( mode == "Stop" )
    {
        setPlayMode( STOP );
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::SetUseShaders( bool useShaders )
{
    _useShaders = useShaders;
}
////////////////////////////////////////////////////////////////////////////////
bool cfdTextureManager::IsOnSlaveNode()
{
    return m_isSlave;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::SetCurrentFrame( unsigned int frame,
                                         bool isSlave )
{
    _curField = frame;
    m_isSlave = isSlave;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdTextureManager::GetCurrentFrame()
{
    return _curField;
}
////////////////////////////////////////////////////////////////////////////////
unsigned int cfdTextureManager::getNextFrame()
{
    size_t numFields = _dataFields.size();
    //now update the current field
    if( _direction == 1 )
    {
        //haven't reached the end yet
        //so return the next field
        if( static_cast<size_t>( _curField ) < numFields - 1 )
        {
            _curField += _direction;
        }
        else if( _curField == numFields - 1 )
        {
            //we're at the end so we need to
            //return the beginning
            _curField = 0;
        }
    }
    else
    {
        if( _curField > 0 )
        {
            _curField += _direction;
        }
        else if( _curField == 0 )
        {
            //we're at the end so we need to
            //return the beginning
            _curField = static_cast< int >( numFields - 1 );
        }
    }
    return _curField;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::addFieldTextureFromFile( std::string textureFile )
{
    double tempMag[6] = {0, 0, 0, 0, 0, 0};
    std::ifstream fin( textureFile.c_str() );

    if( fin.is_open() )
    {
        fin.close();

        vtkImageData* flowImage = dynamic_cast< vtkImageData* >( ves::xplorer::util::readVtkThing( textureFile, 0 ) );
        if( flowImage->GetPointData()->GetNumberOfArrays() > 1 )
        {
            std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile : There are too many scalars in this texture " << std::endl;
        }
        else if( flowImage->GetPointData()->GetNumberOfArrays() == 0 )
        {
            std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile : No scalars in this texture " << std::endl;
        }

        vtkDataArray* flowData = flowImage->GetPointData()->GetArray( 0 );
        if( !flowData )
        {
            std::cout << "no flowdata!!" << std::endl;
        }
        dataName = flowData->GetName();

        DataType curType;
        //read the file type
        if( flowData->GetNumberOfComponents() == 1 )
        {
            //scalar
            _types.push_back( SCALAR );
            curType = SCALAR;
        }
        else if( flowData->GetNumberOfComponents() == 4 )
        {
            //vector file
            _types.push_back( VECTOR );
            curType = VECTOR;
        }
        else
        {
            std::cerr << " ERROR : cfdTextureManager::addFieldTextureFromFile :" <<
            " There are too many components in this texture " << std::endl;
        }

        //data range(magnitude) for scalars
        //ignore this value for vectors
        ScalarRange newRange;
        double* range = flowData->GetRange();
        newRange.range[0] = range[ 0 ];
        newRange.range[1] = range[ 1 ];

        _ranges.push_back( newRange );
        _range[0] = newRange.range[0];
        _range[1] = newRange.range[1];

        _transientRange[0] = ( _range[0] < _transientRange[0] ) ? _range[0] : _transientRange[0];
        _transientRange[1] = ( _range[1] > _transientRange[1] ) ? _range[1] : _transientRange[1];
        //bounding box
        double* bbox = flowImage->GetBounds();
        _ensureBBox( bbox );
        /*for ( unsigned int i = 0; i < 6; ++i )
        {
           _bbox[ i ] = bbox[ i ];
        }*/

        int oldResolution[3] =
            {
                0, 0, 0
            };
        if( !_resolution )
        {
            _resolution = new int[3];
        }
        else
        {
            oldResolution[0] = _resolution[0];
            oldResolution[1] = _resolution[1];
            oldResolution[2] = _resolution[2];
        }

        //the dimensions

        flowImage->GetDimensions( _resolution );

        if( !_resolution[2] )
        {
            _resolution[2] = 2;
        }

        if( _dataFields.size() &&
                (( oldResolution[0] != _resolution[0] ) ||
                 ( oldResolution[1] != _resolution[1] ) ||
                 ( oldResolution[2] != _resolution[2] )
                )
           )
        {
            std::cout << "Error!!!" << std::endl;
            std::cout << "Texture resolutions are non-consistent within the set!!" << std::endl;
            std::cout << "Re-Translate the texture data!!!" << std::endl;
            flowImage->Delete();
            return;
        }
        double R, G, B, A;
        float alpha = 0;

        int nPixels = _resolution[0] * _resolution[1] * _resolution[2];
        unsigned char* pixels = 0;
        bool crossRoot = false;
        float deltaMinMax[2] = {0, 0};
        if( _range[0] < 0 )
        {
            if( _range[1] > 0 )
            {
                crossRoot = true;
                deltaMinMax[0] = 0 - _range[0];
                deltaMinMax[1] = _range[1] - 0;
            }
        }
        float invSRange = 1.0;
        if( !crossRoot )
        {
            invSRange = 1.0 / ( _range[1] - _range[0] );
        }
        else
        {
            //only map positive values to color
            //invSRange = 1.0/(_range[1]);
            invSRange = 1.0 / ( _range[1] - _range[0] );
        }
        invSRange = 1.0 / ( _range[1] - _range[0] );
        if( curType == VECTOR )
        {
            pixels = new unsigned char[nPixels*4];
            for( int p = 0; p < nPixels; p++ )
            {
                double* rawData = flowData->GetTuple4( p );
                pixels[p*4   ]  = static_cast< unsigned char >( rawData[ 0 ] );
                pixels[p*4 + 1] = static_cast< unsigned char >( rawData[ 1 ] );
                pixels[p*4 + 2] = static_cast< unsigned char >( rawData[ 2 ] );
                A = static_cast< unsigned char>( rawData[ 3 ] );
                alpha = ( A - _range[0] ) * invSRange;
                pixels[p*4 + 3] = ( unsigned char )( alpha * 255 );
            }
        }
        else if( curType == SCALAR )
        {
            //the scalar data
            float scalarValue = 0;
            if( _useShaders )
            {
                pixels = new unsigned char[nPixels*2];
            }
            else
            {
                pixels = new unsigned char[nPixels*4];
            }
            unsigned char negValue = 0;
            for( int p = 0; p < nPixels; p++ )
            {
                scalarValue = flowData->GetTuple1( p );
                negValue = ( scalarValue <= 0.0 ) ? 255 : 0;
                if( crossRoot )
                {
                    if( negValue )
                    {
                        alpha = 0;//(scalarValue - _range[0])/(deltaMinMax[0]);
                    }
                    else
                    {
                        alpha = ( scalarValue - _range[0] ) / ( deltaMinMax[1] );
                    }
                }
                else
                {
                    alpha = ( negValue ) ? 0 : ( scalarValue - _range[0] ) * invSRange;
                }
                if( _useShaders )
                {
                    pixels[p*2   ]  = ( unsigned char )negValue;
                    pixels[p*2 + 1]  = ( unsigned char )( 255.0 * alpha );
                }
                else
                {
                    //set the scalar pixel data
                    if( alpha <= .5 )
                    {
                        R = 0;
                        G = ( 2.0 * alpha ) * 255,
                            B = ( 1.0 - 2.0 * alpha ) * 255;
                        A = 255 * alpha * .5;
                    }
                    else
                    {
                        R = ( 2.0 * alpha - 1.0 ) * 255;
                        G = ( 2.0 - 2.0 * alpha ) * 255;
                        B = 0.;
                        A = 255 * alpha * .5;
                    }
                    pixels[p*4   ]  = ( unsigned char )R;
                    pixels[p*4 + 1] = ( unsigned char )G;
                    pixels[p*4 + 2] = ( unsigned char )B;
                    pixels[p*4 + 3] = ( unsigned char )A;
                }
            }
        }
        //add the field
        _dataFields.push_back( pixels );
        flowImage->Delete();
    }
    else
    {
        std::cout << "Invalid file: " << textureFile << std::endl;
        std::cout << "cfdTextureManager couldn't load new vector field!!" << std::endl;
        return;
    }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::_ensureBBox( double* newBBox )
{
    ///why doesn't dynamic cast work???
    float fBBox[6];
    fBBox[0] = newBBox[0];
    fBBox[1] = newBBox[1];
    fBBox[2] = newBBox[2];
    fBBox[3] = newBBox[3];
    fBBox[4] = newBBox[4];
    fBBox[5] = newBBox[5];
    if( _lengthOfBBox( fBBox ) > _lengthOfBBox( _bbox ) )
    {
        for( unsigned int i = 0; i < 6; ++i )
        {
            _bbox[ i ] = newBBox[ i ];
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
double cfdTextureManager::_lengthOfBBox( float* bbox )
{
    double xSquared = ( bbox[0] - bbox[1] ) * ( bbox[0] - bbox[1] );
    double ySquared = ( bbox[2] - bbox[3] ) * ( bbox[2] - bbox[3] );
    double zSquared = ( bbox[4] - bbox[5] ) * ( bbox[4] - bbox[5] );
    return sqrt( xSquared + ySquared + zSquared );
}
////////////////////////////////////////////////////////////////////////////////
bool cfdTextureManager::TimeToUpdate()
{
    return _timeToUpdate;
}
////////////////////////////////////////////////////////////////////////////////
unsigned char* cfdTextureManager::getCurrentField()
{
    return _dataFields.at( _curField );
}
////////////////////////////////////////////////////////////////////////////////
unsigned char* cfdTextureManager::getNextField()
{
    //int dir = 1;
    //get the appropriate direction
    //positive
    //if(!plusNeg)dir = -1;

    size_t numFields = _dataFields.size();
    //now update the current field
    if( _direction == 1 )
    {
        //haven't reached the end yet
        //so return the next field
        if( size_t( _curField ) < numFields - 1 )
        {
            _curField += _direction;
        }
        else if( _curField == numFields - 1 )
        {
            //we're at the end so we need to
            //return the beginning
            _curField = 0;
        }
    }
    else
    {
        if( _curField > 0 )
        {
            _curField += _direction;
        }
        else if( _curField == 0 )
        {
            //we're at the end so we need to
            //return the beginning
            _curField = static_cast< int >( numFields - 1 );
        }
    }

    //return _vectorFields.at(_curField);
    unsigned char* field = 0;
    field = _dataFields.at( _curField );
    if( field )
    {
        return field;//_dataFields[_curField];
    }
    else return 0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::CalculateUpdateTime( double curTime, double delay )
{
    size_t numFields = _dataFields.size();

    if (( numFields > 1 ) && _mode == PLAY )
    {
        //This requires VRJ Start Barrier to work properly
        if( curTime - _prevTime >= delay )
        {
            _prevTime = curTime;
            _timeToUpdate = true;
            return;
        }
    }
    _timeToUpdate = false;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTextureManager::setDirection( int forwardBackward )
{
    _direction = forwardBackward;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdTextureManager::GetDataName( void )
{
    return dataName;
}
////////////////////////////////////////////////////////////////////////////////
cfdTextureManager& cfdTextureManager::operator=( const cfdTextureManager& tm )
{
    if( this != &tm )
    {
        if( _dataFields.size() )
        {
            _dataFields.clear();
        }

        if( _types.size() )
        {
            _types.clear();
        }

        if( _ranges.size() )
        {
            _ranges.clear();
        }
        size_t nFields = tm._dataFields.size();

        for( size_t i = 0; i < nFields; i++ )
        {
            _dataFields.push_back( tm._dataFields.at( i ) );
            _types.push_back( tm._types.at( i ) );
            _ranges.push_back( tm._ranges.at( i ) );
        }
        _curField = tm._curField;

        if( !_resolution )_resolution = new int[3];
        _resolution[0] = tm._resolution[0];
        _resolution[1] = tm._resolution[1];
        _resolution[2] = tm._resolution[2];
        _bbox[0] = tm._bbox[0];
        _bbox[1] = tm._bbox[1];
        _bbox[2] = tm._bbox[2];
        _bbox[3] = tm._bbox[3];
        _bbox[4] = tm._bbox[4];
        _bbox[5] = tm._bbox[5];

        _prevTime = tm._prevTime;
        _range[0] = tm._range[0];
        _range[1] = tm._range[1];
        _direction = tm._direction;
        _mode = tm._mode;
    }
    return *this;

}
////////////////////////////////////////////////////////////////////////////////
