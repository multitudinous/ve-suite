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
#include <ves/xplorer/scenegraph/VTKStreamlineTextureCreator.h>

#include <osg/Texture2D>

#include <vtkDataArray.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkLookupTable.h>
#include <vtkPolyData.h>
#include <vtkPoints.h>

using namespace ves::xplorer::scenegraph;

////////////////////////////////////////////////////////////////////////////////
VTKStreamlineTextureCreator::VTKStreamlineTextureCreator()
    : 
    VectorFieldData(),
    m_rawVTKData( 0 )
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
osg::BoundingBox VTKStreamlineTextureCreator::getBoundingBox()
{
    double bounds[6];
    m_rawVTKData->GetPoints()->GetBounds(bounds);
    //VTK does bounds xmin, xmax,....
    //OSG does bounds xmin, ymin, zmin, xmax, ymax,...
    osg::BoundingBox bb(bounds[0],bounds[2],bounds[4],bounds[1],bounds[3],bounds[5]);
    return( bb );
}
////////////////////////////////////////////////////////////////////////////////
VTKStreamlineTextureCreator::~VTKStreamlineTextureCreator()
{
    ;
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::SetPointMultiplier( unsigned int pointMultiplier )
{
    m_pointMultiplier = pointMultiplier;
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::SetPolyData( vtkPolyData* rawVTKData )
{
    m_rawVTKData = rawVTKData;
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::SetPointQueue( std::deque< Point >& pointList )
{
    m_pointList = pointList;
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::internalLoad()
{
    // Actual data size would come from file.
    // NOTE: Crash in NVIDIA friver if total _dataSize
    // is > 32768.  
    _dataSize = m_pointList.size()*m_pointMultiplier;

    // Determine optimal 3D texture dimensions.
    int s, t, p;
    compute3DTextureSize( getDataCount(), s, t, p );
    _texSizes = osg::Vec3s( s, t, p );
    
    // Allocate memory for data.
    //unsigned int size( getDataCount() );
    //_pos = new float[ size * 3 ];
    //_dir = new float[ size * 3 ];
    //_scalar = new float[ size * 3 ];
    _pos = new float[ s * t * p * 3 ];
    //_dir = new float[ s * t * p * 3 ];
    _scalar = new float[ s * t * p ];
    
    // TBD You would replace this line with code to load the data from file.
    // In this example, we just generate test data.
    createDataArrays( _pos, _scalar );
    
    _texPos = makeFloatTexture( (unsigned char*)_pos, 3, osg::Texture2D::NEAREST );
    _texScalar = makeFloatTexture( (unsigned char*)_scalar, 1, osg::Texture2D::NEAREST );        
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::createDataArrays( float* pos, float* scalar )
{
    float* posI = pos;
    float* scalarI = scalar;
    
    vtkPointData* pointData = m_rawVTKData->GetPointData();
    vtkDataArray* scalarArray = pointData->GetScalars(m_scalarName.c_str());
    
    size_t maxData = getDataCount();
    size_t texSize = _texSizes.x() * _texSizes.y() * _texSizes.z();
    size_t numActualPoints = m_pointList.size();
    size_t j = 0;
    size_t nextIndex = 0;
    double curPoint[3] = {0,0,0};
    double nextPoint[3];
    nextPoint[ 0 ] = m_pointList.at(0).x[ 0 ];
    nextPoint[ 1 ] = m_pointList.at(0).x[ 1 ];
    nextPoint[ 2 ] = m_pointList.at(0).x[ 2 ]; 
    
    double curScalar = 0.0;
    double multiplierConstant = 1.0f/double(m_pointMultiplier);
    
    for( size_t i = 0; i < texSize; ++i )
    {
        if( i < maxData )
        {            
            int mod = i%m_pointMultiplier;
            //If we can get the point directly from the vtk dataset
            if( mod == 0 )
            {
                *posI++=(float)nextPoint[0];
                *posI++=(float)nextPoint[1];
                *posI++=(float)nextPoint[2];
                
                curPoint[0]=nextPoint[0];
                curPoint[1]=nextPoint[1];
                curPoint[2]=nextPoint[2];
                
                nextIndex = j + 1;
                if( nextIndex < numActualPoints)
                {
                    nextPoint[ 0 ] = m_pointList.at(nextIndex).x[ 0 ];
                    nextPoint[ 1 ] = m_pointList.at(nextIndex).x[ 1 ];
                    nextPoint[ 2 ] = m_pointList.at(nextIndex).x[ 2 ];
                }
                
                if( j < numActualPoints )
                {
                    //Setup the color array
                    curScalar = scalarArray->GetTuple1( m_pointList.at( j ).vertId );
                    *scalarI++ = curScalar;                    
                }
                ++j;
            }
            //If we need to create points we will do a linear interpolation
            else
            {
                *posI++=(float)(curPoint[0]+mod*(nextPoint[0]-curPoint[0])*multiplierConstant);
                *posI++=(float)(curPoint[1]+mod*(nextPoint[1]-curPoint[1])*multiplierConstant);
                *posI++=(float)(curPoint[2]+mod*(nextPoint[2]-curPoint[2])*multiplierConstant);
                //There is no way for us to reasonably interpolate a scalar
                //value at this point so just use the previous value
                *scalarI++ = curScalar;
            }
        }
        else
        {
            *posI++ = 0.0;
            *posI++ = 0.0;
            *posI++ = 0.0; 

            *scalarI++ = 0.0;
        }
    }
}
////////////////////////////////////////////////////////////////////////////////
void VTKStreamlineTextureCreator::SetActiveVectorAndScalar( const std::string& vectorName, 
    const std::string& scalarName )
{
    m_vectorName = vectorName;
    m_scalarName = scalarName;
}
////////////////////////////////////////////////////////////////////////////////
osg::Image* VTKStreamlineTextureCreator::CreateColorTextures( double* dataRange )
{
    //Here we build a color look up table
    vtkLookupTable* lut = vtkLookupTable::New(); 
    lut->SetHueRange(0.667, 0);
    lut->SetRange(dataRange);
    lut->SetRampToLinear();
    //lut->SetRampToSCurve();
    //lut->SetRampToSQRT();
    lut->Build();
    //lut->Print( std::cout );
    vtkIdType numTuples = lut->GetTable()->GetNumberOfTuples();
    vtkIdType numComponents = lut->GetTable()->GetNumberOfComponents();
    unsigned char* charLut = 0;
    //std::cout << " rgb " << lut->GetTable()->GetNumberOfTuples() << " " 
    //<< lut->GetTable()->GetNumberOfComponents() << " " << charLut << std::endl;
    //unsigned char* charLut2= lut->GetPointer( 0 );
    //std::cout << sizeof( charLut2 ) << std::endl;
    float* newScalarLutArray = new float[ numTuples * 3 ];
    for( int i = 0; i < numTuples; ++i )
    {
        int numLuts = (i*numComponents);
        int numColorIndex = (i*3);
        charLut = lut->GetTable()->WritePointer( numLuts, 0 );
        //std::cout << sizeof( charLut ) << " " << charLut<<  " " 
        //<< (double*)charLut << std::endl;
        newScalarLutArray[  numColorIndex + 0 ] = float( charLut[ 0 ] )/255.0f;
        newScalarLutArray[  numColorIndex + 1 ] = float( charLut[ 1 ] )/255.0f;
        newScalarLutArray[  numColorIndex + 2 ] = float( charLut[ 2 ] )/255.0f;
        //newScalarLutArray[  numLuts + 3 ] = float( charLut[ 3 ] )/255.0f;
        //std::cout << newScalarLutArray[  numLuts + 0 ] << " " 
        //    << newScalarLutArray[  numLuts + 1 ] << " "
        //    << newScalarLutArray[  numLuts + 2 ] << " " 
        //    << newScalarLutArray[  numLuts + 3 ] << std::endl;
    }
    //std::string tempString( charLut );
    lut->Delete();
    
    // Set up the color spectrum.
    osg::Image* iColorScale = new osg::Image();
    iColorScale->setImage( int( numTuples ), 1, 1, GL_RGB32F_ARB, GL_RGB, GL_FLOAT,
                          (unsigned char*)newScalarLutArray, osg::Image::NO_DELETE );
    return iColorScale;
}
////////////////////////////////////////////////////////////////////////////////
