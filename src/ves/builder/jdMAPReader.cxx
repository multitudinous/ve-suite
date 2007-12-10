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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <jdMAPReader.h>
#include <fstream>
#include <iostream>
#include <string>
#include <sstream>

#include <converter.h>

#include <vtkPolyDataWriter.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkDataSetAttributes.h>
#include <vtkTransform.h>
#include <vtkObjectBase.h>
#include <vtkTransformPolyDataFilter.h>
#include <vtkSphereSource.h>
#include <vtkGlyph3D.h>
#include <vtkDoubleArray.h>
#include <vtkLookupTable.h>

jdMAPReader::jdMAPReader( void )
{
    std::cout << "[DBG] Inside Ctor..." << std::endl;
    writer      = vtkPolyDataWriter::New();
    writer->SetFileTypeToBinary();
    //polydata    = vtkPolyData::New();
    //points      = vtkPoints::New();
    //parameterData = vtkFloatArray::New();
    translateFluentPartToVTK();
}

jdMAPReader::~jdMAPReader( void )
{
    writer->Delete();
    writer = NULL;
    //polydata->Delete();
    polydata = NULL;
    //points->Delete();
    points = NULL;
    //parameterData->Delete();
    //transform->Delete();
    //transFilter->Delete();
}

void jdMAPReader::translateFluentPartToVTK( void )
{
    std::cout << "[DBG] Translating Fluent Part To VTK" << std::endl;

    numParameters = 1;
    vtkIdType id;
    int IDinVTKFile = 0;
    int position[ 3 ];
    int rgb[ 3 ];
    double normRGB[ 3 ];
    int i;
    char comma[1];

    std::ifstream inFile;
    char inputFileName[256];
    //char textLine[256];
    std::string line;
    const std::string letters( "X," );
    std::string names2;
    //const std::string letters ( "XE" );
    std::cout << "Input filename to be converted : " << std::endl;
    std::cin >> inputFileName;
    inFile.open( inputFileName );
    std::getline( inFile, line );
    names2 = line.substr( 0, 2 );
    //std::cout << names2 << std::endl;
    //std::cout << names2.compare( letters )  << std::endl;
    //exit( 1 );

    polydata = vtkPolyData::New();
    polydata->Allocate();
    this->points = vtkPoints::New();
    // set up arrays to store scalar and vector data over entire mesh...
    parameterData = NULL;
    parameterData = new vtkFloatArray * [numParameters];
    this->transform   = vtkTransform::New();
    this->transFilter = vtkTransformPolyDataFilter::New();

    for( i = 0; i < numParameters; i++ )
    {
        parameterData[i] = vtkFloatArray::New();
        parameterData[i]->SetNumberOfComponents( 1 );
        if( parameterData[i] == NULL )
        {
            std::cerr << "ERROR: can't get memory for parameterData, so exiting" << std::endl;
            exit( 1 );
        }
    }
    parameterData[ 0 ]->SetName( "Point_Number" );

    vtkDoubleArray** lutData = new vtkDoubleArray* [ 1 ];
    lutData[ 0 ] = vtkDoubleArray::New();
    lutData[ 0 ]->SetNumberOfComponents( 3 );

    while( !inFile.eof() )
    {
        std::getline( inFile, line );
        names2 = line.substr( 0, 2 );
        if( names2.compare( letters ) == 0 )
        {
            std::cout << line << std::endl;
            continue;
        }
        std::istringstream input( line );
        IDinVTKFile++;

        /*
              int value;
              input >> value >> comma[ 0 ];
              std::cout << value << " " << comma[ 0 ];
              input >> value >> comma[ 0 ];
              std::cout << value << " " << comma[ 0 ];
        */

        input >> position[ 0 ] >> comma[ 0 ] >> position[ 1 ] >> comma[ 0 ] >> position[ 2 ] >> comma[ 0 ]
        >> rgb[ 0 ] >> comma[ 0 ] >> rgb[ 1 ] >> comma[ 0 ] >> rgb[ 2 ];
        //std::cout << position[ 2 ] << " " << comma[ 0 ] << position[ 0 ] << " " << comma[ 0 ] << position[ 1 ]<< std::endl;
        id = IDinVTKFile - 1;

        parameterData[ 0 ]->InsertValue( id, ( float )id );

        points->InsertPoint( id, ( float )position[ 0 ],
                             ( float )position[ 1 ],
                             ( float )position[ 2 ] );
        normRGB[ 0 ] = ( double )rgb[ 0 ] / ( double )255;
        normRGB[ 1 ] = ( double )rgb[ 1 ] / ( double )255;
        normRGB[ 2 ] = ( double )rgb[ 2 ] / ( double )255;

        lutData[ 0 ]->InsertTuple3( id, ( double )normRGB[ 0 ],
                                    ( double )normRGB[ 1 ],
                                    ( double )normRGB[ 2 ] );

        polydata->InsertNextCell( VTK_VERTEX, 1, ( vtkIdType * )&id );
        //if ( line != NULL )
        //   std::cout << id << " : " << line << std::endl;
    }
    std::cout << "here " << std::endl;

    lut = vtkLookupTable::New();
    lut->SetNumberOfTableValues( IDinVTKFile );
    std::cout <<  IDinVTKFile << std::endl;
    for( int i = 0; i < IDinVTKFile; i++ )
    {
        double* data = lutData[ 0 ]->GetTuple3( i );

        if( data == NULL )
        {
            std::cout << "here " << i << std::endl;
            //exit();
        }
        lut->SetTableValue( i, data[ 0 ], data[ 1 ], data[ 2 ], 1 );
    }

    std::cout << "Number of Points = " << IDinVTKFile - 1 << std::endl;
    inFile.close();

    writePolydata();
}

void jdMAPReader::writePolydata( )
{
    float   xRot,   yRot,   zRot;
    float xTrans, yTrans, zTrans;
    float xScale, yScale, zScale;
    // char textLine[ 256 ];

    //ifstream paramFile( "./particle.param", ios::in );
    //std::ostringstream file_name;
    std::string outVtkFileName = "data.vtk";
    //file_name<<"finalVtkFile_0"<<timestep<<".vtk";
    //outVtkFileName=file_name.str();
    //outVtkfile.open((outVtkFileName).c_str(),std::ios::app||std::ios::ate);

    polydata->SetPoints( points );
    //letUsersAddParamsToField( numParameters, parameterData, polydata->GetPointData(), 0 );
    std::cout << "here 1 " << std::endl;
    polydata->Update();
    polydata->GetPointData()->SetScalars( parameterData[ 0 ] );
    //lut->PrintSelf( cout,  );
    polydata->GetPointData()->GetScalars()->SetLookupTable( lut );
    std::cout << "here 2 " << std::endl;
    polydata->Update();
    /*
       this->sphere   = vtkSphereSource::New();

       this->sphere->SetRadius( 0.5f);

       this->sphere->SetThetaResolution( 3 );
       this->sphere->SetPhiResolution( 3 );
       this->sphere->Update();

       this->glyph = vtkGlyph3D::New();
       this->glyph->SetSource( this->sphere->GetOutput() );
       this->glyph->SetInput( this->polydata );
    */
    xTrans = 0.0f;
    yTrans = 0.0f;
    zTrans = 0.0f;
    yScale = zScale = xScale = 0.002152782f;
    xRot = 0;
    yRot = 0;
    zRot = 0;
    /*std::cout << xRot   << " : " << yRot   << " : " << zRot   << " : "
         << xTrans << " : " << yTrans << " : " << zTrans << " : "
         << xScale << " : " << coordinateFilter << std::endl;*/
    transform->Scale( xScale, yScale, zScale );
    transform->RotateX( xRot );
    transform->RotateY( yRot );
    transform->RotateZ( zRot );
    transform->Translate( xTrans, yTrans, zTrans );
    transform->Update();

    //transFilter->SetInput( this->glyph->GetOutput() );
    transFilter->SetInput( polydata );
    transFilter->SetTransform( transform );
    transFilter->Update();

    writer->SetInput( transFilter->GetOutput() );
    writer->SetFileName(( outVtkFileName ).c_str() );
    writer->Write();
    //outVtkfile.close();
}

