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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.rb END do not edit this line> ***************/
#include <vtkPolyDataWriter.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkPointData.h>
#include <vtkPoints.h>
#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkDataSetAttributes.h>
#include <vtkTransform.h>
#include <vtkTransformPolyDataFilter.h>
#include <reiParticles.h>
#include <particle.h>
#include <iostream>
#include <fstream>
#include <converter.h>

reiParticles::reiParticles( void )
{
    writer      = vtkPolyDataWriter::New();
    polydata    = vtkPolyData::New();
    points      = vtkPoints::New();
    lines       = vtkCellArray::New();
    //parameterData = vtkFloatArray::New();
    transform   = vtkTransform::New();
    transFilter = vtkTransformPolyDataFilter::New();

    readPPLOT1();
    readPPLOT3();
    writeParticlePolyData();
}

reiParticles::~reiParticles( void )
{
    writer->Delete();
    //scalar->Delete();
    polydata->Delete();
    points->Delete();
    lines->Delete();
    transform->Delete();
    transFilter->Delete();
}

reiParticles::reiParticles( reiParticles * )
{}

void reiParticles::readPPLOT1( void )
{
    std::ifstream inPPLOT1file( "PPLOT1", std::ios::in );
    float trash;
    int i;

    inPPLOT1file >> nsl >> nps;

    for( i = 0; i < nsl * nps; i++ )
    {
        particles.push_back( new Particle() );
        inPPLOT1file >>  particles[i]->timeSteps;
    }

    inPPLOT1file >> trash >> trash >> trash;

    for( i = 0; i < nsl * nps; i++ )
        inPPLOT1file >>  particles[i]->pathIndex;
    /*
       for(i = 0; i < particles.size(); i++ )
          std::cout << particles[i]->timeSteps << "  " << particles[i]->pathIndex << std::endl;
          std::cout << nsl << "  " << nps << std::endl;
    */
}

void reiParticles::readPPLOT3( void )
{
    int lineNumber;
    char title[20];
    int i;
    float px, py, pz;
    int counter;

    std::ifstream inPPLOT3file( "PPLOT3", std::ios::in );
    inPPLOT3file >> lineNumber >> title;

    for( i = 0; i < nsl * nps; i++ )
    {
        inPPLOT3file >> lineNumber >> particles[i]->particleSize >> particles[i]->particleNumber;
        while( particles[i]->timeSteps + particles[i]->pathIndex > lineNumber )
        {
            particles[i]->locations.push_back( new Location() );
            counter = particles[i]->locations.size() - 1;
            inPPLOT3file >> lineNumber
            >> particles[i]->locations[counter]->x
            >> particles[i]->locations[counter]->y
            >> particles[i]->locations[counter]->z
            >> particles[i]->locations[counter]->time
            >> particles[i]->locations[counter]->IDLIM
            >> particles[i]->locations[counter]->particleCoalFraction
            >> particles[i]->locations[counter]->particleCharFraction
            >> particles[i]->locations[counter]->CPTREF
            >> particles[i]->locations[counter]->particleTemperature
            >> particles[i]->locations[counter]->gasTemperature
            >> particles[i]->locations[counter]->particleSize
            >> particles[i]->locations[counter]->particleCloudDispersion
            >> particles[i]->locations[counter]->massOfChar
            >> particles[i]->locations[counter]->moistureFraction;
            if( px == particles[i]->locations[counter]->x &&
                    py == particles[i]->locations[counter]->y &&
                    pz == particles[i]->locations[counter]->z )
            {
                particles[i]->locations.pop_back();
                //std::cout << px << " " << py << " " << pz << std::endl;
            }
            else
            {
                px = particles[i]->locations[counter]->x;
                py = particles[i]->locations[counter]->y;
                pz = particles[i]->locations[counter]->z;
            }
        }
    }

    int debug = 0;
    if( debug )
        for( i = 0; i < ( int )particles.size(); i++ )
            for( int j = 0; j < ( int )particles[i]->locations.size(); j++ )
                std::cout << particles[i]->locations[j]->x << "  "
                << particles[i]->locations[j]->y << "  "
                << particles[i]->locations[j]->z << "  "
                << particles[i]->locations[j]->time << "  "
                << particles[i]->locations[j]->IDLIM << "  "
                << particles[i]->locations[j]->particleCoalFraction << "  "
                << particles[i]->locations[j]->particleCharFraction << "  "
                << particles[i]->locations[j]->CPTREF << "  "
                << particles[i]->locations[j]->particleTemperature << "  "
                << particles[i]->locations[j]->gasTemperature << "  "
                << particles[i]->locations[j]->particleSize << "  "
                << particles[i]->locations[j]->particleCloudDispersion << "  "
                << particles[i]->locations[j]->massOfChar << "  "
                << particles[i]->locations[j]->moistureFraction << std::endl;

}

void reiParticles::writeParticlePolyData( void )
{
    int i, j, k;
    int counter;
    int coordinateFilter;
    float   xRot,   yRot,   zRot;
    float xTrans, yTrans, zTrans;
    float xScale, yScale, zScale;
    char textLine[ 256 ];
    int numParameters = 10; // Number of scalars to be stored

    // set up arrays to store scalar and vector data over entire mesh...
    parameterData = NULL;
    parameterData = new vtkFloatArray * [numParameters];
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

    parameterData[ 0 ]->SetName( "IDLIM" );
    parameterData[ 1 ]->SetName( "Particle Coal Fraction" );
    parameterData[ 2 ]->SetName( "Particle Char Fraction" );
    parameterData[ 3 ]->SetName( "CPTREF" );
    parameterData[ 4 ]->SetName( "Particle Temperature" );
    parameterData[ 5 ]->SetName( "Gas Temperature" );
    parameterData[ 6 ]->SetName( "Particle Size" );
    parameterData[ 7 ]->SetName( "Particle Cloud Dispersion" );
    parameterData[ 8 ]->SetName( "Mass Of Char" );
    parameterData[ 9 ]->SetName( "Moisture Fraction" );

    //scalar->SetName("Gas_temp");
    polydata->Allocate();
    k = 0;
    counter = 0;

    std::ifstream paramFile( "./particle.param", std::ios::in );
    int particleNumber;
    typedef std::vector< int > IntList;
    IntList whichParticles;

    if( !paramFile )
    {
        std::cerr << "File Could Not Be Opened" << std::endl;
        exit( 1 );
    }

    paramFile >> xRot   >> yRot   >> zRot;
    paramFile.getline( textLine, 256 );   //skip past remainder of line
    paramFile >> xTrans >> yTrans >> zTrans;
    paramFile.getline( textLine, 256 );   //skip past remainder of line
    paramFile >> xScale >> coordinateFilter;
    paramFile.getline( textLine, 256 );   //skip past remainder of line

    yScale = zScale = xScale;
    std::cout << xRot   << " : " << yRot   << " : " << zRot   << " : "
    << xTrans << " : " << yTrans << " : " << zTrans << " : "
    << xScale << " : " << coordinateFilter << std::endl;

    while( paramFile >> particleNumber )
    {
        std::cout << " Particle Number : " << particleNumber << std::endl;
        paramFile.getline( textLine, 256 );   //skip past remainder of line
        whichParticles.push_back( particleNumber );
    }

    for( i = 0; i < ( int )particles.size(); i++ )
    {
        if( i + 1 == whichParticles[k] )
        {
            int test = 0;
            for( int y = 0; y < ( int )particles[i]->locations.size(); y += coordinateFilter )
                test++;

            std::cout << " Time Steps = " << particles[i]->timeSteps << " "
            << " Path Index = " << whichParticles[k] << " "
            << " Actual Time Steps = " << test << " "
            << " PPLOT3 LINE Index = " << particles[i]->pathIndex << std::endl;

            lines->InsertNextCell( test );
            for( j = 0; j < ( int )particles[i]->locations.size(); j += coordinateFilter )
            {
                points->InsertNextPoint( particles[i]->locations[j]->x,
                                         particles[i]->locations[j]->y,
                                         particles[i]->locations[j]->z );
                //scalar->InsertNextValue( particles[i]->locations[j]->gasTemperature );
                //parameterData[ k ]->InsertNextValue( particles[i]->locations[j]->gasTemperature );
                parameterData[ 0 ]->InsertNextValue( particles[i]->locations[j]->IDLIM );
                parameterData[ 1 ]->InsertNextValue( particles[i]->locations[j]->particleCoalFraction );
                parameterData[ 2 ]->InsertNextValue( particles[i]->locations[j]->particleCharFraction );
                parameterData[ 3 ]->InsertNextValue( particles[i]->locations[j]->CPTREF );
                parameterData[ 4 ]->InsertNextValue( particles[i]->locations[j]->particleTemperature );
                parameterData[ 5 ]->InsertNextValue( particles[i]->locations[j]->gasTemperature );
                parameterData[ 6 ]->InsertNextValue( particles[i]->locations[j]->particleSize );
                parameterData[ 7 ]->InsertNextValue( particles[i]->locations[j]->particleCloudDispersion );
                parameterData[ 8 ]->InsertNextValue( particles[i]->locations[j]->massOfChar );
                parameterData[ 9 ]->InsertNextValue( particles[i]->locations[j]->moistureFraction );
                lines->InsertCellPoint( counter );
                counter++;
            }
            k++;
            if( k >= ( int )whichParticles.size() )
                break;
        }
    }

    polydata->SetPoints( points );
    polydata->SetLines( lines );
    //polydata->GetPointData()->SetScalars( scalar );
    letUsersAddParamsToField( numParameters, parameterData, polydata->GetPointData() );
    polydata->Update();

    transform->Scale( xScale, yScale, zScale );
    transform->RotateX( xRot );
    transform->RotateY( yRot );
    transform->RotateZ( zRot );
    transform->Translate( xTrans, yTrans, zTrans );
    transform->Update();

    transFilter->SetInput( polydata );
    transFilter->SetTransform( transform );
    transFilter->Update();

    writer->SetInput( transFilter->GetOutput() );
    writer->SetFileName( "vtkPolyData.vtk" );
    writer->SetFileTypeToBinary();
    writer->Write();

    //delete parameterData
    for( i = 0; i < numParameters; i++ )
        parameterData[i]->Delete();

    delete [] parameterData;
    parameterData = NULL;

}

void reiParticles::readParticleParamFile( void )
{
    //std::cout << "in" <<std::endl;
}
