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
#include <string>
#include <ves/xplorer/event/viz/cfdPlanes.h>
#include <ves/xplorer/event/viz/cfdCuttingPlane.h>
#include <ves/xplorer/util/fileIO.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

#include <vtkCellArray.h>
#include <vtkFloatArray.h>
#include <vtkPlane.h>
#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkDataSet.h>
#include <vtkAppendPolyData.h>
#include <vtkCutter.h>

#include <ves/xplorer/Debug.h>

#include <sstream>

using namespace ves::xplorer;
using namespace ves::xplorer::util;

cfdPlanes::cfdPlanes( const int xyz, const char directory[],
                      const double bounds[ 6 ] )
    :
    numPlanes( 0 ),
    isPlaneSelected( NULL ),
    collectivePolyData( NULL ),
    cuttingPlane( NULL ),
    type( xyz )
{
    if( this->type == 0 ) this->typeLabel = 'X';
    else if( this->type == 1 ) this->typeLabel = 'Y';
    else if( this->type == 2 ) this->typeLabel = 'Z';
    else
    {
        std::cerr << "ERROR: in cfdPlanes, xyz must be 0, 1, or 2" << std::endl;
        exit( 1 );
    }
    vprDEBUG( vesDBG, 1 ) << "this->typeLabel = " << this->typeLabel
        << std::endl << vprDEBUG_FLUSH;

    vprDEBUG( vesDBG, 1 ) << "directory: \"" << directory << "\""
        << std::endl << vprDEBUG_FLUSH;

    // count the total number of cut planes
    for( int i = 0; 1; i++ )
    {
        std::ostringstream dirStringStream;
        dirStringStream << directory << "/" 
            << this->typeLabel << "_Cont" << i << ".vtk";
        std::string dirString = dirStringStream.str();

        if( !fileIO::isFileReadable( dirString ) )
        {
            this->numPlanes = i;
            vprDEBUG( vesDBG, 0 ) << "\t\tFound " << this->numPlanes
                << " " << this->typeLabel << "-planes"
                << std::endl << vprDEBUG_FLUSH;
            break;
        }
    }

    if( this->numPlanes == 0 )
    {
        return;
    }

    for( int i = 0; i < this->numPlanes; i++ )
    {
        std::ostringstream dirStringStream;
        dirStringStream << directory << "/" 
            << this->typeLabel << "_Cont" << i << ".vtk";
        std::string dirString = dirStringStream.str();

        vtkDataObject* tempobject = readVtkThing( dirString );
        vtkPolyData* tempPolyData = vtkPolyData::SafeDownCast( tempobject );
        if( tempPolyData->GetPoints()->GetNumberOfPoints() > 0 )
        {
            // look at POINTS to see what coordinate that the plane goes through
            double vertex[ 3 ];
            tempPolyData->GetPoints()->GetPoint( 0, vertex );
            sliceLocation.push_back( vertex[this->type] );
            vprDEBUG( vesDBG, 1 ) << "\t\tplane[" << i
                << "] goes through coordinate "
                << sliceLocation.back() 
                << std::endl << vprDEBUG_FLUSH;
            
            vtkPolyData* testPD = vtkPolyData::New();
            testPD->ShallowCopy( tempPolyData );
            m_pdSlices.push_back( testPD );
        }
        tempPolyData->Delete();
    }
    numPlanes = m_pdSlices.size();
    
    // allocate space for the array that keeps track of which planes
    // are selected for display...
    this->isPlaneSelected = new int [ this->numPlanes ];

    // Set all planes to be selected and concatenate them all into one
    // polyData object called collectivePolyData
    //this->SetAllPlanesSelected();
    //this->ConcatenateSelectedPlanes();

    // create a cutting plane object from the bounds of the original dataset
    // to use when the user wants a close-enough plane
    this->cuttingPlane = new cfdCuttingPlane( bounds, xyz, 1 );

    /*
       // create a cutting plane object from the bounds of the collectivePolyData
       // to use when the user wants a close-enough plane
       this->cuttingPlane = new cfdCuttingPlane(
                                  this->collectivePolyData->GetBounds(), xyz, 1 );
    */
}

cfdPlanes::~cfdPlanes()
{
    if( this->numPlanes > 0 )
    {
        for( int i = 0; i  < this->numPlanes; i++ )
        {
            m_pdSlices[ i ]->Delete();
        }
    }
    m_pdSlices.clear();
    
    if( this->collectivePolyData != NULL )
    {
        //this->collectivePolyData->Delete();
        this->collectivePolyData = NULL;
    }

    if( this->cuttingPlane != NULL )
    {
        delete this->cuttingPlane;
        this->cuttingPlane = NULL;
    }

    if( this->isPlaneSelected != NULL )
    {
        delete [] this->isPlaneSelected;
        this->isPlaneSelected = NULL;
    }

    sliceLocation.clear();
}

void cfdPlanes::SetAllPlanesSelected( void )
{
    for( int i = 0; i < this->numPlanes; i++ )
    {
        this->isPlaneSelected[ i ] = 1;
    }
}

vtkPolyDataAlgorithm* cfdPlanes::GetPlanesData( void )
{
    return this->collectivePolyData;
}

// 0 <= sliderBarPos <= 100
vtkPolyData * cfdPlanes::GetClosestPlane( const int sliderBarPos )
{
    if( this->numPlanes == 0 )
    {
        return NULL;
    }

    this->cuttingPlane->Advance( sliderBarPos );

    double origin[3];
    this->cuttingPlane->GetOrigin( origin );
    double coordinate = origin[ this->type ];

    vprDEBUG( vesDBG, 1 )
    << "activating precomputed plane corresponding to requested coordinate: "
    << coordinate << " : Slider Bar Position : " << sliderBarPos << std::endl << vprDEBUG_FLUSH;

    float leastSquaredDistance = 1e12;
    int index = 0;

    for( int i = 0; i < this->numPlanes; i++ )
    {
        float sqDist = ( coordinate - sliceLocation[ i ] ) *
                       ( coordinate - sliceLocation[ i ] );
        vprDEBUG( vesDBG, 1 )
            << "plane " << i << ": sliceLocation = " << sliceLocation[ i ]
            << ", sqDist = " << sqDist << std::endl << vprDEBUG_FLUSH;

        if( leastSquaredDistance > sqDist )
        {
            leastSquaredDistance = sqDist;
            index = i;
        }
    }
    this->isPlaneSelected[ index ] = 1;

    return m_pdSlices[ index ];
}

void cfdPlanes::ConcatenateSelectedPlanes( void )
{
    if( this->numPlanes == 0 )
    {
        return;
    }

    collectivePolyData = vtkAppendPolyData::New();

    for( int i = 0; i < this->numPlanes; i++ )
    {
        vprDEBUG( vesDBG, 1 )
            << "isPlaneSelected[" << i << "] = " << isPlaneSelected[ i ]
            << std::endl << vprDEBUG_FLUSH;

        if( !isPlaneSelected[ i ] )
        {
            continue;
        }
        collectivePolyData->AddInput( m_pdSlices[ i ] );
    }

    collectivePolyData->Update();
/*
    if( this->collectivePolyData != NULL )
    {
        // user classes should delete
        //this->collectivePolyData->Delete();
    }
    this->collectivePolyData = vtkPolyData::New();
    this->collectivePolyData->ShallowCopy( appendPolyData->GetOutput() );
    collectivePolyData->Update();
    appendPolyData->Delete();*/
}

int cfdPlanes::GetNumberOfPlanes()
{
    return this->numPlanes;
}

vtkPolyData* cfdPlanes::GetPlane( const int i )
{
    return m_pdSlices[ i ];
}

