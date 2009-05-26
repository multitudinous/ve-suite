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
#include <ves/xplorer/event/viz/cfdAnimatedStreamlineCone.h>

#include <ves/xplorer/event/viz/cfdStreamers.h>

#include <ves/xplorer/DataSet.h>
#include <ves/xplorer/environment/cfdEnum.h>

#include <ves/open/xml/Command.h>

#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkSphereSource.h>
#include <vtkGlyph3D.h>
#include <vtkPolyDataWriter.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkGenericCell.h>
#include <vtkCleanPolyData.h>
#include <vtkXMLPolyDataWriter.h>
#include <vtkPointData.h>
#include <vtkMath.h>

#include <ves/xplorer/Debug.h>

using namespace ves::xplorer;
using namespace ves::xplorer::scenegraph;
////////////////////////////////////////////////////////////////////////////////
cfdAnimatedStreamlineCone::cfdAnimatedStreamlineCone( void )
    :
    m_streamers( 0 )
{
    this->mapper   = vtkPolyDataMapper::New();
    this->polydata = vtkPolyData::New();
    this->polyData = vtkPolyData::New();
    this->glyph    = vtkGlyph3D::New();
    this->sphere   = vtkSphereSource::New();

    //this->_sequence = new cfdTempAnimation();
    this->particleDiameter = 1.0f;
}
////////////////////////////////////////////////////////////////////////////////
cfdAnimatedStreamlineCone::~cfdAnimatedStreamlineCone()
{
    this->mapper->Delete();
    this->polydata->Delete();
    this->polyData->Delete();
    this->glyph->Delete();
    this->sphere->Delete();
}
////////////////////////////////////////////////////////////////////////////////
void cfdAnimatedStreamlineCone::SetStreamlineSource( cfdStreamers* streamers )
{
    m_streamers = streamers;
}
////////////////////////////////////////////////////////////////////////////////
void cfdAnimatedStreamlineCone::SetPolyDataSource( vtkPolyData *input )
{
    polyData->DeepCopy( input );
}
////////////////////////////////////////////////////////////////////////////////
void cfdAnimatedStreamlineCone::Update( void )
{
    vtkCleanPolyData* cleanPD = vtkCleanPolyData::New();
    cleanPD->PointMergingOn();
    cleanPD->SetInput( m_streamers->GetStreamersOutput() );
    cleanPD->Update();

    polyData->DeepCopy( cleanPD->GetOutput() );
    cleanPD->Delete();
    
    vtkXMLPolyDataWriter *writer = vtkXMLPolyDataWriter::New();
     writer->SetInput( polyData );
     writer->SetFileName( "outPut.vtk" );
     writer->SetDataModeToAscii();
     writer->Write();
     writer->Delete();
    
    m_streamlines.clear();
    
    vtkIdType cellId;        //vtkIdType
    vtkIdType npts;          //vtkIdType
    vtkPoints * points;
    vtkPoints **pointsArray;

    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Cells : " << this->polyData->GetNumberOfCells()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Lines : " << this->polyData->GetNumberOfLines()
        << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 )
        << "|\tNumber of Points : " << this->polyData->GetNumberOfPoints()
        << std::endl << vprDEBUG_FLUSH;
    
    int numberOfStreamLines;
    numberOfStreamLines = this->polyData->GetNumberOfLines();

    if( numberOfStreamLines == 0 )
    {
        std::cout << "|\tcfdAnimatedStreamlineCone::Update : Number of streamlines is 0 " << std::endl;
        return;
    }

    // Find the maximum number of points in one streamline
    int maxNpts = 0;
    int minNpts = 1000000;

    vtkPoints* points2 = 0;
    double x2[ 3 ];
    double x1[ 3 ];
    for( cellId = 0; cellId < numberOfStreamLines; ++cellId )
    {
        points = this->polyData->GetCell( cellId )->GetPoints();
        points->GetPoint( 0, x1 );
        npts = points->GetNumberOfPoints();

        bool foundmatch = false;
        for( vtkIdType cellId2 = cellId + 1; cellId2 < numberOfStreamLines; ++cellId2 )
        {
            points2 = this->polyData->GetCell( cellId2 )->GetPoints();
            points2->GetPoint( 0, x2 );
            if( (x1[ 0 ] == x2[ 0 ]) && (x1[ 1 ] == x2[ 1 ]) && (x1[ 2 ] == x2[ 2 ]) )
            {
                std::cout << "|\t\tx1[ " << 0 << " ] = " << x1[ 0 ] << " : "
                << x1[ 1 ] << " : " << x1[ 2 ] << " " << cellId << std::endl;
                std::cout << "|\t\tx2[ " << 0 << " ] = " << x2[ 0 ] << " : "
                << x2[ 1 ] << " : " << x2[ 2 ] << " " << cellId2 << std::endl;
                std::cout << " Found a match " << cellId << " " << cellId2 << std::endl;
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( cellId, cellId2 ) );
                foundmatch = true;
                npts += points2->GetNumberOfPoints();
                break;
            }
        }

        vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of points in cell " << cellId
            << " = " << npts << std::endl << vprDEBUG_FLUSH;
        if( maxNpts < npts )
            maxNpts = npts;
        
        if( minNpts > npts )
            minNpts = npts;
        
        if( !foundmatch )
        {
            bool foundstandalone = true;
            std::cout << "did not find match " << cellId << std::endl;
            
            for( size_t i = 0; i < m_streamlines.size(); ++i )
            {
                if( (m_streamlines.at( i ).first == cellId) || (m_streamlines.at( i ).second == cellId) )
                {
                    foundstandalone = false;
                    break;
                }
            }
            
            if( !foundstandalone )
            {
                continue;
            }
            
            vtkIdType globalPointId1 = polyData->FindPoint( x1 );
            points->GetPoint( 1, x2 );
            //vtkIdType globalPointId2 = polyData->FindPoint( x2 );

            double xComp = x2[ 0 ] - x1[ 0 ];
            double yComp = x2[ 1 ] - x1[ 1 ];
            double zComp = x2[ 2 ] - x1[ 2 ];

            polyData->GetPointData()->GetVectors( GetActiveDataSet()->GetActiveVectorName().c_str() )->GetTuple( globalPointId1, x1 );
            //polyData->GetPointData()->GetVectors( GetActiveDataSet()->GetActiveVectorName().c_str() )->GetTuple( globalPointId2, x2 );
            double diffX = xComp - x1[ 0 ];
            double diffY = yComp - x1[ 1 ];
            double diffZ = zComp - x1[ 2 ];
            std::cout << diffX << " " << diffY << " " << diffZ << std::endl;

            /*double floorX = vtkMath::Floor( diffX );
            double floorY = vtkMath::Floor( diffY );
            double floorZ = vtkMath::Floor( diffZ );
            std::cout << floorX << " " << floorY << " " << floorZ << std::endl;
             */
             bool isBackwards = false;
             if( (diffX < 0) || (diffY < 0) || (diffZ < 0) )
             {
                 isBackwards = true;
             }
            
            if( isBackwards )
            {
                //Is a backward integrated line
                std::cout << " Use backward" << std::endl;
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( -1, cellId ) );
            }
            else
            {
                std::cout << " Use forward" << std::endl;
                m_streamlines.push_back( std::make_pair< vtkIdType, vtkIdType >( cellId, -1 ) );
            }
        }
    }

    // Define the points at each integration time step
    pointsArray = new vtkPoints*[ maxNpts ];
    for( size_t i = 0; i < maxNpts;  i++ )
    {
        pointsArray[ i ] = vtkPoints::New();
    }

    double *x;

    for( size_t i = 0; i < m_streamlines.size(); ++i )
    {
        cellId = m_streamlines.at( i ).first;
        // For forward integrated points
        int forwardPoints = 0;
        if( cellId != -1 )
        {
            points = polyData->GetCell( cellId )->GetPoints();
            forwardPoints = points->GetNumberOfPoints();
            vprDEBUG( vesDBG, 1 )
                << "|\t\tNumber of Forward points = " << forwardPoints
                << std::endl << vprDEBUG_FLUSH;
            for( size_t j = 0; j < forwardPoints; ++j )
            {
                x = points->GetPoint( j );
                /*if( j == 0 )
                {
                    vprDEBUG( vesDBG, 0 )
                    << "|\t\tx[ " << j << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << " " << cellId << std::endl << vprDEBUG_FLUSH;
                }*/
                vprDEBUG( vesDBG, 3 )
                    << "|\t\tx[ " << j << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
                pointsArray[ j ]->InsertNextPoint( x );
            }
        }
        
        cellId = m_streamlines.at( i ).second;
        // For backward integrated points
        if( cellId != -1 )
        {
            points = polyData->GetCell( cellId )->GetPoints();
            npts = points->GetNumberOfPoints();
            vprDEBUG( vesDBG, 1 ) << "|\t\tNumber of Backward points = " << npts
                << std::endl << vprDEBUG_FLUSH;
            for( int j = npts - 1; j >= 0; j-- )
            {
                x = points->GetPoint( j );
                /*if( j == 0 )
                {
                    vprDEBUG( vesDBG, 0 )
                    << "|\t\tx[ " << i << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << " " << cellId + ( increment - 1 ) << std::endl << vprDEBUG_FLUSH;
                }*/
                vprDEBUG( vesDBG, 3 )
                    << " x[ " << j << " ] = " << x[ 0 ] << " : "
                    << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
                pointsArray[( npts - 1 ) - j + forwardPoints]->InsertNextPoint( x );
            }
        }
    }

    this->sphere->SetRadius( m_streamers->GetArrowDiameter() );
    this->sphere->SetThetaResolution( 3 );
    this->sphere->SetPhiResolution( 3 );
    this->sphere->Update();

    vprDEBUG( vesDBG, 1 ) << "|\t\tmaxNpts = " 
        << maxNpts << std::endl << vprDEBUG_FLUSH;
    vprDEBUG( vesDBG, 1 ) << "|\t\tminNpts = " 
        << minNpts << std::endl << vprDEBUG_FLUSH;
    int w = maxNpts;
    double decimalRatio = ( double )w / 150.0;
    int ratio = ( int )ceil( decimalRatio );

    for( size_t i = 0; i < w; i += ratio )
    {
        //Make ploydata from the points
        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop " << i << std::endl << vprDEBUG_FLUSH;
        this->polydata->SetPoints( pointsArray[ i ] );
        //polydata->Update();
        //polydata->Print( cout );

        //Map spheres to the polydata
        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop1" << std::endl << vprDEBUG_FLUSH;
        glyph->SetSource( this->sphere->GetOutput() );
        glyph->SetInput( this->polydata );
        glyph->SetScaleModeToDataScalingOff();
        //glyph->Update();

        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop2" << std::endl << vprDEBUG_FLUSH;
        this->mapper->SetInput( this->glyph->GetOutput() );
        //this->mapper->Update();


        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop3" << std::endl << vprDEBUG_FLUSH;
        vtkActor* temp = vtkActor::New();
        temp->SetMapper( this->mapper );
        temp->GetProperty()->SetSpecularPower( 20.0f );
        temp->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
        geodes.push_back( new ves::xplorer::scenegraph::Geode() );
        geodes.back()->TranslateToGeode( temp );
        temp->Delete();

        //Make geodes from each polydata
        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: begin loop4" << std::endl << vprDEBUG_FLUSH;
        //this->_sequence->CreateGeodeVector( this->actor );

        // Reset polydata to its intial state and release all memory
        //polydata->Reset();
        this->polydata->Initialize();
        vprDEBUG( vesDBG, 2 ) << "|\t\tcfdAnimatedStreamlineCone:: end loop" << std::endl << vprDEBUG_FLUSH;
    }

    vprDEBUG( vesDBG, 1 ) << "|\t\tDeleting Point Array" << std::endl << vprDEBUG_FLUSH;
    for( size_t i = 0; i < maxNpts;  i++ )
    {
        pointsArray[ i ]->Delete();
    }

    delete [] pointsArray;
    vprDEBUG( vesDBG, 1 ) << "|\t\tDeleting Point Array" << std::endl << vprDEBUG_FLUSH;

    this->updateFlag = true;
    vprDEBUG( vesDBG, 1 ) << "|\tExiting cfdStreamers Update " << std::endl << vprDEBUG_FLUSH;
}
////////////////////////////////////////////////////////////////////////////////
#if 0
///This code needs to be updated to ves::open::xml::command
bool cfdAnimatedStreamlineCone::CheckCommandId( cfdCommandArray* commandArray )
{
    // This is here because Dr. K. has code in
    // cfdObjects that doesn't belong there
    bool flag = cfdObjects::CheckCommandId( commandArray );

    if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STREAMLINE_DIAMETER )
    {
        vprDEBUG( vesDBG, 0 ) << " STREAMLINE_DIAMETER\t"
        << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
        << std::endl << vprDEBUG_FLUSH;

        // diameter is obtained from gui, -100 < vectorScale < 100
        // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
        // convert range to -2.5 < x < 2.5, and compute the exponent...
        float range = 2.5f;
        int diameter = ( int )commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
        /*
              this->particleDiameter = 8.0f * (exp( diameter / ( 100.0 / range ) ) *
                               this->GetActiveDataSet()->GetLength()*0.001f);
        */
        // this is to convert 1 to 50 on the GUI  to approx from 1 to 28 pixels
        //vector arrows and seed points are in feet
//     this->particleDiameter =0.1f * exp(diameter*0.10f);
        this->particleDiameter = ( diameter + 5 ) * 0.01f;

        vprDEBUG( vesDBG, 1 ) << "\tNew Particle Diameter : "
        << this->particleDiameter << std::endl << vprDEBUG_FLUSH;
        return true;
    }
    else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == BACKWARD_INTEGRATION )
    {
        vprDEBUG( vesDBG, 2 ) << " BACKWARD_INTEGRATION"
        << std::endl << vprDEBUG_FLUSH;

        streamDir = cfdAnimatedStreamlineCone::BACKWARD;
        return true;
    }
    else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == FORWARD_INTEGRATION )
    {
        vprDEBUG( vesDBG, 2 ) << " FORWARD_INTEGRATION"
        << std::endl << vprDEBUG_FLUSH;

        streamDir = cfdAnimatedStreamlineCone::FORWARD;
        return true;
    }
    else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TWO_DIRECTION_INTEGRATION )
    {
        vprDEBUG( vesDBG, 2 ) << " 2_DIRECTION_INTEGRATION"
        << std::endl << vprDEBUG_FLUSH;

        streamDir = cfdAnimatedStreamlineCone::BOTH;
        return true;
    }
    else if( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STREAMLINE_CURSOR )
    {
        // diameter is obtained from gui, -100 < vectorScale < 100
        // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
        // convert range to -2.5 < x < 2.5, and compute the exponent...
        float range = 2.5f;
        int diameter = ( int )commandArray->GetCommandValue( cfdCommandArray::CFD_SC );

        /*
              particleDiameter = 8.0f * exp( diameter / ( 100.0 / range ) ) *
                               this->GetActiveDataSet()->GetLength()*0.001f;

        */
        // this is to convert 1 to 50 on the GUI  to approx from 1 to 28 pixels
        //vector arrows and seed points are in feet
//      this->particleDiameter =0.1f * exp(diameter*0.10f);
        this->particleDiameter = ( diameter + 5 ) * 0.01f;


        return true;
    }

    return flag;
}
#endif
////////////////////////////////////////////////////////////////////////////////
void cfdAnimatedStreamlineCone::UpdateCommand()
{
    cfdObjects::UpdateCommand();
    std::cerr << "doing nothing in cfdAnimatedStreamlineCone::UpdateCommand()" << std::endl;
}
////////////////////////////////////////////////////////////////////////////////
