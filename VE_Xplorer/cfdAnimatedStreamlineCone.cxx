/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdAnimatedStreamlineCone.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdAnimatedStreamlineCone.h"
#include "cfdDataSet.h"
#include "cfdEnum.h"
#include "cfdCommandArray.h"

#include <vtkPolyData.h>
#include <vtkActor.h>
#include <vtkSphereSource.h>
#include <vtkGlyph3D.h>
#include <vtkPolyDataWriter.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>
#include <vtkGenericCell.h>

#include <vpr/Util/Debug.h>

cfdAnimatedStreamlineCone::cfdAnimatedStreamlineCone( void )
{
   vprDEBUG(vprDBG_ALL,2) << "cfdAnimatedStreamlineCone constructor"
                          << std::endl << vprDEBUG_FLUSH;

   this->mapper   = vtkPolyDataMapper::New();
   //this->actor    = vtkActor::New();
   this->polydata = vtkPolyData::New();
   this->polyData = vtkPolyData::New();
   this->glyph    = vtkGlyph3D::New();
   this->sphere   = vtkSphereSource::New();

   //this->_sequence = new cfdTempAnimation();
   this->particleDiameter = 1.0f;
}

cfdAnimatedStreamlineCone::~cfdAnimatedStreamlineCone()
{
   vprDEBUG(vprDBG_ALL,2) << "cfdAnimatedStreamlineCone destructor"
                          << std::endl << vprDEBUG_FLUSH;

   this->mapper->Delete();
   //this->actor->Delete();
   this->polydata->Delete();
   this->polyData->Delete();
   this->glyph->Delete();
   this->sphere->Delete();
   
   //this->_sequence->ClearSequence();
   //delete this->_sequence;
   
   for ( unsigned int i = 0; i < actors.size(); ++i )
   {
      this->actors.at( i )->Delete();
   }
   this->actors.clear();
}

void cfdAnimatedStreamlineCone::SetPolyDataSource( vtkPolyData *input )
{
   this->polyData->DeepCopy( input );
}

void cfdAnimatedStreamlineCone::Update( void )
{
   vtkIdType cellId;        //vtkIdType
   vtkIdType npts;          //vtkIdType
   int i;
   vtkPoints * points;   
   vtkPoints **pointsArray;   
   double *x;
   int numberOfStreamLines;

   vprDEBUG(vprDBG_ALL, 1) 
      << "Number of Cells : " << this->polyData->GetNumberOfCells()
      << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 1) 
      << "Number of Lines : " << this->polyData->GetNumberOfLines()
      << std::endl << vprDEBUG_FLUSH;

   numberOfStreamLines = this->polyData->GetNumberOfLines();

   if ( numberOfStreamLines == 0 )
   {
      std::cout << "|   cfdAnimatedStreamlineCone::Update : Number of streamlines is 0 " << std::endl;
      return;
   }

   // Find the maximum number of points in one streamline
   int maxNpts = 0;
   int minNpts = 1000000;

   for ( cellId = 0; cellId < numberOfStreamLines; cellId += 2 )
   {
      points = this->polyData->GetCell( cellId )->GetPoints();
      npts = points->GetNumberOfPoints();
      points = this->polyData->GetCell( cellId + 1 )->GetPoints();
      npts += points->GetNumberOfPoints();
      vprDEBUG(vprDBG_ALL, 1) << " Number of points in cell " << cellId 
         << " = " << npts << std::endl << vprDEBUG_FLUSH;
      if ( maxNpts < npts )
         maxNpts = npts;
      
      if ( minNpts > npts )
         minNpts = npts;
   }

   // Define the points at each integration time step
   pointsArray = new vtkPoints*[ maxNpts ];
   for ( i = 0; i < maxNpts;  i++ )
   {
      pointsArray[ i ] = vtkPoints::New();
   }

   int forwardPoints;
   for ( cellId = 0; cellId < numberOfStreamLines; cellId += 2 )
   {
      // For forward integrated points
      points = this->polyData->GetCell( cellId )->GetPoints();
      forwardPoints = points->GetNumberOfPoints();
      vprDEBUG(vprDBG_ALL, 1) 
         << "Number of Forward points = " << forwardPoints
         << std::endl << vprDEBUG_FLUSH;      
      for ( i = 0; i < forwardPoints; i++ )
      {
         x = points->GetPoint( i );
         vprDEBUG(vprDBG_ALL, 3) 
            << "x[ " << i << " ] = " << x[ 0 ] << " : " 
            << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
         pointsArray[ i ]->InsertNextPoint( x );        
      }
      
      // For backward integrated points
      points = this->polyData->GetCell( cellId + 1 )->GetPoints();
      npts = points->GetNumberOfPoints();
      vprDEBUG(vprDBG_ALL, 1) << "Number of Backward points = " << npts
         << std::endl << vprDEBUG_FLUSH;
      for ( i = npts - 1; i >= 0; i-- )
      {
         x = points->GetPoint( i );
         vprDEBUG(vprDBG_ALL, 3)
            << " x[ " << i << " ] = " << x[ 0 ] << " : " 
            << x[ 1 ] << " : " << x[ 2 ] << std::endl << vprDEBUG_FLUSH;
         pointsArray[ ( npts - 1 ) - i + forwardPoints]->InsertNextPoint( x );
      }
   }

   this->sphere->SetRadius( this->particleDiameter );
   this->sphere->SetThetaResolution( 3 );
   this->sphere->SetPhiResolution( 3 );
   this->sphere->Update();

   vprDEBUG(vprDBG_ALL, 1) << "maxNpts = " << maxNpts << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 1) << "minNpts = " << minNpts << std::endl << vprDEBUG_FLUSH;
   int w = maxNpts;
   double decimalRatio = (double)w / 150.0;
   int ratio = (int)ceil( decimalRatio );

   for ( i = 0; i < w; i+=ratio )
   {
      //Make ploydata from the points
      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: begin loop " << i << std::endl << vprDEBUG_FLUSH;
      this->polydata->SetPoints( pointsArray[ i ] );
      //polydata->Update();
      //polydata->Print( cout );

      //Map spheres to the polydata
      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: begin loop1" << std::endl << vprDEBUG_FLUSH;
      this->glyph->SetSource( this->sphere->GetOutput() );
      this->glyph->SetInput( this->polydata );
      //glyph->Update();      

      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: begin loop2" << std::endl << vprDEBUG_FLUSH;
      this->mapper->SetInput( this->glyph->GetOutput() );
      this->mapper->Update();


      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: begin loop3" << std::endl << vprDEBUG_FLUSH;
      this->actors.push_back( vtkActor::New() );
      this->actors.back()->SetMapper( this->mapper );
      this->actors.back()->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );   
     
      //Make geodes from each polydata
      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: begin loop4" << std::endl << vprDEBUG_FLUSH;
      //this->_sequence->CreateGeodeVector( this->actor );

      // Reset polydata to its intial state and release all memory
      //polydata->Reset();
      this->polydata->Initialize();
      vprDEBUG(vprDBG_ALL, 2) << "\t cfdAnimatedStreamlineCone:: end loop" << std::endl << vprDEBUG_FLUSH;
   }
   
   /*vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
   writer->SetInput( input );
   writer->SetFileName( "outPut.vtk" );
   writer->SetFileTypeToASCII();
   writer->Write();
   writer->Delete();*/
   vprDEBUG(vprDBG_ALL, 1) << "Deleting Point Array" << std::endl << vprDEBUG_FLUSH;
   for ( i = 0; i < maxNpts;  i++ )
   {
      pointsArray[ i ]->Delete();
   }
   
   delete [] pointsArray;
   vprDEBUG(vprDBG_ALL, 1) << "Deleting Point Array" << std::endl << vprDEBUG_FLUSH;

   this->updateFlag = true;
   vprDEBUG(vprDBG_ALL, 1) << "|   Exiting cfdStreamers Update " << std::endl << vprDEBUG_FLUSH;
}

bool cfdAnimatedStreamlineCone::CheckCommandId( cfdCommandArray* commandArray )
{
   // This is here because Dr. K. has code in 
   // cfdObjects that doesn't belong there
   bool flag = cfdObjects::CheckCommandId( commandArray );

   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == STREAMLINE_DIAMETER )
   {
      vprDEBUG(vprDBG_ALL,0) << " STREAMLINE_DIAMETER\t" 
                              << commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) 
                              << std::endl << vprDEBUG_FLUSH;
         
      // diameter is obtained from gui, -100 < vectorScale < 100
      // we use a function y = exp(x), that has y(0) = 1 and y'(0) = 1
      // convert range to -2.5 < x < 2.5, and compute the exponent...
      float range = 2.5f;
      int diameter = commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      this->particleDiameter = 8.0f * (exp( diameter / ( 100.0 / range ) ) * 
                       this->GetActiveDataSet()->GetLength()*0.001f);

         vprDEBUG(vprDBG_ALL,1) << "\tNew Particle Diameter : " 
                             << this->particleDiameter << std::endl << vprDEBUG_FLUSH;
      return true;
   }

   return flag;
}

void cfdAnimatedStreamlineCone::UpdateCommand()
{
   cfdObjects::UpdateCommand();
   cerr << "doing nothing in cfdStreamers::UpdateCommand()" << endl;
}
