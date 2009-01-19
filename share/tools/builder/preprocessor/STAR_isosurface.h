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
#ifndef STAR_ISOSURFACE_H
#define STAR_ISOSURFACE_H

#include <vtkContourFilter.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <vtkDecimatePro.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkPolyDataNormals.h>
#include <vtkPolyDataWriter.h>
#include <sstream>
#include <iomanip>

#include <ves/xplorer/util/setScalarAndVector.h>

using namespace ves::xplorer::util;

class isosurfaceVtkOutput
{
public:
  isosurfaceVtkOutput() { }
  ~isosurfaceVtkOutput() { }

  void writeIsosurface( vtkDataSet *unsGrid, std::string postDataDir,
       int numContours = 20, float rangeStart = -0.1, float rangeEnd = -0.1 );

};

void isosurfaceVtkOutput::writeIsosurface( vtkDataSet *unsGrid,
        std::string postDataDir, int numContours, float rangeStart, float rangeEnd )
{
   ::activateScalar( unsGrid );

   // remove other point data from the isosurface...
   // unsigned long len = static_cast<unsigned long>(strlen( unsGrid->GetPointData()->GetScalars()->GetName() ));
   std::string scalarName;// = new char [len+1];
   scalarName.assign(  unsGrid->GetPointData()->GetScalars()->GetName() );//strcpy( scalarName, unsGrid->GetPointData()->GetScalars()->GetName() );
   //std::cout << "scalarName = " << scalarName << std::endl;

   int numPDArrays = unsGrid->GetPointData()->GetNumberOfArrays();
   //std::cout << "numPDArrays = " << numPDArrays << std::endl;

   int k = 0;
   for (int i=0; i < numPDArrays; i++)
   {
      if ( scalarName.compare(unsGrid->GetPointData()->GetArray(k)->GetName()) == 0 )//strcmp( unsGrid->GetPointData()->GetArray(k)->GetName(), scalarName ) )
      {
         //std::cout << "removing array " << unsGrid->GetPointData()->GetArray(k)->GetName() << std::endl;
         unsGrid->GetPointData()->RemoveArray( 
                            unsGrid->GetPointData()->GetArray(k)->GetName() );
      }
      else
         k++;
   }

   //delete [] scalarName;

   if ( rangeStart == rangeEnd )
   {
      double isoRange[2];
      unsGrid->GetPointData()->GetScalars()->GetRange( isoRange );
      rangeStart = (float)isoRange[0];
      rangeEnd = (float)isoRange[1];
   }

   //std::cout << "Min value of isosurface = " << rangeStart << std::endl;
   //std::cout << "Max value of isosurface = " << rangeEnd << std::endl;
  
   float dRange = ( rangeEnd - rangeStart ) / (float) numContours;

   //char*  isoFname;
   float value = rangeStart + dRange;

   for ( int i=0; i<numContours; i++ )
   {
      std::cout << "\tCreating isosurface at " << value << std::endl;

      vtkContourFilter *contour = vtkContourFilter::New();
         contour->SetInput( unsGrid );
         contour->SetValue( 0, value );
         //contour->Update();

      vtkDecimatePro *deci = vtkDecimatePro::New();
         deci->SetInput( contour->GetOutput() );
         deci->SetTargetReduction( 0.90 );
         deci->PreserveTopologyOn();

      vtkSmoothPolyDataFilter *smoother = vtkSmoothPolyDataFilter::New();
         smoother->SetInput( deci->GetOutput() );
         smoother->SetNumberOfIterations( 0 );   //was set to one

      vtkPolyDataNormals *normals = vtkPolyDataNormals::New();
         normals->SetInput( smoother->GetOutput() );

		std::ostringstream dirStringStream;
		dirStringStream << postDataDir << "/iso_" << std::setw(3) << ".vtk";

      vtkPolyDataWriter *isoWriter = vtkPolyDataWriter::New();
         isoWriter->SetInput( normals->GetOutput() );
         isoWriter->SetFileName( dirStringStream.str().c_str() );
         isoWriter->SetFileTypeToBinary();
         isoWriter->Write();

      contour->Delete();
      deci->Delete();
      smoother->Delete();
      normals->Delete();
      isoWriter->Delete();

      value += dRange;
   }
}
#endif
