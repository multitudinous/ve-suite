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
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef STAR_SURFACE_H
#define STAR_SURFACE_H

#include <vtkAppendFilter.h>
#include <vtkDecimatePro.h>
#include <vtkGeometryFilter.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkSmoothPolyDataFilter.h>
#include <vtkTriangleFilter.h>
//#include <vtkUnstructuredGrid.h>
//#include <vtkUnstructuredGridWriter.h>
//#include <vtkUnstructuredGrid.h>
#include <vtkDataSet.h>
#include <ves/xplorer/util/readWriteVtkThings.h>

using namespace VE_Util;

class surfaceVtkOutput
{
public:
  surfaceVtkOutput( ) { }
  ~surfaceVtkOutput( ) { }

  void writeSurface( vtkDataSet *unsGrid, float deciVal, char fname[] );

};

void surfaceVtkOutput::writeSurface( vtkDataSet *unsGrid, float deciVal, char fname[] )
{
  vtkGeometryFilter *cFilter = vtkGeometryFilter::New( );
     cFilter->SetInput( unsGrid );

  vtkTriangleFilter *tFilter = vtkTriangleFilter::New( );
     tFilter->SetInput( cFilter->GetOutput( ) );

  vtkDecimatePro *deci = vtkDecimatePro::New( );
     deci->SetInput( tFilter->GetOutput( ) );
     deci->SetTargetReduction( deciVal );
     deci->PreserveTopologyOn( );

  vtkSmoothPolyDataFilter *smoother = vtkSmoothPolyDataFilter::New( );
     smoother->SetInput( deci->GetOutput( ) );
     smoother->SetNumberOfIterations( 1 );

  vtkPolyDataNormals *cNormal = vtkPolyDataNormals::New( );
     cNormal->SetInput( smoother->GetOutput( ) );
  
  vtkAppendFilter *afilter = vtkAppendFilter::New();
     afilter->SetInput(cNormal->GetOutput( ) ); 

  //vtkPolyDataWriter *writer = vtkPolyDataWriter::New( );
/*    vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New( );
     writer->SetInput( afilter->GetOutput( ) );
     writer->SetFileName( fname );
     writer->SetFileTypeToBinary( );
     writer->Write( );
*/
      writeVtkThing( (vtkDataSet*)afilter->GetOutput(), fname, 1 );
      
     cFilter->Delete( );
     tFilter->Delete( );
     deci->Delete( );
     smoother->Delete( );
     cNormal->Delete( );
     //writer->Delete( );
}

#endif
