/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
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
 * File:          $RCSfile: addNormals2Poly.cpp,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <vtkDataSet.h>
#include <vtkPolyData.h>
#include <vtkPolyDataNormals.h>
#include <vtkGeometryFilter.h>

#include "fileIO.h"
#include "readWriteVtkThings.h"

int main( int argc, char *argv[] )
{    
   // If the command line contains an input vtk file name and an output file set them up.
   // Otherwise, get them from the user...
   char *inFileName = NULL;
   char *outFileName = NULL;
   fileIO::processCommandLineArgs( argc, argv, "add normals to", inFileName, outFileName );
   if ( ! inFileName ) return 1;

   vtkDataSet * dataset = readVtkThing( inFileName, 1 ); // "1" means print info to screen

   // convert vtkUnstructuredGrid to vtkPolyData    
   vtkGeometryFilter *gFilter = vtkGeometryFilter::New();
      gFilter->SetInput( dataset );

   vtkPolyDataNormals * pdWithNormals = vtkPolyDataNormals::New();
      pdWithNormals->SetInput( gFilter->GetOutput() );
      //Specify the angle that defines a sharp edge. If the difference in angle across neighboring
      //polygons is greater than this value, the shared edge is considered "sharp".    
      pdWithNormals->SetFeatureAngle( 60 );

   writeVtkThing( pdWithNormals->GetOutput(), outFileName );

   dataset->Delete();
   gFilter->Delete();
   pdWithNormals->Delete();
   delete [] inFileName;   inFileName = NULL;
   delete [] outFileName;  outFileName = NULL;

   return 0;
}

