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
 * File:          $RCSfile: readWriteVtkThings.h,v $
 * Date modified: $Date: 2004-05-18 15:44:18 -0500 (Tue, 18 May 2004) $
 * Version:       $Rev: 382 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "enSightGoldReader.h"

#include <iostream>

#include <vtkEnSightGoldReader.h>          // will open any ensight file
#include <vtkCellDataToPointData.h>
#include <vtkUnstructuredGrid.h>

using namespace std;

enSightGoldReader::enSightGoldReader( void )
{
   reader = NULL;
   cell2point = NULL;
}

enSightGoldReader::~enSightGoldReader( void )
{
   if ( reader != NULL )
      reader->Delete();

   if ( cell2point != NULL )
      cell2point->Delete();
}

vtkUnstructuredGrid* enSightGoldReader::GetUnstructuredGrid( char* caseFileName, int debug )
{
   reader = vtkEnSightGoldReader::New();
   reader->SetCaseFileName( caseFileName );
   reader->Update();
   
   // Convert vtkEnSightGoldReader cell centered data to point data
   cell2point = vtkCellDataToPointData::New();
   cell2point->SetInput( reader->GetOutput() );
   cell2point->PassCellDataOff();
   cell2point->Update();
   
   // Must do this so that we can clean up properly in the destructor
   vtkUnstructuredGrid *finalUGrid = vtkUnstructuredGrid::New();
   finalUGrid->DeepCopy( cell2point->GetUnstructuredGridOutput() );

   return finalUGrid;
}
