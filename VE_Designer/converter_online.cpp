/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2003 by Iowa State University
 *
 * Original Authors:
 *   Thermal Systems Virtual Engineering Group 
 *   Headed by: Kenneth Mark Bryden, Ph.D.
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
 * File:          $RCSfile: converter.cpp,v $
 * Date modified: $Date: 2003/12/13 21:26:06 $
 * Version:       $Revision: 1.8 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>


#include <vtkPointData.h>
#include <vtkFloatArray.h> // this code requires VTK4
#include "converter_online.h"

void letUsersAddParamsToField_online( const int numParams, vtkFloatArray** data,
                               vtkPointData* pointData )
{  

      std::cout << "in letUsersAddParamsToField with numParams = " 
                << numParams << std::endl;

   for ( int i=0; i<numParams; i++ )
   {
         std::cout << "data[" << i << "]->GetNumberOfTuples() = " 
                   << data[i]->GetNumberOfTuples() << std::endl;

      // if there is just one scalar/vector then write it to field without
      // checking with user.  Otherwise, check with user if the scalar/vector
      // should be written to the flowdata.vtk file...
      // verbose sets wether or not just to skip the questions from being asked
      
         pointData->AddArray( data[i] );
   }

}

