/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: readWriteVtkThings.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdlib>

#include "VE_Xplorer/readWriteVtkThings.h"

#include <vtkDataSet.h>
#include <vtkDataSetReader.h>
//#include <vtkPointData.h>
#include <vtkPolyData.h>
#include <vtkPolyDataWriter.h>
#include <vtkRectilinearGrid.h>
#include <vtkRectilinearGridWriter.h>
#include <vtkStructuredGrid.h>
#include <vtkStructuredGridWriter.h>
#include <vtkUnstructuredGrid.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkXMLUnstructuredGridReader.h>
#include "VE_Xplorer/cfdVTKFileHandler.h"

using namespace VE_Util;

void VE_Util::printWhatItIs( vtkDataSet * dataSet )
{
   if ( dataSet == NULL )
   {
      std::cout << "\tdataSet == NULL" << std::endl;
      return;
   }
   if ( dataSet->IsA("vtkDataSet") )          
      std::cout << "\tIsA(\"vtkDataSet\")" << std::endl;
   if ( dataSet->IsA("vtkPointSet") )         
      std::cout << "\tIsA(\"vtkPointSet\")" << std::endl;
   if ( dataSet->IsA("vtkUnstructuredGrid") ) 
      std::cout << "\tIsA(\"vtkUnstructuredGrid\")" << std::endl;
   if ( dataSet->IsA("vtkStructuredGrid") )   
      std::cout << "\tIsA(\"vtkStructuredGrid\")"<< std::endl;
   if ( dataSet->IsA("vtkPolyData") )         
      std::cout << "\tIsA(\"vtkPolyData\")"<< std::endl;
   if ( dataSet->IsA("vtkRectilinearGrid") )  
      std::cout << "\tIsA(\"vtkRectilinearGrid\")" << std::endl;
   //std::cout << "GetDataObjectType() = " << dataSet->GetDataObjectType() << std::endl;
}

void VE_Util::printBounds( double bounds[6] )
{
   std::cout << "Geometry bounding box information..." << std::endl;
   std::cout << "\tx-min = \t" << bounds[0]
             << "\tx-max = \t" << bounds[1] << std::endl;
   std::cout << "\ty-min = \t" << bounds[2] 
             << "\ty-max = \t" << bounds[3] << std::endl;
   std::cout << "\tz-min = \t" << bounds[4] 
             << "\tz-max = \t" << bounds[5] << std::endl;
}

vtkDataSet * VE_Util::readVtkThing( std::string vtkFilename, int printFlag )
{
   cfdVTKFileHandler fileReader;
   vtkDataSet* temp = fileReader.GetDataSetFromFile(vtkFilename.c_str());
   if ( printFlag )
   {
      double bounds[6];
      temp->GetBounds( bounds );
      printBounds( bounds );
      VE_Util::printWhatItIs( temp );
   }
   return temp;
}

bool VE_Util::writeVtkThing( vtkDataSet * vtkThing, std::string vtkFilename, int binaryFlag )
{
   cfdVTKFileHandler fileWriter;
   if(!binaryFlag) 
      fileWriter.SetOutFileWriteMode(cfdVTKFileHandler::CFD_ASCII);
   return fileWriter.WriteDataSet(vtkThing,vtkFilename);
}

