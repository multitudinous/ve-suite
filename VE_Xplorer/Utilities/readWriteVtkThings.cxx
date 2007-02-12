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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <iostream>
#include <cstdlib>

#include "VE_Xplorer/Utilities/readWriteVtkThings.h"

#include <vtkDataSet.h>
#include <vtkDataObject.h>
#include <vtkDataSetReader.h>
#include <vtkInformationStringKey.h>
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
#include "VE_Xplorer/Utilities/cfdVTKFileHandler.h"
#ifdef VTK_CVS
#include <vtkMultiGroupDataSet.h>
#endif

using namespace VE_Util;
///////////////////////////////////////////////////////////////
void VE_Util::printWhatItIs( vtkDataObject * dataSet )
{
   if ( dataSet == NULL )
   {
      std::cout << "\tdataSet == NULL" << std::endl;
      return;
   }
   std::cout<<dataSet->GetClassName()<<std::endl;
}
///////////////////////////////////////////////////////
void VE_Util::printBounds( vtkDataObject* dataObject)//double bounds[6] )
{
   double bounds[6];
   std::cout << "Geometry bounding box information..." << std::endl;
#ifdef VTK_CVS
   if(dataObject->IsA("vtkMultiGroupDataSet"))
   {
	  try
	  {
	     vtkMultiGroupDataSet* mgd = dynamic_cast<vtkMultiGroupDataSet*>(dataObject);
		 unsigned int nGroups = mgd->GetNumberOfGroups();
		 unsigned int nDatasetsInGroup = 0;
		 for(unsigned int i = 0; i < nGroups; i++)
		 {
			 std::cout<<"Group: "<<i<<std::endl;
			 nDatasetsInGroup = mgd->GetNumberOfDataSets(i);
			 for(unsigned int j = 0; j < nDatasetsInGroup; j++)
			 {
				 std::cout<<"Dataset: "<<j<<std::endl;
				 dynamic_cast<vtkDataSet*>(mgd->GetDataSet(i,j))->GetBounds(bounds);
			 }
		 }
	  }
	  catch(...)
	  {
		  std::cout<<"Invalid Dataset: "<<dataObject->GetClassName()<<std::endl;
	  }
   }
   else
#endif
   {
      dynamic_cast<vtkDataSet*>(dataObject)->GetBounds(bounds);
	   //ProcessScalarRangeInfo(dataObject);
   }
   //dataObject->Delete();
	/*
   std::cout << "Geometry bounding box information..." << std::endl;*/
   std::cout << "\tx-min = \t" << bounds[0]
             << "\tx-max = \t" << bounds[1] << std::endl;
   std::cout << "\ty-min = \t" << bounds[2] 
             << "\ty-max = \t" << bounds[3] << std::endl;
   std::cout << "\tz-min = \t" << bounds[4] 
             << "\tz-max = \t" << bounds[5] << std::endl;
			 
}
/////////////////////////////////////////////////////////////////////////////
vtkDataObject* VE_Util::readVtkThing( std::string vtkFilename, int printFlag )
{
   cfdVTKFileHandler fileReader;
   vtkDataObject* temp = fileReader.GetDataSetFromFile(vtkFilename);
   temp->Update();
   if ( printFlag )
   {
	  printBounds( temp );
      VE_Util::printWhatItIs( temp );
   }
   return temp;
}

bool VE_Util::writeVtkThing( vtkDataObject* vtkThing, std::string vtkFilename, int binaryFlag )
{
   cfdVTKFileHandler fileWriter;
   if(!binaryFlag) 
      fileWriter.SetOutFileWriteMode(cfdVTKFileHandler::CFD_ASCII);
   return fileWriter.WriteDataSet(vtkThing,vtkFilename);
}

