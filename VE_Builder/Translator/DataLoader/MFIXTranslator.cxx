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
 * File:          $RCSfile: filename,v $
 * Date modified: $Date: date $
 * Version:       $Rev: 999999 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Builder/Translator/DataLoader/MFIXTranslator.h"
#include <vtkDataSet.h>
#include <vtkMFIXReader.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
MFIXTranslator::MFIXTranslator()
{

   SetTranslateCallback( &mfixToVTK );
   SetPreTranslateCallback( &cmdParser );
}
/////////////////////////////////////////
MFIXTranslator::~MFIXTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void MFIXTranslator::MFIXPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void MFIXTranslator::MFIXTranslateCbk::Translate( vtkDataSet*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK )
{
   VE_Builder::MFIXTranslator* MFIXToVTK =
              dynamic_cast< VE_Builder::MFIXTranslator* >( toVTK );
   if ( MFIXToVTK )
   {
	   vtkMFIXReader *reader = vtkMFIXReader::New();
	   reader->SetFileName( MFIXToVTK->GetFile(0).c_str() );
	   reader->Update();
	   int TimeStepRange[2]; 
	   reader->GetTimeStepRange( TimeStepRange );
	   //cout << "Number of Timesteps = " << TimeStepRange[0] << ", "<< TimeStepRange[1] << endl;
	   reader->SetTimeStep( TimeStepRange[0] );

      if ( !outputDataset )
      {
         outputDataset = vtkUnstructuredGrid::New();
      }
      vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
      tmpDSet->DeepCopy( reader->GetOutput() );

      //get the info about the data in the data set
      if ( tmpDSet->GetPointData()->GetNumberOfArrays() == 0 )
      {
         //std::cout<<"Warning!!!"<<std::endl;
         //std::cout<<"No point data found!"<<std::endl;
         //std::cout<<"Attempting to convert cell data to point data."<<std::endl;

         vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();      
         dataConvertCellToPoint->SetInput(tmpDSet);
         dataConvertCellToPoint->PassCellDataOff();
         dataConvertCellToPoint->Update();
         outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
         dataConvertCellToPoint->Delete();
      }
      else
      {
         outputDataset->DeepCopy(tmpDSet);
      }
      outputDataset->Update();
      reader->Delete();
      tmpDSet->Delete();
   }
}
 
