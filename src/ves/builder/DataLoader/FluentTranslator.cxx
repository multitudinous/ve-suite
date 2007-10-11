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
#include <ves/builder/DataLoader/FluentTranslator.h>
#include <vtkDataSet.h>
#include <vtkDataObject.h>
#include <vtkFLUENTReader.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>

#include <vtkMultiBlockDataSet.h>
#include <vtkXMLMultiGroupDataWriter.h>

#include <iostream>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
FluentTranslator::FluentTranslator()
{

   SetTranslateCallback( &fluentToVTK );
   SetPreTranslateCallback( &cmdParser );
}
/////////////////////////////////////////
FluentTranslator::~FluentTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void FluentTranslator::FluentPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   PreTranslateCallback::Preprocess( argc, argv, toVTK );
}
////////////////////////////////////////////////////////////////////////////////
void FluentTranslator::FluentTranslateCbk::Translate( vtkDataObject*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK )
{
   VE_Builder::FluentTranslator* FluentToVTK =
              dynamic_cast< VE_Builder::FluentTranslator* >( toVTK );
   if ( FluentToVTK )
   {
	   vtkFLUENTReader* reader = vtkFLUENTReader::New();
	   reader->SetFileName(FluentToVTK->GetFile(0).c_str() );
	   reader->Update();

      if ( !outputDataset )
      {
         outputDataset = vtkMultiBlockDataSet::New();
      }
      /*vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
      tmpDSet->DeepCopy( reader->GetOutput()*/ //);

      //get the info about the data in the data set
      //reader->GetOutput()
      //GetNumberOfGroups ()
      //GetNumberOfDataSets( );
      //GetDataSet( ) 
      /*if ( dynamic_cast< vtkDataSet* >( reader->GetOutput()->GetDataSet( 0, 0 ) )->GetPointData()->GetNumberOfArrays() == 0 )
      {
         //std::cout<<"Warning!!!"<<std::endl;
         //std::cout<<"No point data found!"<<std::endl;
         //std::cout<<"Attempting to convert cell data to point data."<<std::endl;

         vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();      
         dataConvertCellToPoint->SetInput(reader->GetOutput());
         dataConvertCellToPoint->PassCellDataOff();
         dataConvertCellToPoint->Update();
         outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
         reader->Delete();
         dataConvertCellToPoint->Delete();
      }
      else*/
      {
         outputDataset->DeepCopy(reader->GetOutput());
         reader->Delete();
      }
      outputDataset->Update();
      
      /*vtkXMLMultiGroupDataWriter* writer = vtkXMLMultiGroupDataWriter::New();
      writer->SetInput( reader->GetOutput() );
      writer->SetWriteMetaFile( 1 );
      writer->SetFileName( "test_multigrou.vtu" );
      writer->Write();*/
      //tmpDSet->Delete();
   }
}
////////////////////////////////////////////////////////////////////////////////
void FluentTranslator::DisplayHelp( void )
{
   std::cout << "|\tFluent Translator Usage:" << std::endl
               << "\t -singleFile <filename_to_load> -o <output_dir> "
               << "-outFileName <output_filename> -loader cas -w file" << std::endl;
}
