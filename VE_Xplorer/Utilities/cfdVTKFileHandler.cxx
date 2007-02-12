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
#include "VE_Xplorer/Utilities/cfdVTKFileHandler.h"
#include <vtkDataSet.h>
#include <vtkDataObject.h>
#include <vtkGenericDataObjectReader.h>
#include <vtkXMLFileReadTester.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkXMLRectilinearGridReader.h>
#include <vtkXMLImageDataReader.h>
#include <vtkImageData.h>
#include <vtkPolyDataReader.h>
#include <vtkDataSetReader.h>
#include <vtkDataReader.h>
#include <vtkStructuredGrid.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkXMLPolyDataReader.h>
#include <vtkPolyData.h>
#include <vtkXMLDataSetWriter.h>
#include <vtkUnstructuredGridWriter.h>
#include <vtkStructuredGridWriter.h>
#include <vtkRectilinearGridWriter.h>
#include <vtkPolyDataWriter.h>
#include <iostream>
#include <vtkUnstructuredGridReader.h>

#ifdef VTK_CVS
#include <vtkXMLMultiGroupDataReader.h>
#include <vtkXMLHierarchicalDataReader.h>
#include <vtkXMLMultiBlockDataReader.h>
#include <vtkHierarchicalDataSet.h>
#include <vtkMultiBlockDataSet.h>
#endif
#include <fstream>

using namespace VE_Util;

//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler()
{
   _outFileType = CFD_XML;
   _outFileMode = CFD_BINARY;

   _xmlTester = 0;   
   _dataSet = 0;
}
/////////////////////////////////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler(const cfdVTKFileHandler& fh)
{
   _outFileType = fh._outFileType;
   _outFileMode = fh._outFileMode;

   _inFileName.assign( fh._inFileName );
   _outFileName.assign( fh._outFileName );

   _xmlTester = vtkXMLFileReadTester::New();
   _xmlTester = fh._xmlTester;
   _dataSet = fh._dataSet;
}
///////////////////////////////////////
//Destructor                         //
///////////////////////////////////////
cfdVTKFileHandler::~cfdVTKFileHandler()
{
   if(_xmlTester)
   {
      _xmlTester->Delete();
   }
   
   _inFileName.erase();
   _outFileName.erase();
   
}
////////////////////////////////////////////////////////////
void cfdVTKFileHandler::SetVTKOutFileType(OutFileType type)
{
   _outFileType = type;
}
/////////////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutFileWriteMode(OutFileMode mode)
{
   _outFileMode = mode;
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetInputFileName(std::string inFile)
{
   if( inFile.empty() )
      return;

   _inFileName.clear();
   _inFileName = inFile;
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutputFileName(std::string oFile)
{
   if( oFile.empty() )
      return;

   _outFileName.erase();
   _outFileName = oFile;
}
////////////////////////////////////////////////////////////////////
vtkDataObject* cfdVTKFileHandler::GetDataSetFromFile(std::string vtkFileName)
{
   std::cout<<"Loading: "<<vtkFileName<<std::endl;
   if ( vtkFileName.empty() )
   {
      return 0;
   }
   _inFileName = vtkFileName;

   if(!_xmlTester)
   {
      _xmlTester = vtkXMLFileReadTester::New();
   }
   _xmlTester->SetFileName(vtkFileName.c_str());

   std::cout<<"cfdVTKFileHandler::Checking file type...";
   if(_xmlTester->TestReadFile())
   {
      std::cout<<" XML ";
      std::cout<< _xmlTester->GetFileDataType()<<std::endl;
      //process xml file
      if(!strcmp(_xmlTester->GetFileDataType(),"UnstructuredGrid"))
      {
         _getXMLUGrid();
      }
      else if(!strcmp(_xmlTester->GetFileDataType(),"StructuredGrid"))
      {
         _getXMLSGrid();
      }
      else if(!strcmp(_xmlTester->GetFileDataType(),"RectilinearGrid"))
      {
         _getXMLRGrid();
      }
      else if(!strcmp(_xmlTester->GetFileDataType(),"PolyData"))
      {
         _getXMLPolyData();
      }
      else if ( !strcmp( _xmlTester->GetFileDataType(), "ImageData" ) )
      {
         GetXMLImageData();
      }
#ifdef VTK_CVS
	  else if ( !strcmp( _xmlTester->GetFileDataType(), "MultiBlockDataSet" ) )
      {
		  std::cout<<"MultiBlockDataset!!"<<std::endl;
		  _getXMLMultiGroupDataSet();
      }

	  else if ( !strcmp( _xmlTester->GetFileDataType(), "vtkHierarchicalDataSet" ) )
      {
		  _getXMLMultiGroupDataSet(false);	     
	  }
#endif
   }
   else
   {
      //this is a "classic" style vtk file
      _readClassicVTKFile();   
   }
   return _dataSet;
}
/////////////////////////////////////////////
void cfdVTKFileHandler::_readClassicVTKFile()
{
   if(!_inFileName.c_str())
      return;

   vtkGenericDataObjectReader* genericReader = vtkGenericDataObjectReader::New();
   genericReader->SetFileName( _inFileName.c_str() );
   genericReader->Update();
   
   {
      if ( genericReader->IsFileUnstructuredGrid())
      {
         std::cout<<"Unstructured Grid..."<<std::endl;
         _dataSet = vtkUnstructuredGrid::New();
	  }
	  else if ( genericReader->IsFileStructuredGrid())
      {
         std::cout<<"Structured Grid..."<<std::endl;
         _dataSet = vtkStructuredGrid::New();
	  }
	  else if ( genericReader->IsFileRectilinearGrid())
      {
         std::cout<<"Rectilinear Grid..."<<std::endl;
         _dataSet = vtkRectilinearGrid::New();
      }
	  else if ( genericReader->IsFilePolyData())
      {
         std::cout<<"PolyData..."<<std::endl;
         _dataSet = vtkPolyData::New();
      }
	  else
      {
         std::cerr <<"\nERROR - Unable to read this vtk file format"
                   << std::endl;
         return ;
      }
	  _dataSet->DeepCopy( genericReader->GetOutput());
      genericReader->Delete();
   }
}
#ifdef VTK_CVS
/////////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLMultiGroupDataSet(bool isMultiBlock)
{
   vtkXMLMultiGroupDataReader* mgdReader = 0;
   if(isMultiBlock)
   {
	   mgdReader = vtkXMLMultiBlockDataReader::New();
	   _dataSet = vtkMultiBlockDataSet::New();
   }
   else
   {
	   mgdReader= vtkXMLHierarchicalDataReader::New();
	   _dataSet = vtkHierarchicalDataSet::New();
   }
   mgdReader->SetFileName(_inFileName.c_str());
   mgdReader->Update();
   _dataSet->DeepCopy(mgdReader->GetOutput());
   mgdReader->Delete();
}
#endif
//////////////////////////////////////
void cfdVTKFileHandler::_getXMLUGrid()
{
   vtkXMLUnstructuredGridReader* ugReader
	   = vtkXMLUnstructuredGridReader::New();   
   ugReader->SetFileName(_inFileName.c_str());
   ugReader->Update();
   _dataSet = vtkUnstructuredGrid::New();
   //_dataSet->ShallowCopy(ugReader->GetOutput());
   _dataSet->DeepCopy(ugReader->GetOutput());
   ugReader->Delete();
}
//////////////////////////////////////
void cfdVTKFileHandler::_getXMLSGrid()
{
   vtkXMLStructuredGridReader* sgReader
	   = vtkXMLStructuredGridReader::New();   
   sgReader->SetFileName(_inFileName.c_str());
   sgReader->Update();
   _dataSet = vtkStructuredGrid::New();
   _dataSet->ShallowCopy(sgReader->GetOutput());
   sgReader->Delete();
}
/////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLRGrid()
{
   vtkXMLRectilinearGridReader* rgReader
	   = vtkXMLRectilinearGridReader::New();   
   rgReader->SetFileName(_inFileName.c_str());
   rgReader->Update();
   _dataSet = vtkRectilinearGrid::New();
   _dataSet->ShallowCopy(rgReader->GetOutput());
   rgReader->Delete();
}
////////////////////////////////////////////////
void cfdVTKFileHandler::_getXMLPolyData()
{
   vtkXMLPolyDataReader* pdReader
	   = vtkXMLPolyDataReader::New();   
   pdReader->SetFileName(_inFileName.c_str());
   pdReader->Update();
   _dataSet = vtkPolyData::New();
   _dataSet->ShallowCopy(pdReader->GetOutput());
   pdReader->Delete();
}
////////////////////////////////////////////////
void cfdVTKFileHandler::GetXMLImageData( void )
{
   std::cout<<"Reading image data..."<<std::endl;
   vtkXMLImageDataReader* reader = vtkXMLImageDataReader::New();
   reader->SetFileName( _inFileName.c_str() );
   reader->Update();
   _dataSet = vtkImageData::New();
   _dataSet->ShallowCopy( reader->GetOutput() );
   reader->Delete();
   std::cout<<"Finished Reading image data..."<<std::endl;
}
/////////////////////////////////////////////////////////////////////////////////
bool cfdVTKFileHandler::WriteDataSet(vtkDataObject* dataSet,std::string outFileName)
{
   if( outFileName.empty() )
      return false;

   if ( _outFileType == CFD_XML )
   {
      vtkXMLDataSetWriter* xmlWriter = vtkXMLDataSetWriter::New();
      xmlWriter->SetFileName(outFileName.c_str());
      xmlWriter->SetInput(dataSet);
      if ( _outFileMode == CFD_ASCII )
         xmlWriter->SetDataModeToAscii();
      
      if ( xmlWriter->Write())
      {
         xmlWriter->Delete();
         return true;
      }
      else
      {
         xmlWriter->Delete();
         return false;
      }
   }
   else
   {
      _writeClassicVTKFile(dataSet,outFileName,_outFileMode);
      return true;
   }
   return false;
}
////////////////////////////////////////////////////////////////////////
void cfdVTKFileHandler::_writeClassicVTKFile( vtkDataObject * vtkThing, 
                            std::string vtkFilename, int binaryFlag)
{
    if ( vtkThing == NULL )
   {
      std::cout << "Error: Can't write because vtkThing == NULL" << std::endl;
      return;
   }

   vtkThing->Update();

   std::cout << "Writing \"" << vtkFilename << "\", " << std::flush;

   if ( vtkThing->IsA("vtkUnstructuredGrid") )
   {
      std::cout << "a vtkUnstructuredGrid... " << std::flush;

      vtkUnstructuredGrid * uGrid = vtkUnstructuredGrid::SafeDownCast( vtkThing );
      if ( uGrid == NULL )
      {
         std::cout << "SafeDownCast to a vtkUnstructuredGrid failed";
      }

      vtkUnstructuredGridWriter *writer = vtkUnstructuredGridWriter::New();
      //writer->SetInput( (vtkUnstructuredGrid *)vtkThing );
      writer->SetInput( uGrid );
      writer->SetFileName( vtkFilename.c_str() );
      if (binaryFlag)
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else if ( vtkThing->IsA("vtkStructuredGrid") )
   {
      std::cout << "a vtkStructuredGrid... " << std::flush;

      vtkStructuredGrid * sGrid = vtkStructuredGrid::SafeDownCast( vtkThing );
      if ( sGrid == NULL )
      {
         std::cout << "SafeDownCast to a vtkStructuredGrid failed";
      }
      // Part of old vtk build may need to fix later
      //sGrid->BlankingOff();

      vtkStructuredGridWriter *writer = vtkStructuredGridWriter::New();
      writer->SetInput( sGrid );
      //writer->SetInput( (vtkStructuredGrid *)vtkThing );    //core dumped
      //writer->SetInput( vtkThing );                         //won't compile
      writer->SetFileName( vtkFilename.c_str() );
      if (binaryFlag) 
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
      //sGrid->Delete();
   }
   else if ( vtkThing->IsA("vtkRectilinearGrid") )
   {
      std::cout << "a vtkRectilinearGrid... " << std::flush;
      vtkRectilinearGridWriter *writer = vtkRectilinearGridWriter::New();
      writer->SetInput( (vtkRectilinearGrid*)vtkThing );
      writer->SetFileName( vtkFilename.c_str() );
      if (binaryFlag)
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else if ( vtkThing->IsA("vtkPolyData") )
   {
      std::cout << "a vtkPolyData... " << std::flush;
      vtkPolyDataWriter *writer = vtkPolyDataWriter::New();
      writer->SetInput( (vtkPolyData*)vtkThing );
      writer->SetFileName( vtkFilename.c_str() );
      if (binaryFlag) 
      {
         writer->SetFileTypeToBinary();
      }
      writer->Write();
      writer->Delete();
   }
   else
   {
      std::cerr <<"\nERROR - Unsupported vtk file format: file not written"
                << std::endl;
      return;
   }

   std::cout << "... done\n" << std::endl;
}
////////////////////////////////////////////////////////////////////////
cfdVTKFileHandler& cfdVTKFileHandler::operator=(const cfdVTKFileHandler& fh)
{
   if(this != &fh)
   {
      _outFileType = fh._outFileType;
      _outFileMode = fh._outFileMode;

      _inFileName.clear();
      _outFileName.clear();

      _inFileName.assign( fh._inFileName );
      _outFileName.assign( fh._outFileName );

      _xmlTester = fh._xmlTester;   
      _dataSet = fh._dataSet;
   }
   return *this;
}
