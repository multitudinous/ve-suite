#include "VE_Xplorer/cfdVTKFileHandler.h"
#include <vtkDataSet.h>
#include <vtkXMLFileReadTester.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkXMLRectilinearGridReader.h>
#include <vtkXMLImageDataReader.h>
#include <vtkImageData.h>
#include <vtkPolyDataReader.h>
#include <vtkDataSetReader.h>
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

using namespace VE_Util;

//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler()
{
   _outFileType = CFD_XML;
   _outFileMode = CFD_BINARY;

   //_inFileName = 0;
   //_outFileName = 0;
   _xmlTester = 0;   
   _dataSet = 0;
}
/////////////////////////////////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler(const cfdVTKFileHandler& fh)
{
   _outFileType = fh._outFileType;
   _outFileMode = fh._outFileMode;

   //_inFileName = new char[strlen(fh._inFileName)+1];
   //_outFileName = new char[strlen(fh._outFileName)+1];

   //strcpy(_inFileName,fh._inFileName);
   //strcpy(_outFileName,fh._outFileName);
   std::string _inFileName;
   std::string _outFileName;

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
   if(_xmlTester){
      _xmlTester->Delete();
   }
   
   if(_inFileName.c_str()){
      //delete []_inFileName;
      //_inFileName = 0;
      _inFileName.erase();
      
   }
   if(_outFileName.c_str()){
      //delete []_outFileName;
      //_outFileName = 0;
      _outFileName.erase();
   }
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

   if(_inFileName.c_str()){
      //delete [] _inFileName;
      //_inFileName = 0;
      _inFileName.erase();
   }

   //_inFileName = new char[strlen(inFile)+1];
   //strcpy(_inFileName, inFile);
   _inFileName.assign( inFile );
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutputFileName(std::string oFile)
{
   if( oFile.empty() )
      return;

   if(_outFileName.c_str()){
      //delete [] _outFileName;
      //_outFileName = 0;
      _outFileName.erase();
   }

   //_outFileName = new char[strlen(oFile)+1];
   //strcpy(_outFileName, oFile);
   _outFileName.assign( oFile );
}
////////////////////////////////////////////////////////////////////
vtkDataSet* cfdVTKFileHandler::GetDataSetFromFile(std::string vtkFileName)
{
   std::cout<<"Loading: "<<vtkFileName<<std::endl;
   if ( vtkFileName.empty() )
      return 0;
   //SetInputFileName(vtkFileName);
   _inFileName = vtkFileName;

   if(!_xmlTester){
      _xmlTester = vtkXMLFileReadTester::New();
   }
   _xmlTester->SetFileName(vtkFileName.c_str());

   std::cout<<"cfdVTKFileHandler::Checking file type...";
   if(_xmlTester->TestReadFile()){
      std::cout<<" XML ";
      std::cout<< _xmlTester->GetFileDataType()<<std::endl;
      //process xml file
      if(!strcmp(_xmlTester->GetFileDataType(),"UnstructuredGrid")){
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

   vtkDataSetReader *genericReader = vtkDataSetReader::New();
   genericReader->SetFileName( _inFileName.c_str() );
   _dataSet = genericReader->GetOutput();
   if (!_dataSet)
   {
      std::cout << "vtkDataSetReader failed, will try the "
                << "vtkXMLUnstructuredGridReader" << std::endl;
      genericReader->Delete();

      // try with the vtkXMLUnstructuredGridReader...
      vtkXMLUnstructuredGridReader *ugr = vtkXMLUnstructuredGridReader::New();
      ugr->SetFileName( _inFileName.c_str() );
      ugr->Update();
      _dataSet = vtkUnstructuredGrid::New();
      _dataSet->DeepCopy( ugr->GetOutput() );
      ugr->Delete();
   }
   else
   {
      int dataObjectType = genericReader->GetOutput()->GetDataObjectType();
      if ( dataObjectType == VTK_UNSTRUCTURED_GRID )
      {
         std::cout<<"Unstructured Grid..."<<std::endl;
         _dataSet = vtkUnstructuredGrid::New();
         _dataSet->ShallowCopy( genericReader->GetUnstructuredGridOutput() );
         genericReader->Delete();
      }
      else if ( dataObjectType == VTK_STRUCTURED_GRID )
      {
         std::cout<<"Structured Grid..."<<std::endl;
         // Because of a vtk BUG involving structured grid deep copy,
         // the follow code is not working...
         /*_dataSet = vtkStructuredGrid::New();
         _dataSet->DeepCopy( genericReader->GetUnstructuredGridOutput() );
         genericReader->Delete();
         */
	      _dataSet = genericReader->GetStructuredGridOutput();
      }
      else if ( dataObjectType == VTK_RECTILINEAR_GRID )
      {
         std::cout<<"Rectilinear Grid..."<<std::endl;
         _dataSet = vtkRectilinearGrid::New();
         _dataSet->ShallowCopy( genericReader->GetRectilinearGridOutput() );
         genericReader->Delete();
      }
      else if ( dataObjectType == VTK_POLY_DATA )
      {
         std::cout<<"PolyData..."<<std::endl;
         _dataSet = vtkPolyData::New();
         _dataSet->ShallowCopy( genericReader->GetPolyDataOutput() );
         genericReader->Delete();
      }
      else
      {
         std::cerr <<"\nERROR - Unable to read this vtk file format"
                   << std::endl;
         return ;
      }
   }
}
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
   vtkXMLImageDataReader* reader = vtkXMLImageDataReader::New();
   reader->SetFileName( _inFileName.c_str() );
   reader->Update();
   _dataSet = vtkImageData::New();
   _dataSet->ShallowCopy( reader->GetOutput() );
   reader->Delete();
}
/////////////////////////////////////////////////////////////////////////////////
bool cfdVTKFileHandler::WriteDataSet(vtkDataSet* dataSet,std::string outFileName)
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
void cfdVTKFileHandler::_writeClassicVTKFile( vtkDataSet * vtkThing, 
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
   if(this != &fh){
      _outFileType = fh._outFileType;
      _outFileMode = fh._outFileMode;

      if(_inFileName.c_str()){
         //delete [] _inFileName;
	      // _inFileName = 0;
         _inFileName.erase();
      }
      if(_outFileName.c_str()){
         //delete [] _outFileName;
	      // _outFileName = 0;
         _outFileName.erase();
      }
      //_inFileName = new char[strlen(fh._inFileName)+1];
      //_outFileName = new char[strlen(fh._outFileName)+1];

      //strcpy(_inFileName,fh._inFileName);
      //strcpy(_outFileName,fh._outFileName);

      _inFileName.assign( fh._inFileName );
      _outFileName.assign( fh._outFileName );

      _xmlTester = fh._xmlTester;   
      _dataSet = fh._dataSet;
   }
   return *this;
}
