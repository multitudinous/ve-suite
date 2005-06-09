#include "cfdVTKFileHandler.h"
#include <vtkDataSet.h>
#include <vtkXMLFileReadTester.h>
#include <vtkXMLUnstructuredGridReader.h>
#include <vtkXMLStructuredGridReader.h>
#include <vtkXMLRectilinearGridReader.h>
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

//////////////////////////////////////
//Constructors                      //
//////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler()
{
   _outFileType = CFD_XML;
   _outFileMode = CFD_BINARY;

   _inFileName = 0;
   _outFileName = 0;
   _xmlTester = 0;   
   _dataSet = 0;
}
/////////////////////////////////////////////////////////////////
cfdVTKFileHandler::cfdVTKFileHandler(const cfdVTKFileHandler& fh)
{
   _outFileType = fh._outFileType;
   _outFileMode = fh._outFileMode;

   _inFileName = new char[strlen(fh._inFileName)+1];
   _outFileName = new char[strlen(fh._outFileName)+1];

   strcpy(_inFileName,fh._inFileName);
   strcpy(_outFileName,fh._outFileName);

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
   
   if(_inFileName){
      delete []_inFileName;
      _inFileName = 0;
   }
   if(_outFileName){
      delete []_outFileName;
      _outFileName = 0;
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
void cfdVTKFileHandler::SetInputFileName(char* inFile)
{
   if(!inFile)
      return;

   if(_inFileName){
      delete [] _inFileName;
      _inFileName = 0;
   }

   _inFileName = new char[strlen(inFile)+1];

   strcpy(_inFileName, inFile);
}
//////////////////////////////////////////////////////
void cfdVTKFileHandler::SetOutputFileName(char* oFile)
{
   if(!oFile)
      return;

   if(_outFileName){
      delete [] _outFileName;
      _outFileName = 0;
   }

   _outFileName = new char[strlen(oFile)+1];

   strcpy(_outFileName, oFile);
}
////////////////////////////////////////////////////////////////////
vtkDataSet* cfdVTKFileHandler::GetDataSetFromFile(char* vtkFileName)
{
   std::cout<<"Loading: "<<vtkFileName<<std::endl;
   if(!vtkFileName)
      return 0;
   SetInputFileName(vtkFileName);
   std::cout<<"Creating xml tester..."<<std::endl;
   if(!_xmlTester){
      _xmlTester = vtkXMLFileReadTester::New();
   }
   _xmlTester->SetFileName(vtkFileName);

   std::cout<<"Checking file type..."<<std::endl;
   if(_xmlTester->TestReadFile()){
      std::cout<<"XML VTK file"<<std::endl;
      std::cout<<"File type: "<<_xmlTester->GetFileDataType()<<std::endl;
      //process xml file
      if(!strcmp(_xmlTester->GetFileDataType(),"UnstructuredGrid")){
         std::cout<<"============Unstructured grid==========="<<std::endl;
         _getXMLUGrid();
      }else if(!strcmp(_xmlTester->GetFileDataType(),"StructuredGrid")){
         std::cout<<"============Structured grid==========="<<std::endl;
         _getXMLSGrid();
      }else if(!strcmp(_xmlTester->GetFileDataType(),"RectilinearGrid")){
         std::cout<<"============Rectilinear grid==========="<<std::endl;
         _getXMLRGrid();
      }else if(!strcmp(_xmlTester->GetFileDataType(),"PolyData")){
         _getXMLPolyData();
      }
   }else{
      //this is a "classic" style vtk file
      _readClassicVTKFile();   
   }
   return _dataSet;
}
/////////////////////////////////////////////
void cfdVTKFileHandler::_readClassicVTKFile()
{
   if(!_inFileName)
      return;

   vtkDataSetReader *genericReader = vtkDataSetReader::New();
   genericReader->SetFileName( _inFileName );
   _dataSet = genericReader->GetOutput();
   if (!_dataSet)
   {
      std::cout << "vtkDataSetReader failed, will try the "
                << "vtkXMLUnstructuredGridReader" << std::endl;
      genericReader->Delete();

      // try with the vtkXMLUnstructuredGridReader...
      vtkXMLUnstructuredGridReader *ugr = vtkXMLUnstructuredGridReader::New();
      ugr->SetFileName( _inFileName );
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

         _dataSet = vtkUnstructuredGrid::New();
         _dataSet->ShallowCopy( genericReader->GetUnstructuredGridOutput() );
         genericReader->Delete();
      }
      else if ( dataObjectType == VTK_STRUCTURED_GRID )
      {
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

         _dataSet = vtkRectilinearGrid::New();
         _dataSet->ShallowCopy( genericReader->GetRectilinearGridOutput() );
         genericReader->Delete();
      }
      else if ( dataObjectType == VTK_POLY_DATA )
      {

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
   ugReader->SetFileName(_inFileName);
   ugReader->Update();
   _dataSet = vtkUnstructuredGrid::New();
   _dataSet->ShallowCopy(ugReader->GetOutput());
   ugReader->Delete();
}
//////////////////////////////////////
void cfdVTKFileHandler::_getXMLSGrid()
{
   vtkXMLStructuredGridReader* sgReader
	   = vtkXMLStructuredGridReader::New();   
   sgReader->SetFileName(_inFileName);
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
   rgReader->SetFileName(_inFileName);
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
   pdReader->SetFileName(_inFileName);
   pdReader->Update();
   _dataSet = vtkPolyData::New();
   _dataSet->ShallowCopy(pdReader->GetOutput());
   pdReader->Delete();
}
///////////////////////////////////////////////////////////////////////////
bool cfdVTKFileHandler::WriteDataSet(vtkDataSet* dataSet,char* outFileName)
{
   if(!outFileName)
      return false;

   if(_outFileType == CFD_XML){
      vtkXMLDataSetWriter* xmlWriter = vtkXMLDataSetWriter::New();
      xmlWriter->SetFileName(outFileName);
      xmlWriter->SetInput(dataSet);
      if(xmlWriter->Write()){
         return true;
      }else{
         return false;
      }
   }else{
      _writeClassicVTKFile(dataSet,outFileName,_outFileMode);
      return true;
   }
   return false;
}
////////////////////////////////////////////////////////////////////////
void cfdVTKFileHandler::_writeClassicVTKFile( vtkDataSet * vtkThing, 
                            char * vtkFilename, int binaryFlag)
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
      writer->SetFileName( vtkFilename );
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
      writer->SetFileName( vtkFilename );
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
      writer->SetFileName( vtkFilename );
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
      writer->SetFileName( vtkFilename );
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

      if(_inFileName){
         delete [] _inFileName;
	       _inFileName = 0;
      }
      if(_outFileName){
         delete [] _outFileName;
	       _outFileName = 0;
      }
      _inFileName = new char[strlen(fh._inFileName)+1];
      _outFileName = new char[strlen(fh._outFileName)+1];

      strcpy(_inFileName,fh._inFileName);
      strcpy(_outFileName,fh._outFileName);

      _xmlTester = fh._xmlTester;   
      _dataSet = fh._dataSet;
   }
   return *this;
}
