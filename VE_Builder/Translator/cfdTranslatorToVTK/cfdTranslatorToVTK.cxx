#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"
#include <bitset>
#include <sstream>
#include <vtkDataset.h>
#include "VE_Xplorer/readWriteVtkThings.h"
using namespace VE_Builder;
////////////////////////////////////////
//Constructor                         //
////////////////////////////////////////
cfdTranslatorToVTK::cfdTranslatorToVTK()
{
   _nFoundFiles = 0;

   _fileExtension = ".*";
   _inputDir = "./";
   _outputDir = "./";

   _preTCbk = 0;
   _postTCbk = 0;
   _translateCbk = 0;

   _outputDataset = 0;
}
/////////////////////////////////////////
cfdTranslatorToVTK::~cfdTranslatorToVTK()
{
   if(_outputDataset)
   {
      _outputDataset->Delete();
   }
  
   _outfileNames.clear();
   _infileNames.clear();
}
////////////////////////////////////////////////////////////////////   
void cfdTranslatorToVTK::SetFileExtension(std::string fileExtension)
{
   _fileExtension = fileExtension;
}
/////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::SetInputDirectory(std::string inDir)
{
   _inputDir = inDir;
}
///////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::SetOutputDirectory(std::string outDir)
{
   _outputDir = outDir;
}
//////////////////////////////////////////////////////////////////////////////   
void cfdTranslatorToVTK::SetPreTranslateCallback(PreTranslateCallback* preTCbk)
{
   _preTCbk = preTCbk;
}
/////////////////////////////////////////////////////////////////////////////////   
void cfdTranslatorToVTK::SetPostTranslateCallback(PostTranslateCallback* postTCbk)
{
   _postTCbk = postTCbk;
}
/////////////////////////////////////////////////////////////////////   
void cfdTranslatorToVTK::SetTranslateCallback(TranslateCallback* tCbk)
{
   _translateCbk = tCbk;
}
////////////////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::SetNumberOfFoundFiles(unsigned int nFilesFound)
{
   _nFoundFiles = nFilesFound;
}
////////////////////////////////////////////////////////////////
std::string cfdTranslatorToVTK::GetFile(unsigned int fileNumber)
{
   return _infileNames.at(fileNumber);
}
//////////////////////////////////////////////////   
std::string cfdTranslatorToVTK::GetFileExtension()
{
   return _fileExtension;
}
//////////////////////////////////////////////////   
std::string cfdTranslatorToVTK::GetInputDirectory()
{
   return _inputDir;
}
////////////////////////////////////////////////////   
std::string cfdTranslatorToVTK::GetOutputDirectory()
{
   return _outputDir;
}
////////////////////////////////////////////////////////
unsigned int cfdTranslatorToVTK::GetNumberOfFoundFiles()
{
   return _nFoundFiles;
}
//////////////////////////////////////////////////////////////
bool cfdTranslatorToVTK::TranslateToVTK(int argc, char** argv)
{
   //this may be modified later to handle multiple files
   if(_preTCbk)
   {
      _preTCbk->Preprocess(argc,argv,this);
   }
   //just in case the Preprocess didn't set this
   if(!_nFoundFiles && _infileNames.size())
   {
      _nFoundFiles = static_cast< int >( _infileNames.size() );
   }

   std::bitset<16> status;

   for(unsigned int i = 0; i < _nFoundFiles; i++)
   {
      if(_translateCbk)
      {
         _translateCbk->Translate(_outputDataset,this);
      }
      if(_postTCbk)
      {
         _postTCbk->PostProcess(this);
      }
      status.set(i,_writeToVTK(i));
   }
   for(unsigned int i = 0; i < status.size(); i++)
   {
      if(!status.test(i))
      {
        return false;
      }
   }
   return true;
}
//////////////////////////////////////////////////////////
bool cfdTranslatorToVTK::_writeToVTK(unsigned int fileNum)
{
   if(_outputDataset)
   {
      if(_outfileNames.empty())
      {
         std::stringstream tempName;
         tempName<<_outputDir<<"/flowdata_"<<fileNum<<".vtk"<<"\0";
         _outfileNames.push_back(tempName.str());
      }
      VE_Util::writeVtkThing(_outputDataset, 
                          (char*)_outfileNames.at(fileNum).c_str(),0);
      return true;
   }else{
      std::cout<<"Invalid output vtk dataset!!!"<<std::endl;
      std::cout<<"cfdTranslatorToVTK::_writeToVTK"<<std::endl;
      return false;
   }
}
////////////////////////////////////////////////////////////////////////////////////
//This is the default behavior to look for the input and output directory from the//
//command line.                                                                   //
////////////////////////////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::PreTranslateCallback::Preprocess(int argc,char** argv,
                                                    cfdTranslatorToVTK* toVTK)
{
   if(toVTK)
   {
      std::string inDir;
      if(_extractOptionFromCmdLine(argc,argv,std::string("-i"),inDir))
      {
         toVTK->SetInputDirectory(inDir);
         toVTK->SetNumberOfFoundFiles(1);
      }
      std::string outDir;
      if(_extractOptionFromCmdLine(argc,argv,std::string("-o"),outDir))
      {
         toVTK->SetOutputDirectory(outDir);
      }
   }
}
/////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::AddFoundFile(std::string singleFile)
{
   _infileNames.push_back(singleFile);
   _nFoundFiles = static_cast< int >( _infileNames.size() );
}
/////////////////////////////////////////////////////////////////////////////////
bool cfdTranslatorToVTK::PreTranslateCallback::_extractOptionFromCmdLine(int argc,
                                                               char** argv,
                                                      std::string optionFlag,
                                                      std::string& optionArg)
{
   std::cout << "Number of arguments: " << argc << std::endl;
   for(int i = 0; i < argc; i++)
   {
      std::string curArg(argv[i]);
      if(curArg == optionFlag)
      {
         optionArg = std::string(argv[++i]);
         std::cout<<"Found option: "<<optionFlag<<":"<<optionArg<<std::endl;

         return true;
      }
   }
   return false;
}
