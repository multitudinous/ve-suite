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
#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"
#include <bitset>
#include <sstream>
#include <iostream>
#include <vtkDataSet.h>
#include "VE_Xplorer/readWriteVtkThings.h"
using namespace VE_Builder;
////////////////////////////////////////
//Constructor                         //
////////////////////////////////////////
cfdTranslatorToVTK::cfdTranslatorToVTK()
{
   _nFoundFiles = 0;

   baseFileName = "flowdata";
   _fileExtension = "vtk";
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
///////////////////////////////////////////////////////////////
void cfdTranslatorToVTK::SetFileName( std::string fileName )
{
   baseFileName = fileName;
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

   std::bitset< 16 > status;

   for ( unsigned int i = 0; i < _nFoundFiles; ++i )
   {
      if ( _translateCbk )
      {
         _translateCbk->Translate( _outputDataset, this );
      }

      if ( _postTCbk )
      {
         _postTCbk->PostProcess( this );
      }
      // Determine if we need to write the file or not
      std::string writeOption;
      if ( _extractOptionFromCmdLine( argc, argv, std::string("-w"), writeOption ) )
      {
         if ( writeOption == std::string( "file" ) )
         {
            status.set( i, _writeToVTK(i) );
            // because we are done with it after this...
            _outputDataset->Delete();
         }
         else
         {
            status.set( i, true );
         }
      }
   }

   for ( unsigned int i = 0; i < status.size(); ++i )
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
         tempName << _outputDir << "/" 
                  << baseFileName << "_" 
                  << fileNum << "." 
                  << _fileExtension << "\0";
         _outfileNames.push_back(tempName.str());
      }
      VE_Util::writeVtkThing(_outputDataset, 
                          (char*)_outfileNames.at(fileNum).c_str(),1);
      return true;
   }
   else
   {
      std::cout<<"Invalid output vtk dataset!!!"<<std::endl;
      std::cout<<"cfdTranslatorToVTK::_writeToVTK"<<std::endl;
      return false;
   }
}
//////////////////////////////////////////////////////////
vtkDataSet* cfdTranslatorToVTK::GetVTKFile( unsigned int fileNum )
{
   if ( _outputDataset )
   {
      return _outputDataset;
   }
   else
   {
      std::cout<<"Invalid output vtk dataset!!!"<<std::endl;
      std::cout<<"cfdTranslatorToVTK::_writeToVTK"<<std::endl;
      return 0;
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
      if( toVTK->_extractOptionFromCmdLine(argc,argv,std::string("-i"),inDir))
      {
         toVTK->SetInputDirectory(inDir);
         toVTK->SetNumberOfFoundFiles(1);
      }
      std::string outDir;
      if( toVTK->_extractOptionFromCmdLine(argc,argv,std::string("-o"),outDir))
      {
         toVTK->SetOutputDirectory(outDir);
      }
      std::string outFileName;
      if( toVTK->_extractOptionFromCmdLine(argc,argv,std::string("-outFileName"),outFileName))
      {
         toVTK->SetFileName( outFileName );
      }
      std::string singleFile;
      if ( toVTK->_extractOptionFromCmdLine(argc,argv,std::string("-singleFile"),singleFile) )
      {
         toVTK->AddFoundFile(singleFile);
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
bool cfdTranslatorToVTK::_extractOptionFromCmdLine(int argc,
                                                               char** argv,
                                                      std::string optionFlag,
                                                      std::string& optionArg)
{
   //std::cout << "Number of arguments: " << argc << std::endl;
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
