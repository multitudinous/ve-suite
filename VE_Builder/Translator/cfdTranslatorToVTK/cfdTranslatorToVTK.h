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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef _CFD_TRANSLATOR_TO_VTK_H_
#define _CFD_TRANSLATOR_TO_VTK_H_
#include <string>
#include <vector>


class vtkDataSet;
#include "VE_Installer/include/VEConfig.h"
namespace VE_Builder
{
class VE_BUILDER_EXPORTS cfdTranslatorToVTK
{
public:
   cfdTranslatorToVTK();
   virtual ~cfdTranslatorToVTK();
   
   void SetFileExtension(std::string fileExtension);
   void SetNumberOfFoundFiles(unsigned int nFilesFound);
   void SetInputDirectory(std::string inDir);
   void SetOutputDirectory(std::string inDir);
   void SetFileName( std::string fileName );
   ///Function to list all the features of a respective translator
   ///when the -h option is specified
   virtual void DisplayHelp( void ) = 0;

   ///Utility function to process command line args
   ///\param argc The number of command line args
   ///\param argv The arg values as chars
   ///\param optionFlag The option you are looking for
   ///\param optionArg The returned argument
   bool _extractOptionFromCmdLine(int argc,char** argv,
                                  std::string optionFlag,
                                  std::string& optionArg);
   /////////////////////////////////////////////////////
   //The idea for these callback classes is similar to//
   //the setup in OSG (www.openscenegraph.org)        //
   /////////////////////////////////////////////////////
   
   ////////////////////////////////////////////////////////
   //Any preprocessing can be easily setup to happen here//
   ////////////////////////////////////////////////////////
   class VE_BUILDER_EXPORTS PreTranslateCallback{
   public:
      PreTranslateCallback(){};
      virtual ~PreTranslateCallback(){};
      virtual void Preprocess(int argc, char** argv,
                            cfdTranslatorToVTK* toVTK);
   };

   /////////////////////////////////////////////////////////
   //translate callback must be defined or nothing happens//
   /////////////////////////////////////////////////////////
   class VE_BUILDER_EXPORTS TranslateCallback{
   public:
      TranslateCallback(){};
      virtual ~TranslateCallback(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		             VE_Builder::cfdTranslatorToVTK* toVTK) = 0;
   protected:
   };
   ///////////////////////////////////////////////////////////
   //Any post-processing can be easily setup to happen here //
   ///////////////////////////////////////////////////////////
   class VE_BUILDER_EXPORTS PostTranslateCallback{
   public:
      PostTranslateCallback(){};
      virtual ~PostTranslateCallback(){};
      virtual void PostProcess(VE_Builder::cfdTranslatorToVTK* toVTK) = 0;
   protected:
   };

   ///Set all the callbacks for the required translator
   ///\param preTCbk Callback class
   void SetPreTranslateCallback(PreTranslateCallback* preTCbk);
   ///\param postTCbk Callback class
   void SetPostTranslateCallback(PostTranslateCallback* postTCbk);
   ///\param tCbk Callback class
   void SetTranslateCallback(TranslateCallback* tCbk);
   ///Get all the callbacks for the required translator
   PreTranslateCallback* GetPreTranslateCallback( void );
   PostTranslateCallback* GetPostTranslateCallback( void );
   TranslateCallback* GetTranslateCallback( void );

   void AddFoundFile(std::string singleFile);
   void AddBaseName(std::string baseName);
   void ExtractBaseName(std::string fileName);
   /////////////////////////////////////////
   //main translation calling method      //
   //Basically makes the following calls: //
   //PreTranslateCallback::Preprocess();  //
   //TranslateCallback::Translate();      // 
   //PostTranslateCallback::PostProcess();//
   //_writeVTKFile();                      //
   /////////////////////////////////////////
   bool TranslateToVTK(int argc, char** argv);
   
   std::string GetFileExtension();
   std::string GetInputDirectory();
   std::string GetOutputDirectory();

   unsigned int GetNumberOfFoundFiles();
   std::string GetFile( unsigned int fileNumber );

   vtkDataSet* GetVTKFile( unsigned int whichFile );
protected:
   bool _writeToVTK( unsigned int whichFile );
   ///Write the file to memory so that it is accessible 
   ///through other interfaces
   unsigned int _nFoundFiles;

   std::vector<std::string> baseFileNames;
   std::string _fileExtension;
   std::string _inputDir;
   std::string _outputDir;
   std::vector<std::string> _infileNames;

   std::vector<std::string> _outfileNames;

   PreTranslateCallback* _preTCbk;
   PostTranslateCallback* _postTCbk;
   TranslateCallback* _translateCbk;

   vtkDataSet* _outputDataset;
};
}
#endif //_CFD_TRANSLATOR_TO_VTK_H_
