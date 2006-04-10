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
   //protected:
      bool _extractOptionFromCmdLine(int argc,char** argv,
                                  std::string optionFlag,
                                  std::string& optionArg);

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

   void SetPreTranslateCallback(PreTranslateCallback* preTCbk);
   void SetPostTranslateCallback(PostTranslateCallback* postTCbk);
   void SetTranslateCallback(TranslateCallback* tCbk);

   void AddFoundFile(std::string singleFile);
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

   std::string baseFileName;
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
