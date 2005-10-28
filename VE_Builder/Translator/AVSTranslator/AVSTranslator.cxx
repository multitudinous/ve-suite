#include "VE_Builder/Translator/AVSTranslator/AVSTranslator.h"
#include <vtkDataSet.h>
#include <vtkAVSucdReader.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <vtkPointData.h>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
AVSTranslator::AVSTranslator()
{

   SetTranslateCallback(&_AVSToVTK);
   SetPreTranslateCallback(&_cmdParser);
}
/////////////////////////////////////////
AVSTranslator::~AVSTranslator()
{

}
//////////////////////////////////////////////////////////////////////////
void AVSTranslator::AVSPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   if(toVTK)
   {
      std::string singleFile;
      if(_extractOptionFromCmdLine(argc,argv,std::string("-singleFile"),singleFile))
      {
         toVTK->AddFoundFile(singleFile);
      }
      std::string outDir;
      if(_extractOptionFromCmdLine(argc,argv,std::string("-o"),outDir))
      {
         toVTK->SetOutputDirectory(outDir);
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void AVSTranslator::AVSTranslateCbk::Translate(vtkDataSet*& outputDataset,
		                                     VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::AVSTranslator* AVSToVTK =
              dynamic_cast<VE_Builder::AVSTranslator*>(toVTK);
   if(AVSToVTK)
   {
      vtkAVSucdReader* avsReader = vtkAVSucdReader::New();
      avsReader->SetFileName(AVSToVTK->GetFile(0).c_str());
      avsReader->Update();
      if(!outputDataset){
         outputDataset = vtkUnstructuredGrid::New();
      }
      vtkDataSet* tmpDSet = vtkUnstructuredGrid::New();
      tmpDSet->DeepCopy(avsReader->GetOutput());

      //get the info about the data in the data set
      unsigned int nPtDataArrays = tmpDSet->GetPointData()->GetNumberOfArrays();
      if(!nPtDataArrays){
         std::cout<<"Warning!!!"<<std::endl;
         std::cout<<"No point data found!"<<std::endl;
         std::cout<<"Attempting to convert cell data to point data."<<std::endl;

         vtkCellDataToPointData* dataConvertCellToPoint = vtkCellDataToPointData::New();
      
         dataConvertCellToPoint->SetInput(tmpDSet);
         dataConvertCellToPoint->PassCellDataOff();
         dataConvertCellToPoint->Update();
         outputDataset->DeepCopy(dataConvertCellToPoint->GetOutput());
         outputDataset->Update();
         return;
      }else{
         outputDataset->DeepCopy(tmpDSet);
         outputDataset->Update();
      }
   }
}
 
