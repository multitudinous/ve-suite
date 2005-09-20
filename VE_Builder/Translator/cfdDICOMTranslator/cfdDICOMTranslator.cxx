#include "VE_Builder/Translator/cfdDICOMTranslator/cfdDICOMTranslator.h"
#include <vtkDataSet.h>
#include <vtkImageData.h>
#include <vtkDICOMImageReader.h>
#include <vtkStructuredGrid.h>
#include <vtkProbeFilter.h>
#include <vtkPoints.h>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
cfdDICOMTranslator::cfdDICOMTranslator()
{

   SetTranslateCallback(&_dicomToVTK);
   SetPreTranslateCallback(&_cmdParser);
}
/////////////////////////////////////////
cfdDICOMTranslator::~cfdDICOMTranslator()
{

}
////////////////////////////////////////////////////////////////////////////////
void cfdDICOMTranslator::DICOMTranslateCbk::Translate(vtkDataSet*& outputDataset,
		                                           VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::cfdDICOMTranslator* dicomToVTK =
              dynamic_cast<VE_Builder::cfdDICOMTranslator*>(toVTK);
   if(dicomToVTK)
   {
      vtkDICOMImageReader* dicomTranslator = vtkDICOMImageReader::New();
      //dicomTranslator->DebugOn();
      dicomTranslator->SetDirectoryName(dicomToVTK->GetInputDirectory().c_str());
      dicomTranslator->Update();
      
      double delta[3] = {0,0,0};
      double origin[3] = {0,0,0};
      dicomTranslator->GetOutput()->GetOrigin(origin);
      dicomTranslator->GetOutput()->GetSpacing(delta);

      int dims[3];
      dicomTranslator->GetOutput()->GetDimensions(dims);
 /*     dims[0] = dicomTranslator->GetWidth();
      dims[1] = dicomTranslator->GetHeight();
      dims[2] = 16;//how do we get the number of files?dicomTranslator->GetNumberOfDICOMFileNames();
*/
      vtkProbeFilter* probeFilter = vtkProbeFilter::New();
      //probeFilter->DebugOn();

      if(!outputDataset){
         outputDataset = vtkStructuredGrid::New();
      }
      vtkStructuredGrid* tempGrid = vtkStructuredGrid::New();
      vtkPoints* tempPoints = vtkPoints::New();
      tempPoints->Allocate(dims[0]*dims[1]*dims[2]);
      
      unsigned int kOffset = 0;
      unsigned int jOffset = 0;
      double pt[3] = {0,0,0};
      for ( int k=0; k < dims[2]; k++)
      {
         pt[2] = k*delta[2] + origin[2];
         kOffset = k * dims[0] * dims[1];
         for (int j=0; j<dims[1]; j++) 
         {
            jOffset = j * dims[0];
            pt[1] = j*delta[1] + origin[1];
             
             for ( int i=0; i<dims[0]; i++) 
             {
               pt[0] = i*delta[0] + origin[0];
               tempPoints->InsertPoint((i + jOffset + kOffset),pt);
             }
         }
      }
      tempGrid->SetDimensions(dims);
      tempGrid->SetPoints(tempPoints);
      tempGrid->Update();

      probeFilter->SetInput(tempGrid);
      probeFilter->SetSource(dicomTranslator->GetOutput());
      probeFilter->Update();
   
      outputDataset->DeepCopy(probeFilter->GetStructuredGridOutput());
      outputDataset->Update();
      //outputDataset->Print(std::cerr);
      tempPoints->Delete();
      tempGrid->Delete();
      probeFilter->Delete();
      dicomTranslator->Delete();
   }
}
 
