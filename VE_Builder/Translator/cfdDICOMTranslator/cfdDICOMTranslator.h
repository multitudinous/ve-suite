#ifndef _CFD_DICOM_TRANSLATOR_H_
#define _CFD_DICOM_TRANSLATOR_H_


#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder{
class /*VE_BUILDER_EXPORTS*/ cfdDICOMTranslator: 
   public VE_Builder::cfdTranslatorToVTK{

public:
   cfdDICOMTranslator();
   virtual ~cfdDICOMTranslator();
 
   class DICOMTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback{
   public:
      DICOMTranslateCbk(){};
      virtual ~DICOMTranslateCbk(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
   protected:
   };
   class DICOMPreTranslateCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback{
   public:
      DICOMPreTranslateCbk(){};
      virtual ~DICOMPreTranslateCbk(){};
   protected:
   };
protected:
   DICOMPreTranslateCbk _cmdParser;
   DICOMTranslateCbk _dicomToVTK;
};

}
#endif//_CFD_DICOM_TRANSLATOR_H_
