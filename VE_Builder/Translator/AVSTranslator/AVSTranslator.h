#ifndef _AVS_TRANSLATOR_H_
#define _AVS_TRANSLATOR_H_


#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"

namespace VE_Builder{
class /*VE_BUILDER_EXPORTS*/ AVSTranslator: 
   public VE_Builder::cfdTranslatorToVTK{

public:
   AVSTranslator();
   virtual ~AVSTranslator();
 
   class AVSTranslateCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback{
   public:
      AVSTranslateCbk(){};
      virtual ~AVSTranslateCbk(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
   protected:
   };
   class AVSPreTranslateCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback{
   public:
      AVSPreTranslateCbk(){};
      virtual ~AVSPreTranslateCbk(){};
      void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
   protected:
   };
protected:
   AVSPreTranslateCbk _cmdParser;
   AVSTranslateCbk _AVSToVTK;
};

}
#endif//_AVS_TRANSLATOR_H_
