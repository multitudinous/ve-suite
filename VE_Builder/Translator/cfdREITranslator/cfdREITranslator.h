#ifndef _CFD_REI_TRANS_H_
#define _CFD_REI_TRANS_H_


#include "VE_Builder/Translator/cfdTranslatorToVTK/cfdTranslatorToVTK.h"
#include <set>

namespace VE_Builder{
class /*VE_BUILDER_EXPORTS*/ cfdREITranslator: 
   public VE_Builder::cfdTranslatorToVTK{

public:
   cfdREITranslator();
   virtual ~cfdREITranslator();

   class REITranslatorCbk: public VE_Builder::cfdTranslatorToVTK::TranslateCallback{
   public:
      REITranslatorCbk(){};
      virtual ~REITranslatorCbk(){};
      //////////////////////////////////////////////////
      //ouputDataset should be populated              //
      //appropriately by the translate callback.      //
      //////////////////////////////////////////////////
      virtual void Translate(vtkDataSet*& outputDataset,
		                     cfdTranslatorToVTK* toVTK);
      int debug;
   protected:
   };
   class REIPreTranslatorCbk: public VE_Builder::cfdTranslatorToVTK::PreTranslateCallback{
   public:
      REIPreTranslatorCbk(){};
      virtual ~REIPreTranslatorCbk(){};
      //void Preprocess(int argc,char** argv,VE_Builder::cfdTranslatorToVTK* toVTK);
   protected:
   };
protected:
   REIPreTranslatorCbk _cmdParser;
   REITranslatorCbk _reiTranslator;
};

}
#endif//_CFD_REI_TRANS_H_
