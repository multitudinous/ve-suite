#include "cfdHDFToVTK.h"

int main(int argc, char** argv)
{
   cfdHDFToVTK* xlatr = new cfdHDFToVTK("C:/HDF/DavidClarke/zhto018ad.hdf");

   xlatr->setVerboseTranslationFlag(0);
   xlatr->setType(cfdHDFToVTK::DCLARKE);
   xlatr->viewGridBeforeWriting(0);
   //xlatr->writeAsciiOutput();
   xlatr->setOutputVTKDirectory("C:/VE_Suite_112204/VE_TestSuite/");
   xlatr->setVTKOutFileName("zhto018ad");
   xlatr->translateFileToVTK();
   

   if(xlatr){
      delete xlatr;
      xlatr = 0;
   }
   return 0;
}