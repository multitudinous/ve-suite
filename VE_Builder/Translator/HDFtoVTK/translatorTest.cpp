#include "cfdHDFToVTK.h"

int main(int argc, char** argv)
{
   cfdHDFToVTK* xlatr = new cfdHDFToVTK("C:/HDF/DavidClarke/zhto002ad.hdf");

   xlatr->setVerboseTranslationFlag(1);
   xlatr->setType(cfdHDFToVTK::DCLARKE);
   xlatr->viewGridBeforeWriting(0);
   xlatr->setOutputVTKDirectory("C:/HDF/DavidClarke/data/");
   xlatr->setVTKOutFileName("DClarke");
   xlatr->translateFileToVTK();
   

   if(xlatr){
      delete xlatr;
      xlatr = 0;
   }
   return 0;
}