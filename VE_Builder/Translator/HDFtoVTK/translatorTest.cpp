#include "cfdHDFtoVTK.h"

int main(int argc, char** argv)
{
   char* inFile = 0;
   char* outdir = 0;
   char* outprefix = 0;
   if(argc < 3 ){
      std::cout<<"Usage:"<<std::endl;
      std::cout<<"HDFtoVTK ./infile.hdf ./outDir outname"<<std::endl 
      		<<"./infile.hdf ==> path and filename to read in"<<std::endl
                <<"./outDir     ==> path to write output"<<std::endl
                <<"outname      ==> prefix(before .vtk extension) for output files"<<std::endl;
      exit(0);
   }else{
      inFile = new char[strlen(argv[1])+1];   
      strcpy(inFile,argv[1]);
      outdir = new char[strlen(argv[2])+1];   
      strcpy(outdir,argv[2]);
      outprefix = new char[strlen(argv[3])+1];   
      strcpy(outprefix,argv[3]);
   }
   cfdHDFToVTK* xlatr = new cfdHDFToVTK(inFile);

   xlatr->setVerboseTranslationFlag(0);
   xlatr->setType(cfdHDFToVTK::DCLARKE);
   xlatr->viewGridBeforeWriting(0);
   xlatr->setOutputVTKDirectory(outdir);
   xlatr->setVTKOutFileName(outprefix);
   xlatr->translateFileToVTK();

   if(xlatr){
      delete xlatr;
      xlatr = 0;
   }
   if(inFile){
      delete [] inFile;
      inFile = 0;
   }
   if(outdir){
      delete [] outdir;
      outdir = 0;
   }
   if(outprefix){
      delete [] outprefix;
      outprefix = 0;
   }
   return 0;
}
