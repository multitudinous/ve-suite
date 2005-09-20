#include <iostream>
#include "VE_Builder/Translator/cfdDICOMTranslator/cfdDICOMTranslator.h"
///////////////////////////////////////////////////////////////////
//Example of how to read dicom files and create vtk files for the//
//dicom data                                                     //
///////////////////////////////////////////////////////////////////
void main(int argc, char** argv)
{
   VE_Builder::cfdDICOMTranslator translator;
   translator.TranslateToVTK(argc,argv);
}