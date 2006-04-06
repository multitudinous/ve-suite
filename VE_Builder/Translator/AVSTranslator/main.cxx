#include <iostream>
#include "VE_Builder/Translator/AVSTranslator/AVSTranslator.h"
///////////////////////////////////////////////////////////////////
//Example of how to read dicom files and create vtk files for the//
//dicom data                                                     //
///////////////////////////////////////////////////////////////////
void main(int argc, char** argv)
{
   VE_Builder::AVSTranslator translator;
   translator.TranslateToVTK(argc,argv);
}