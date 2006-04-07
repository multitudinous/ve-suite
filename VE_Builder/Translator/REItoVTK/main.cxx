#include <iostream>
#include "VE_Builder/Translator/REItoVTK/cfdREIToVTK.h"
///////////////////////////////////////////////////////////////////
//Example of how to read REI files and create vtk files for the//
//REI data                                                     //
///////////////////////////////////////////////////////////////////
void main(int argc, char** argv)
{
   VE_Builder::cfdREItoVTK translator;
   translator.TranslateToVTK(argc,argv);
}