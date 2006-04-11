#include <iostream>
#include "VE_Builder/Translator/DataLoader/DataLoader.h"
///////////////////////////////////////////////////////////////////
//Example of how to read dicom files and create vtk files for the//
//dicom data                                                     //
///////////////////////////////////////////////////////////////////
int main(int argc, char** argv)
{
   VE_Builder::DataLoader loader;
   loader.SetInputData( "something", "somedir" );
   vtkDataSet* tempData = loader.GetVTKDataSet( argc, argv );
   return 0;
}
