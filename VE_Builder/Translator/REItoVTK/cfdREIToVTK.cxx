#include "VE_Builder/Translator/REItoVTK/cfdREIToVTK.h"
#include "VE_Builder/Translator/REItoVTK/DBConverter.h"
#include "VE_Builder/Translator/REItoVTK/PPConverter.h"
#include <vtkDataSet.h>
#include <vtkImageData.h>
#include <vtkDICOMImageReader.h>
#include <vtkStructuredGrid.h>
#include <vtkProbeFilter.h>
#include <vtkPoints.h>

using namespace VE_Builder;
////////////////////////////////////////
//Constructors                        //
////////////////////////////////////////
cfdREItoVTK::cfdREItoVTK()
{
   SetTranslateCallback(&_reiToVTK);
   SetPreTranslateCallback(&_cmdParser);
}
/////////////////////////////////////////
cfdREItoVTK::~cfdREItoVTK()
{

}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::REIPreTranslateCbk::Preprocess(int argc,char** argv,
                                               VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::cfdREItoVTK* reiToVTK =
              dynamic_cast<VE_Builder::cfdREItoVTK*>(toVTK);

   if (argc< 2)
   {
      std::cout<<"wrong number of parameters!"<<std::endl;
      std::cout<<"convertvtk [work directory]"<<std::endl;
      exit(0);
   }
   // argv:
   //   1 : DB case
   //   2 : PPLOT1
   //   3 : PPLOT3
   //   4 : db.vtk to create
   //   5 : pp.vtk to create
   //   6 : pp.dat to create
   //   7 : wireframe to create
   //   8 : scalar 1
   //   9 : scalar 2
   //   .
   //   .
   //   .
   //   (n+7) : scalar n
   //for(int i=8; i<argc; i++)
    //scalars.insert(argv[i]);
   std::set<std::string> scalartemp = reiToVTK->Getscalars();
   scalartemp.insert("walls");
   scalartemp.insert("vel");
   reiToVTK->Setscalars(scalartemp);
    
   reiToVTK->Setdb_file(std::string(argv[1])+"\\BANFFDB");//std::string(argv[1]);
   reiToVTK->Setpp1_file(std::string(argv[1])+"\\PPLOT1");//std::string(argv[2]);
   reiToVTK->Setpp3_file(std::string(argv[1])+"\\PPLOT3");//std::string(argv[3]);
   reiToVTK->Setdbv_file(std::string(argv[1])+"\\db.vtk");//std::string(argv[5]);
   reiToVTK->Setppv_file(std::string(argv[1])+"\\pp.vtk");//std::string(argv[5]);
   //reiToVTK->Setpdv_file("";std::string(argv[1])+"\\pp.dat");//std::string(argv[6]);
   reiToVTK->Setpdv_file(std::string(argv[1])+"\\pp.dat");
   reiToVTK->Setwrv_file(std::string(argv[1])+"\\minmax");//std::string(argv[7]);
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::REITranslateCbk::Translate(vtkDataSet*& outputDataset,
		                                           VE_Builder::cfdTranslatorToVTK* toVTK)
{
   VE_Builder::cfdREItoVTK* reiToVTK =
              dynamic_cast<VE_Builder::cfdREItoVTK*>(toVTK);

   DBConverter conv(reiToVTK->Getdb_file());
   conv.makeVTK(reiToVTK->Getdbv_file(), reiToVTK->Getwrv_file(), reiToVTK->Getscalars()); 

   PPConverter ppconv(reiToVTK->Getpp1_file(), reiToVTK->Getpp3_file());
   ppconv.makeVTK(reiToVTK->Getppv_file(), reiToVTK->Getpdv_file());
}
//////////////////////////////////////////////////////////////////////////////// 
std::string cfdREItoVTK::Getdb_file()
{
   return db_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getpp1_file()
{
   return pp1_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getpp3_file()
{
   return pp3_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getdbv_file()
{
   return dbv_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getppv_file()
{
   return ppv_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getpdv_file()
{
   return pdv_file;
}
////////////////////////////////////////////////////////////////////////////////
std::string cfdREItoVTK::Getwrv_file()
{
   return wrv_file;
}
////////////////////////////////////////////////////////////////////////////////
std::set<std::string> cfdREItoVTK::Getscalars()
{
   return scalars;
}
//////////////////////////////////////////////////////////////////////////////// 
void cfdREItoVTK::Setdb_file(std::string file)
{
   db_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setpp1_file(std::string file)
{
   pp1_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setpp3_file(std::string file)
{
   pp3_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setdbv_file(std::string file)
{
   dbv_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setppv_file(std::string file)
{
   ppv_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setpdv_file(std::string file)
{
   pdv_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setwrv_file(std::string file)
{
   wrv_file = file;
}
////////////////////////////////////////////////////////////////////////////////
void cfdREItoVTK::Setscalars(std::set<std::string> scalarset)
{
   scalars = scalarset;
}