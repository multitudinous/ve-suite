#ifndef CFD_VTK_FILE_HANDLER_H
#define CFD_VTK_FILE_HANDLER_H
class vtkXMLFileReadTester;
class vtkDataSet;

#include "VE_Installer/include/VEConfig.h"
#include <string>

namespace VE_Util
{
   class VE_UTIL_EXPORTS cfdVTKFileHandler
   {
      public:
         cfdVTKFileHandler();
         cfdVTKFileHandler(const cfdVTKFileHandler& fh);
         virtual ~cfdVTKFileHandler();

         enum OutFileType{CFD_XML,VTK_CLASSIC};
         enum OutFileMode{CFD_ASCII=0,CFD_BINARY};

         void SetInputFileName(std::string inFile);
         void SetOutputFileName(std::string oFile);
         void SetVTKOutFileType(OutFileType type);
         void SetOutFileWriteMode(OutFileMode mode);

         vtkDataSet* GetDataSetFromFile(std::string vtkFileName);
         bool WriteDataSet(vtkDataSet* dataSet,std::string outFileName);

         cfdVTKFileHandler& operator=(const cfdVTKFileHandler& fh);
      protected:
         void _getXMLUGrid();
         void _getXMLSGrid();
         void _getXMLRGrid();
         void  _getXMLPolyData();
         void _readClassicVTKFile();
         void _writeClassicVTKFile( vtkDataSet * vtkThing, 
                            std::string vtkFilename, int binaryFlag = 0 );

         OutFileType _outFileType;
         OutFileMode _outFileMode;

         std::string _inFileName;
         std::string _outFileName;
         vtkXMLFileReadTester* _xmlTester;   
         vtkDataSet* _dataSet;
   };
}
#endif// CFD_VTK_FILE_HANDLER_H
