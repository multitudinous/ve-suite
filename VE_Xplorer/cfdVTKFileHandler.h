#ifndef CFD_VTK_FILE_HANDLER_H
#define CFD_VTK_FILE_HANDLER_H
class vtkXMLFileReadTester;
class vtkDataSet;
class cfdVTKFileHandler{
public:
   cfdVTKFileHandler();
   cfdVTKFileHandler(const cfdVTKFileHandler& fh);
   virtual ~cfdVTKFileHandler();

   enum OutFileType{CFD_XML,VTK_CLASSIC};
   enum OutFileMode{CFD_ASCII=0,CFD_BINARY};

   void SetInputFileName(char* inFile);
   void SetOutputFileName(char* oFile);
   void SetVTKOutFileType(OutFileType type);
   void SetOutFileWriteMode(OutFileMode mode);

   vtkDataSet* GetDataSetFromFile(char* vtkFileName);
   bool WriteDataSet(vtkDataSet* dataSet,char* outFileName);

   cfdVTKFileHandler& operator=(const cfdVTKFileHandler& fh);
protected:
   void _getXMLUGrid();
   void _getXMLSGrid();
   void _getXMLRGrid();
   void  _getXMLPolyData();
   void _readClassicVTKFile();
   void _writeClassicVTKFile( vtkDataSet * vtkThing, 
                            char * vtkFilename, int binaryFlag = 0 );

   OutFileType _outFileType;
   OutFileMode _outFileMode;

   char* _inFileName;
   char* _outFileName;
   vtkXMLFileReadTester* _xmlTester;   
   vtkDataSet* _dataSet;
};
#endif// CFD_VTK_FILE_HANDLER_H
