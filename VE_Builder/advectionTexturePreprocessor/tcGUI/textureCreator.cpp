#include "textureCreator.h"
#include <vtkCellLocator.h>
#include <vtkGenericCell.h>
#include <vtkPointData.h>
#include <vtkCellData.h>
#include <vtkDataArray.h>
#include <vtkDoubleArray.h>
#include <vtkUnstructuredGridReader.h>
#include <vtkRectilinearGrid.h>
#include <vtkUnstructuredGrid.h>
#include <vtkCellDataToPointData.h>
#include <iostream>

#ifdef WIN32
#include <direct.h>
#endif
////////////////////////////////////
//Constructor                     // 
////////////////////////////////////
VTKDataToTexture::VTKDataToTexture()
{
   _curPt = 0;
   _nPtDataArrays = 0;
   _resolution[0] = 2;
   _resolution[0] = 2;
   _resolution[0] = 2;

   //_maxDistFromCenter = 0;
   _nScalars = 0;
   _nVectors = 0;
   _scalarNames = 0;
   _vectorNames = 0;
   _dataSet = 0;
   _vFileName = 0;
   _outputDir = 0;
 
   _dataConvertCellToPoint = 0;
   _usgrid = 0;
   _rgrid = 0;

  
   _isRGrid = false;
}
/////////////////////////////////////////////////////////////
VTKDataToTexture::VTKDataToTexture(const VTKDataToTexture& v)
{
   _curPt = v._curPt;
   _isRGrid = v._isRGrid;
   _rgrid = v._rgrid;
   _resolution[0] = v._resolution[0];
   _resolution[1] = v._resolution[1];
   _resolution[2] = v._resolution[2];
  _dataConvertCellToPoint = v._dataConvertCellToPoint;
   _nPtDataArrays = v._nPtDataArrays;
   _nScalars = v._nScalars;
   _nVectors = v._nVectors;
   _dataSet = v._dataSet;
   _velocity = v._velocity;
   _curScalar = v._curScalar;
   _scalarRanges = v._scalarRanges;
   _vectorRanges = v._vectorRanges;
   for( int i=0; i<_nScalars; i++ ){
      delete [] _scalarNames[i];
   }
   delete [] _scalarNames;
   _scalarNames = 0;
   _scalarNames = getParameterNames(1,_nScalars);
   for( int i=0; i<_nVectors; i++ ){
      delete [] _vectorNames[i];
   }
   delete [] _vectorNames;
   _vectorNames = 0;
   _vectorNames = getParameterNames(3,_nVectors);

   //_maxDistFromCenter = v._maxDistFromCenter;
   setVelocityFileName(v._vFileName);
   setOutputDirectory(v._outputDir);
   if(_validPt.size()){
      _validPt.clear();
   }
   unsigned int nPts = v._validPt.size();
   for(unsigned int i = 0; i < nPts; i++){
      _validPt.push_back(v._validPt.at(i));
   }
   /* nPts = v._distance.size();
   for(unsigned int i = 0; i < nPts; i++){
      _distance.push_back(v._distance.at(i));
   }*/

}
/////////////////////////////////////
//Destructor                       //
/////////////////////////////////////
VTKDataToTexture::~VTKDataToTexture()
{
   for( int i=0; i<_nScalars; i++ ){
      delete [] _scalarNames[i];
   }
   delete [] _scalarNames;
   _scalarNames = 0;
   for( int i=0; i<_nVectors; i++ ){
      delete [] _vectorNames[i];
   }
   delete [] _vectorNames;
   _vectorNames = 0;


   if(_vFileName){
      delete [] _vFileName;
      _vFileName = 0;
   }
   if(_outputDir){
      delete [] _outputDir;
      _outputDir = 0;
   }
   if(_dataConvertCellToPoint)
      _dataConvertCellToPoint->Delete(); 
   _velocity.clear();
   _curScalar.clear();
   _scalarRanges.clear();
   _vectorRanges.clear();
   if(_usgrid){
      _usgrid->Delete();
   }
   if(_validPt.size()){
      _validPt.clear();
   }
   /*if(_distance.size()){
      _distance.clear();
   }*/
   
}
//////////////////////////////
void VTKDataToTexture::reset()
{
   for( int i=0; i<_nScalars; i++ ){
      delete [] _scalarNames[i];
   }
   delete [] _scalarNames;
   _scalarNames = 0;
   for( int i=0; i<_nVectors; i++ ){
      delete [] _vectorNames[i];
   }
   delete [] _vectorNames;
   _vectorNames = 0;

   if(_vFileName){
      delete [] _vFileName;
      _vFileName = 0;
   }
   if(_outputDir){
      delete [] _outputDir;
      _outputDir = 0;
   }
  
   _velocity.clear();
   _curScalar.clear();
   _scalarRanges.clear();
   _vectorRanges.clear();
  
   _nPtDataArrays = 0;
   if(_validPt.size()){
      _validPt.clear();
   }
   
}
///////////////////////////////////////////////////////////
//set the file name                                      //
//example:                                               //
//if vFileName == "vectors"                              //
//output will be "./textures/vectors_p.rgba"             //
//               "./textures/vectors_n.rgba"             // 
//representing the positive and negative textures        //
///////////////////////////////////////////////////////////
void VTKDataToTexture::setVelocityFileName(char* vFileName)
{
   if(_vFileName){
      delete [] _vFileName;
      _vFileName = 0;
   }
   _vFileName = new char[strlen(vFileName)+1];
   strcpy(_vFileName,vFileName);
}
///////////////////////////////////////////////////////
//set the output directory                           //
///////////////////////////////////////////////////////
void VTKDataToTexture::setOutputDirectory(char* outDir)
{
   if(_outputDir){
      delete [] _outputDir;
      _outputDir = 0;
   }
   _outputDir = new char[strlen(outDir)+1];
   strcpy(_outputDir,outDir);

}
////////////////////////////////////////////////////////////
//create a dataset from a file                            //
////////////////////////////////////////////////////////////
void VTKDataToTexture::createDataSetFromFile(char* filename)
{
   if(!_isRGrid){
      if(!_usgrid){
         _usgrid = vtkUnstructuredGridReader::New();
      }
      //update the grid w/ the new file name
      _usgrid->SetFileName(filename);
      _usgrid->Update();
      vtkDataSet* tmpDSet = _usgrid->GetOutput();

      
      //get the info about the data in the data set
      _nPtDataArrays = tmpDSet->GetPointData()->GetNumberOfArrays();
      if(!_nPtDataArrays){
         std::cout<<"Warning!!!"<<std::endl;
         std::cout<<"No point data found!"<<std::endl;
         std::cout<<"Attempting to convert cell data to point data."<<std::endl;
         if(!_dataConvertCellToPoint)
            _dataConvertCellToPoint = vtkCellDataToPointData::New();

         _dataConvertCellToPoint->SetInput( tmpDSet);
         _dataConvertCellToPoint->PassCellDataOff();
         _dataConvertCellToPoint->Update();
         setDataset(_dataConvertCellToPoint->GetUnstructuredGridOutput());
      }else{
         setDataset(tmpDSet);
      }
     
   }else{
      if(!_rgrid){
         _rgrid = vtkRectilinearGridReader::New();
      }
      _rgrid->SetFileName(filename);
      _rgrid->Update();
      setDataset(_rgrid->GetOutput());
   }
}
/////////////////////////////////////////
//create the textures for this data set//
/////////////////////////////////////////
void VTKDataToTexture::createTextures()
{
   //check for a dataset
   if(!_dataSet){
      if(_vFileName){
         createDataSetFromFile(_vFileName);
      }else{
         std::cout<<"No dataset available to";
         std::cout<<" create a texture!!"<<std::endl;
         std::cout<<"ERROR: VTKDataToTexture::createTextures()"<<std::endl;
         return;
      }
   }

   //was the resolution initialized?
   if(_resolution[0] == 2 &&
      _resolution[1] == 2 &&
      _resolution[2] == 2){
      std::cout<<"WARNING: Resolution set to the min!:"<<std::endl;
      std::cout<<" : VTKDataToTexture::createTextures()"<<std::endl;
   }
   //build the octree
   _cLocator = vtkCellLocator::New();
   _cLocator->SetDataSet(_dataSet);
   
   _cLocator->SetCacheCellBounds(1);
   //build the octree
   _cLocator->BuildLocator();
   //get the info about the data in the data set
   _nPtDataArrays = _dataSet->GetPointData()->GetNumberOfArrays();
   _nScalars = countNumberOfParameters(1);
   _nVectors = countNumberOfParameters(3);
   _scalarNames = getParameterNames(1,_nScalars);
   _vectorNames = getParameterNames(3,_nVectors);

   _cleanUpFileNames();

   std::cout<<"Sampling valid domain. . ."<<std::endl;
   _createValidityTexture();

   std::cout<<"Processing scalars:"<<std::endl;;
   for(int i = 0; i < _nScalars; i++){
      double bbox[6] = {0,0,0,0,0,0};
      //a bounding box
      _dataSet->GetBounds(bbox);
      
      FlowTexture texture;
      std::cout<<"   Scalar: "<<_scalarNames[i]<<std::endl;
      texture.setTextureDimension(_resolution[0],_resolution[1],_resolution[2]);
      texture.setBoundingBox(bbox);
      _curScalar.push_back(texture);
      std::cout<<"      Resampling data for scalar."<<std::endl;
      _resampleData(i,1);
      std::cout<<"         Writing data to texture."<<std::endl;
      writeScalarTexture(i);
      std::cout<<"      Cleaning up."<<std::endl;
      _curScalar.clear();
   }
   std::cout<<"Processing vectors:"<<std::endl;
   
   for(int i = 0; i < _nVectors; i++){ 
       double bbox[6] = {0,0,0,0,0,0};
      //a bounding box
      _dataSet->GetBounds(bbox);
      FlowTexture texture;
      std::cout<<"   Vector: "<<_vectorNames[i]<<std::endl;
      texture.setTextureDimension(_resolution[0],_resolution[1],_resolution[2]);
      texture.setBoundingBox(bbox);
      _velocity.push_back(texture);
      std::cout<<"      Resampling data for vector."<<std::endl;
      _resampleData(i,0);
      std::cout<<"         Writing data to texture."<<std::endl;
      writeVelocityTexture(i);
      std::cout<<"      Cleaning up."<<std::endl;
      _velocity.clear();
   }
   _cLocator->Delete();
   
}
///////////////////////////////////////////////
void VTKDataToTexture::_createValidityTexture()
{
    double bbox[6] = {0,0,0,0,0,0};
   //a bounding box
   _dataSet->GetBounds(bbox);

   //now create the black/white validity data
   float delta[3] = {0,0,0};
   //delta x
   delta[0] = (bbox[1] - bbox[0])/(_resolution[0]-1);
   //delta y
   delta[1] = (bbox[3] - bbox[2])/(_resolution[1]-1);
   //delta z
   delta[2] = (bbox[5] - bbox[4])/(_resolution[2]-1);

   double deltaDist = sqrt(delta[0]*delta[0] + delta[1]*delta[1]+delta[2]*delta[2]);
   //the bottom corner of the bbox/texture
   double pt[3] ={0,0,0};
   pt[0] = bbox[0];
   pt[1] = bbox[2];
   pt[2] = bbox[4];
   
   //we're going to create a cartiesian mesh that fills the
   //bounding box of the data. This is so that we can represent
   //the data as a texture
   //our original cell that contains our cartesian point
   vtkGenericCell* cell = vtkGenericCell::New();
   
   vtkIdType cellId,subId;
   double dist = 0;
   double closestPt[3];
   double pcoords[3];
   double* weights = 0;
   unsigned int i=0;
   unsigned int j=0;
   unsigned int k = 0;

   unsigned int nX = _resolution[0]-1;
   unsigned int nY = _resolution[1]-1;
   unsigned int nZ = _resolution[2]-1;

   unsigned int nPixels = _resolution[0]*_resolution[1]*_resolution[2];
   for(unsigned int l = 0; l < nPixels; l++){
      if((i == 0 || i == _resolution[0] - 1)||
        (j == 0 || j == _resolution[1] - 1)||
        (k == 0 || k == _resolution[2] - 1)){
        _validPt.push_back(false);
        i++;
     }else{
         pt[2] = bbox[4] + k*delta[2];
         pt[1] = bbox[2] + j*delta[1];
         pt[0] = bbox[0] + (i++)*delta[0];
         
         _cLocator->FindClosestPoint(pt,closestPt,
		                          cell,cellId,subId, dist);
       
         if(dist < deltaDist){
            weights = new double[cell->GetNumberOfPoints()];
            //check to see if this point is in
	         //the returned cell
           if(cell->EvaluatePosition(pt,0,subId,pcoords,dist,weights)){
              _validPt.push_back(true);
           }else{
              _validPt.push_back(false);
           }
           if(weights){
              delete [] weights;
              weights = 0;
           }
         }else{
            _validPt.push_back(false);
         }
         //check if it is time to reset
        
     }
     if((unsigned int)i > (unsigned int)nX){
            i = 0;
            j ++;
           if((unsigned int)j > (unsigned int)nY){
               j = 0;
               k ++;
               if((unsigned int)k > (unsigned int)nZ){
                  k = 0;
               }
           }
        }
   }
   
   
   cell->Delete();
}
/////////////////////////////////////////////////////////////////////
void VTKDataToTexture::_resampleData(int dataValueIndex,int isScalar)
{
   double bbox[6] = {0,0,0,0,0,0};
   //a bounding box
   _dataSet->GetBounds(bbox);
   
  

   //depending on the resolution we need to resample
   //the data
   float delta[3] = {0,0,0};
   //delta x
   delta[0] = (bbox[1] - bbox[0])/(_resolution[0]-1);
   //delta y
   delta[1] = (bbox[3] - bbox[2])/(_resolution[1]-1);
   //delta z
   delta[2] = (bbox[5] - bbox[4])/(_resolution[2]-1);

   //the bottom corner of the bbox/texture
   double pt[3] ={0,0,0};
   pt[0] = bbox[0];
   pt[1] = bbox[2];
   pt[2] = bbox[4];
   
   //we're going to create a cartiesian mesh that fills the
   //bounding box of the data. This is so that we can represent
   //the data as a texture
   //our original cell that contains our cartesian point
   vtkGenericCell* cell = vtkGenericCell::New();
   
   vtkIdType cellId,subId;
   double dist = 0;
   double closestPt[3];
   double pcoords[3];
   double* weights = 0;
   int index = 0;
   _curPt = 0;
   //resample the data into a cartesian grid
   for(int k = 0; k < _resolution[2]; k++){
      pt[2] = bbox[4] + k*delta[2];
      for(int j = 0; j < _resolution[1]; j++){
          pt[1] = bbox[2] +  j*delta[1];
          for(int i= 0; i < _resolution[0]; i++){
             pt[0] = bbox[0] + i*delta[0];
             if(_validPt.at(index++)){
                _cLocator->FindClosestPoint(pt,closestPt,
		                               cell,cellId,subId, dist);
                weights = new double[cell->GetNumberOfPoints()];
	             //check to see if this point is in
	             //the returned cell
                cell->EvaluatePosition(pt,0,subId,pcoords,dist,weights);
                _interpolateDataInCell(cell,weights,dataValueIndex,isScalar); 
                /*if(){
                   //use the weights to interpolate a velocity
		              //at our cell pt
		             
                }else{                     
                   //point isn't in a cell
	                //so set the texture data to 0
                   _addOutSideCellDomainDataToFlowTexture(dataValueIndex,isScalar);
                }*/
                if(weights){
                   delete [] weights;
                   weights = 0;
                }
             }else{
               //point isn't in a cell
	            //so set the texture data to 0
               _addOutSideCellDomainDataToFlowTexture(dataValueIndex,isScalar); 
             }
             _curPt++;
          } 
      }
   } 
   //cleanup?
   cell->Delete();
    
}
///////////////////////////////////////////
char* VTKDataToTexture::_cleanUpFileNames()
{
   char* ptr = 0;
   //char replace = "_";
   int len = 0;
   char* tempName = 0;
   int index = 0;
   for(int i = 0; i < _nScalars; i++){
      len = strlen(_scalarNames[i]);
      ptr = _scalarNames[i];
      tempName = new char[len +1];
      index = 0;
      for(int j = 0; j < len; j++){
         //skip spaces
         if(ptr[j] == ' '){
            continue;
         }
         //replace :'s w/ _'s
         if(ptr[j] ==':'){
            ptr[j] = '_';
         }
         tempName[index++] = ptr[j];
         
      }
      tempName[index] = '\0';
      strcpy(_scalarNames[i],tempName);
      if(tempName){
         delete [] tempName;
         tempName = 0;
      }
   }
   for(int i = 0; i < _nVectors; i++){
      len = strlen(_vectorNames[i]);
      ptr = _vectorNames[i];
      tempName = new char[len +1];
      index = 0;
      for(int j = 0; j < len; j++){
         //skip spaces
         if(ptr[j] == ' '){
            continue;
         }
         //replace :'s w/ _'s
         if(ptr[j] ==':'){
            ptr[j] = '_';
         }
         tempName[index++] = ptr[j];
         
      }
      tempName[index] = '\0';
      strcpy(_vectorNames[i],tempName);
      if(tempName){
         delete [] tempName;
         tempName = 0;
      }
   }
   return ptr;
}
///////////////////////////////////////////////////////////////////////
void VTKDataToTexture::_addOutSideCellDomainDataToFlowTexture(int index,
                                                       int isScalar)
{
   if(isScalar){
      FlowPointData data;
      
      data.setData(_scalarRanges.at(index)[0],0,0,0);
      data.setDataType(FlowPointData::SCALAR);
      _curScalar.at(0).addPixelData(data);
   }else{
      FlowPointData data;
      //the vectors
      //this is quantized in the range(-1,1)
      data.setData(127,127,127,0);
      data.setDataType(FlowPointData::VECTOR);
      _velocity.at(0).addPixelData(data); 
   }
}
///////////////////////////////////////////////////////////////////
void VTKDataToTexture::_interpolateDataInCell(vtkGenericCell* cell,
		                                    double* weights,
                                          int whichValue,
                                          int isScalar)
{
   //list of point ids in cell
   vtkIdList* pointIds = 0;
   
   //list of point ids in cell
   pointIds = cell->GetPointIds();
   
   //number of verts in the cell
   int nCellPts = cell->GetNumberOfPoints();
   
   if(isScalar){
      FlowPointData pixelData;// = new FlowPointData();;
      vtkDoubleArray* scalar = vtkDoubleArray::New();
      scalar->SetNumberOfComponents(1);
      scalar->SetNumberOfTuples(nCellPts);

      _extractTuplesForScalar(pointIds,scalar,whichValue);
      pixelData.setDataType(FlowPointData::SCALAR);
      _interpolatePixelData(pixelData,scalar,weights,nCellPts);
      _curScalar.at(0).addPixelData(pixelData);
      
      scalar->Delete();
   }else{
      FlowPointData pixelData;// = new FlowPointData();
      vtkDoubleArray* vector = vtkDoubleArray::New();
      vector->SetNumberOfComponents(3);
      vector->SetNumberOfTuples(nCellPts);

      _extractTuplesForVector(pointIds,vector,whichValue);
      pixelData.setDataType(FlowPointData::VECTOR);
      _interpolatePixelData(pixelData,vector,weights,nCellPts);
      _velocity.at(0).addPixelData(pixelData);
      vector->Delete();
   }
  
}
/////////////////////////////////////////////////////////////////
void VTKDataToTexture::_interpolatePixelData(FlowPointData& data,
                                        vtkDataArray* array,
                                        double* weights, 
                                        int npts)
{
   if(data.type()== FlowPointData::VECTOR){
      double vector[4];
      vector[0] = 0;
      vector[1] = 0;
      vector[2] = 0;
      double vectorData[3];
      for(int j = 0; j < npts; j++){
         array->GetTuple(j,vectorData);
         //the weighted sum of the velocities
         vector[0] += vectorData[0]*weights[j];
         vector[1] += vectorData[1]*weights[j];
         vector[2] += vectorData[2]*weights[j];
      }
      //calulate the magnitude
      double vMag = 0;
      double iMag = 0;
      vMag = sqrt(vector[0]*vector[0]
                +vector[1]*vector[1]
                +vector[2]*vector[2]);
         
      //inverse magnitude 
      if(vMag >= 1e-6){
         iMag = 1.0/vMag;
      }else{
         iMag = 0;
      }
      
      //normalize data
      vector[0] *= iMag;
      vector[1] *= iMag;
      vector[2] *= iMag;
      vector[3] = vMag;

      //quantize data
      vector[0] = 255*((vector[0] + 1)*.5);
      vector[1] = 255*((vector[1] + 1)*.5);
      vector[2] = 255*((vector[2] + 1)*.5);
      
      data.setData(vector[0],vector[1],vector[2],vector[3]);
   }else if(data.type()== FlowPointData::SCALAR){
      double scalarData;
      double scalar = 0;
      for(int j = 0; j < npts; j++){
         array->GetTuple(j,&scalarData);
         scalar += scalarData*weights[j];
      }
      //double distFactor = 0;
      //distFactor = (_distance.at(_curPt)==_maxDistFromCenter)?0:_maxDistFromCenter/(_maxDistFromCenter-_distance.at(_curPt));
      data.setData(scalar,0,0,0);
   }
}
///////////////////////////////////////////////////////////////////
void VTKDataToTexture::_extractTuplesForVector(vtkIdList* ptIds,
                                          vtkDataArray* vector,
                                          int whichVector)
{
   int vecNumber = 0;
   for ( int i = 0; i < _nPtDataArrays; i++ )
   {
      vtkDataArray * array = _dataSet->GetPointData()->GetArray(i);

      if ( array->GetNumberOfComponents() != 3){
         continue;
      }
      // also, ignore arrays of normals...
      if(array->GetNumberOfComponents() == 3 
         && (!strcmp(array->GetName(),"normals"))){
         continue; 
      }

      if(vecNumber != whichVector){
         vecNumber++;
         continue;
      }else{
         array->GetTuples(ptIds,vector);
         vecNumber++;
      }
   }
   
}
///////////////////////////////////////////////////////////////////
void VTKDataToTexture::_extractTuplesForScalar(vtkIdList* ptIds,
                                          vtkDataArray* scalar,
                                          int whichScalar)
{
   int scaleNum = 0;
   for(int i = 0; i < _nPtDataArrays; i++){
      vtkDataArray * array = _dataSet->GetPointData()->GetArray(i);

      if (array->GetNumberOfComponents() != 1){
         continue;
      }

      if(scaleNum != whichScalar){
         scaleNum++;
         continue;
      }else{
         array->GetTuples(ptIds,scalar);
         scaleNum++;
      }
   }
}
/////////////////////////////////////////////////////////////////////////
int VTKDataToTexture::countNumberOfParameters( const int numComponents )
{
   int numParameters = 0;

   // count the number of paraneters containing numComponents components...
   for ( int i=0; i < _nPtDataArrays; i++ )
   {
      vtkDataArray * array = _dataSet->GetPointData()->GetArray(i);

      if ( array->GetNumberOfComponents() != numComponents )
      {
         continue;
      }

      // also, ignore arrays of normals...
      if ( numComponents == 3 && ( ! strcmp( array->GetName(), "normals" ) ) )
      {
         continue; 
      }

      numParameters++;
   }
   return numParameters;
}
/////////////////////////////////////////////////////////////////////
char ** VTKDataToTexture::getParameterNames( const int numComponents, 
                                       const int numParameters )
{
   char ** name = new char * [numParameters];
   int ii = 0;

   for ( int i=0; i < _nPtDataArrays; i++ )
   {
      vtkDataArray * array = _dataSet->GetPointData()->GetArray(i);

      if ( array->GetNumberOfComponents() != numComponents )
      {
         continue;
      }

      // also, ignore arrays of normals...
      if ( numComponents == 3 && ( ! strcmp( array->GetName(), "normals" ) ) )
      {
         continue; 
      }
      if(numComponents == 1){
         double* range = new double[2];
         array->GetRange(range,-1);
         _scalarRanges.push_back(range);
         
      }else if(numComponents == 3){
         double* range = new double[2];
         array->GetRange(range,-1);
         _vectorRanges.push_back(range);
      }
      name[ii] = new char [ strlen( array->GetName() ) + 1 ];
      strcpy( name[ii], array->GetName() );
      ii++;
   }
   return name;
}

////////////////////////////////////////////////////////////
void VTKDataToTexture::writeVelocityTexture(int whichVector)
{

   char posName[1024];

   if(_outputDir){
      strcpy(posName,_outputDir);
   }else{
      strcpy(posName,"./");
   }
   strcat(posName,"/vectors/");
#ifdef WIN32
   //check to see if the directory exists
   if( _mkdir( posName ) == 0 ){
      std::cout<<"Created directory: "<<posName<<std::endl;
      std::cout<<"VTKDataToTexture::writeVelocityTexture"<<std::endl;
   }else{
      std::cout<<"Directory: "<<posName<<" exists."<<std::endl;
      std::cout<<"VTKDataToTexture::writeVelocityTexture"<<std::endl;
   }
#endif
   strcat(posName,_vectorNames[whichVector]);
   
   if(_vFileName){
      strcat(posName,_vFileName);
   }else{
      strcat(posName,"flow");
   }

   strcat(posName,".rgb");
   double velRange[2] = {0,0};
   velRange[0] = _vectorRanges.at(whichVector)[0];
   velRange[1] = _vectorRanges.at(whichVector)[1];
   _velocity.at(0).writeFlowTexture(posName,velRange);

}
///////////////////////////////////////////
void VTKDataToTexture::writeScalarTexture(int whichScalar)
{
   char name[1024]; 

   if(_outputDir){
      strcpy(name,_outputDir);
   }else{
      strcpy(name,"./");
   }
   strcat(name,"/scalars/");
#ifdef WIN32
   //check to see if the directory exists
   if( _mkdir( name) == 0 ){
      std::cout<<"Created directory: "<<name<<std::endl;
      std::cout<<"VTKDataToTexture::writeVelocityTexture"<<std::endl;
   }else{
      std::cout<<"Directory: "<<name<<" exists."<<std::endl;
      std::cout<<"VTKDataToTexture::writeVelocityTexture"<<std::endl;
   }
#endif
   strcat(name,_scalarNames[whichScalar]);
   
   if(_vFileName){
      strcat(name,_vFileName);
   }else{
      strcat(name,"scalar");
   }

   strcat(name,".rgb");
   double scalarRange[2] = {0,0};
   scalarRange[0] = _scalarRanges.at(whichScalar)[0];
   scalarRange[1] = _scalarRanges.at(whichScalar)[1];
   _curScalar.at(0).writeFlowTexture(name,scalarRange);  
}
	
