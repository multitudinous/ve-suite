#include "cfdTextureBasedModel.h"
#include "cfdTextureManager.h"
#include <fstream>
#include <iostream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
////////////////////////////////////////////////
//Constructors                                //
////////////////////////////////////////////////
cfd3DTextureBasedModel::cfd3DTextureBasedModel()
{
   _activeScalar = 0;
   _activeVector = 0;
   _paramFileName = 0;
   _volumeVizNode = 0;
}
////////////////////////////////////////////////////////////////////////////////
cfd3DTextureBasedModel::cfd3DTextureBasedModel(const cfd3DTextureBasedModel& tbm)
{
   _activeScalar = tbm._activeScalar;
   _activeVector = tbm._activeVector;
   _volumeVizNode = tbm._volumeVizNode;
   int nScalarNames = 0;
   int nVectorNames = 0;
   nScalarNames = tbm._scalarNames.size();
   nVectorNames = tbm._vectorNames.size();

   _scalarNames.clear();
   _vectorNames.clear();

   for(int i = 0; i < nScalarNames; i++){
      _scalarNames.push_back(tbm._scalarNames.at(i));
   }
   for(int i = 0; i < nVectorNames; i++){
      _vectorNames.push_back(tbm._vectorNames.at(i));
   }

   int nScalars = 0; 
   int nVectors = 0; 
   _vectorDataTextures.clear();
   _scalarDataTextures.clear();
   for(int i = 0; i < nScalars; i++){
      _scalarDataTextures.push_back(tbm._scalarDataTextures.at(i));
   } 
   for(int i = 0; i < nVectors; i++){
      _vectorDataTextures.push_back(tbm._vectorDataTextures.at(i));
   } 
   if(!_paramFileName){
      delete [] _paramFileName;
      _paramFileName = 0;
   }
   _paramFileName = new char[strlen(tbm._paramFileName)+1];
   strcpy(_paramFileName,tbm._paramFileName);
}
/////////////////////////////////////////////////
//Destructor                                   //
/////////////////////////////////////////////////
cfd3DTextureBasedModel::~cfd3DTextureBasedModel()
{
   _scalarNames.clear();
   _vectorNames.clear();
   _scalarDataTextures.clear();
   _vectorDataTextures.clear();
   if(_volumeVizNode){
      delete _volumeVizNode;
      _volumeVizNode = 0;
   }
   if(_activeScalar){
      delete _activeScalar;
      _activeScalar = 0;
   }
   if(_activeVector){
      delete _activeVector;
      _activeVector = 0;
   }
   if(_paramFileName){
      delete _paramFileName;
      _paramFileName = 0;
   }
}
/////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::SetParameterFileName(char* filename)
{
   if(!_paramFileName){
      delete [] _paramFileName;
      _paramFileName = 0;
   }
   _paramFileName = new char[strlen(filename)+1];
   strcpy(_paramFileName,filename);
}
//////////////////////////////////////////////
void cfd3DTextureBasedModel::InitializeModel()
{
   if(!_paramFileName){
      std::cout<<"Parameter file not set in cfdTextureBasedModel!!"<<std::endl;
      return;
   }
   int numObjects;
   char textLine[ 256 ];
   std::ifstream input;
   input.open( _paramFileName);
   input >> numObjects; 
   input.getline( textLine, 256 );   //skip past remainder of line

   //vprDEBUG(vprDBG_ALL,1) << " Number of Obejcts in Interactive Geometry : " << numObjects << std::endl  << vprDEBUG_FLUSH;
   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      //vprDEBUG(vprDBG_ALL,1) << "Id of object in Interactive Geometry : " << id << std::endl << vprDEBUG_FLUSH;
      input.getline( textLine, 256 );   //skip past remainder of line
      if ( id == 15 ){
         int numTextureDescriptionFiles = 0;
          input >> numTextureDescriptionFiles;
          input.getline(textLine,256);
          char textureDescriptionFile[256];
          for(int i = 0; i < numTextureDescriptionFiles; i++){
             input>>textureDescriptionFile;
             input.getline(textLine,256);
             _createTextureManager(textureDescriptionFile);
          }
      
      }else{
         _paramReader.ContinueRead(input,id);
      }
   }
}
/////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::_createTextureManager(char* filename)
{
   cfdTextureManager tm;
   std::ifstream fin(filename);   char name[256];   if(fin.is_open()){       int numFiles = 0;      fin>>numFiles;      for(int i = 0; i < numFiles; i++){         fin>>name;         tm.addFieldTextureFromFile(name);      }
      if(tm.GetDataType(0)==cfdTextureManager::SCALAR){
         AddScalarTextureManager(tm,filename);
      }else{
         AddVectorTextureManager(tm,filename);
      }
   }else{
      std::cout<<"Couldn't open file in cfd3DTextureBasedModel::CreateTextureManager!"<<std::endl;
      return;
   }
}
//////////////////////////////////////////////
float* cfd3DTextureBasedModel::GetScalarBoundingBox()
{
   if(_scalarDataTextures.size()){
      if(_activeScalar)return _activeScalar->getBoundingBox();
   }else{
      return 0;
   }
   return 0;
}
////////////////////////////////////////////////////
float* cfd3DTextureBasedModel::GetVectorBoundingBox()
{
   if(_vectorDataTextures.size()){
      if(_activeVector)return _activeVector->getBoundingBox();
   }else{
      return 0;
   }
   return 0;
}
///////////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::AddVectorTextureManager(cfdTextureManager tm,
		                                          char* vectorName)
{
   char* ptr = 0;
   ptr = strrchr(vectorName,'.');
   if(ptr){
      ptr = "\0";
   }
   _vectorNames.push_back(vectorName);
   _vectorDataTextures.push_back(tm);
   _activeVector = &_vectorDataTextures.at(0);
}
///////////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::AddScalarTextureManager(cfdTextureManager tm,
	                                             char* scalarName)
{
   char* ptr = 0;
   ptr = strrchr(scalarName,'.');
   if(ptr){
      ptr = "\0";
   }
   _scalarNames.push_back(scalarName);
   _scalarDataTextures.push_back(tm);
   _activeScalar = &_scalarDataTextures.at(0);
}
//////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::SetActiveScalar(char* scalarName)
{
   int nScalars = _scalarNames.size();
   for(int i = 0; i < nScalars; i++){
      if(!strcmp(scalarName,_scalarNames.at(i))){
         _activeScalar = &_scalarDataTextures.at(i);
	 break;
      } 
   }
}
//////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::SetActiveVector(char* vectorName)
{
   int nVectors = _vectorNames.size();
   for(int i = 0; i < nVectors; i++){
      if(!strcmp(vectorName,_vectorNames.at(i))){
         _activeVector = &_vectorDataTextures.at(i);
	 break;
      } 
   }
}
/////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::SetActiveScalar(int whichScalar)
{
   _activeScalar = &_scalarDataTextures.at(whichScalar);
}
/////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::SetActiveVector(int whichVector)
{
   _activeVector = &_vectorDataTextures.at(whichVector);
}

///////////////////////////////////////////////////////////////
cfdTextureManager* cfd3DTextureBasedModel::GetActiveScalarTexture()
{
   return _activeScalar;
}
///////////////////////////////////////////////////////////////////
cfdTextureManager* cfd3DTextureBasedModel::GetActiveVectorTexture()
{
   return _activeVector;
}
////////////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfd3DTextureBasedModel::GetVolumeVisualizaionWithScalar(int whichScalar)
{
   if(_scalarDataTextures.size()){
      if(!_volumeVizNode){
         _volumeVizNode = new cfdVolumeVisualization();
      }
      _volumeVizNode->SetNumberofSlices(100);
      _volumeVizNode->SetSliceAlpha(.5);
      _volumeVizNode->SetTextureManager(&_scalarDataTextures.at(whichScalar));
      return _volumeVizNode;
   }else{
      std::cout<<"Invalid scalar!!!"<<std::endl;
      std::cout<<"cfd3DTextureBasedModel::GetVolumeVisualizationWithScalar()"<<std::endl;
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfd3DTextureBasedModel::GetVolumeVisualizationWithVector(int whichVector)
{
    if(_vectorDataTextures.size()){
      if(!_volumeVizNode){
         _volumeVizNode = new cfdVolumeVisualization();
      }
      _volumeVizNode->SetNumberofSlices(100);
      _volumeVizNode->SetSliceAlpha(.5);
      _volumeVizNode->SetTextureManager(&_vectorDataTextures.at(whichVector));
      return _volumeVizNode;
   }else{
      std::cout<<"Invalid scalar!!!"<<std::endl;
      std::cout<<"cfd3DTextureBasedModel::GetVolumeVisualizationWithVector()"<<std::endl;
   }
   return 0;
}

////////////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfd3DTextureBasedModel::GetVolumeVisualizaionWithScalar(char* name)
{
   std::cout<<"GetVolumeVisualizationWithScalar(name) not implemented yet!"<<std::endl;
   std::cout<<"Use index instead of name!"<<std::endl;
   return _volumeVizNode;
   /*if(_vectorDataTextures.size()){
      if(!_volumeVizNode){
         _volumeVizNode = new cfdVolumeVisualization();
      }
      _volumeVizNode->SetNumberofSlices(100);
      _volumeVizNode->SetSliceAlpha(.5);
      _volumeVisNode->SetTextureManager(_vectorDataTextures.at(whichVector);
      return _volumeVizNode;
   }else{
      std::cout<<"Invalid scalar!!!"<<std::endl;
      std::"cfd3DTextureBasedModel::GetVolumeVisualizationWithVector()"<<std::endl;
   }
   return 0;*/
}

////////////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfd3DTextureBasedModel::GetVolumeVisualizationWithVector(char* name)
{
   std::cout<<"GetVolumeVisualizationWithVector(name) not implemented yet!"<<std::endl;
   std::cout<<"Use index instead of name!"<<std::endl;
   return _volumeVizNode;
}
/////////////////////////////////////////////////////////////
cfd3DTextureBasedModel& cfd3DTextureBasedModel::operator=(const
	                                  cfd3DTextureBasedModel& tbm)
{
   if(&tbm != this){
      _activeScalar = tbm._activeScalar;
      _activeVector = tbm._activeVector;
      _volumeVizNode = tbm._volumeVizNode;

      int nScalarNames = 0;
      int nVectorNames = 0;
      nScalarNames = tbm._scalarNames.size();
      nVectorNames = tbm._vectorNames.size();

      _scalarNames.clear();
      _vectorNames.clear();

      for(int i = 0; i < nScalarNames; i++){
         _scalarNames.push_back(tbm._scalarNames.at(i));
      }
      for(int i = 0; i < nVectorNames; i++){
         _vectorNames.push_back(tbm._vectorNames.at(i));
      }

      int nScalars = 0; 
      int nVectors = 0; 
      _vectorDataTextures.clear();
      _scalarDataTextures.clear();
      for(int i = 0; i < nScalars; i++){
         _scalarDataTextures.push_back(tbm._scalarDataTextures.at(i));
      } 
      for(int i = 0; i < nVectors; i++){
         _vectorDataTextures.push_back(tbm._vectorDataTextures.at(i));
      } 
      if(!_paramFileName){
         delete [] _paramFileName;
         _paramFileName = 0;
      }
      _paramFileName = new char[strlen(tbm._paramFileName)+1];
      strcpy(_paramFileName,tbm._paramFileName);
   }
   return *this;
}
#endif