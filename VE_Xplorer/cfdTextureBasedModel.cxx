#include "cfdTextureBasedModel.h"
#include "cfdTextureManager.h"
////////////////////////////////////////////////
//Constructors                                //
////////////////////////////////////////////////
cfd3DTextureBasedModel::cfd3DTextureBasedModel()
{
   _activeScalar = 0;
   _activeVector = 0;
}
////////////////////////////////////////////////////////////////////////////////
cfd3DTextureBasedModel::cfd3DTextureBasedModel(const cfd3DTextureBasedModel& tbm)
{
   _activeScalar = tbm._activeScalar;
   _activeVector = tbm._activeVector;

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
}
//////////////////////////////////////////////
float* cfd3DTextureBasedModel::GetScalarBoundingBox()
{
   if(_scalarDataTextures.size()){
      if(_activeScalar)return _activeScalar->getBoundingBox();
   }else{
      return 0;
   }
}
////////////////////////////////////////////////////
float* cfd3DTextureBasedModel::GetVectorBoundingBox()
{
   if(_vectorDataTextures.size()){
      if(_activeVector)return _activeVector->getBoundingBox();
   }else{
      return 0;
   }
}
///////////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::AddVectorTextureManager(cfdTextureManager tm,
		                                     char* vectorName)
{
   _vectorNames.push_back(vectorName);
   _vectorDataTextures.push_back(tm);
   _activeVector = &_vectorDataTextures.at(0);
}
///////////////////////////////////////////////////////////////////////
void cfd3DTextureBasedModel::AddScalarTextureManager(cfdTextureManager tm,
	                                             char* scalarName)
{
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
///////////////////////////////////////////////////////////////
cfdTextureManager* cfd3DTextureBasedModel::GetActiveVectorTexture()
{
   return _activeVector;
}
   
/////////////////////////////////////////////////////////////
cfd3DTextureBasedModel& cfd3DTextureBasedModel::operator=(const
	                                  cfd3DTextureBasedModel& tbm)
{
   if(&tbm != this){
      _activeScalar = tbm._activeScalar;
      _activeVector = tbm._activeVector;

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
   }
   return *this;
}
