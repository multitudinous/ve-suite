#include "cfdTextureBasedModelHandler.h"

#include "cfdCommandArray.h"
#include "cfdReadParam.h"
#include "cfdDCS.h"
#include "cfdCursor.h"
#include "cfdNavigate.h"
#include <fstream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdTextureBasedModelHandler::cfdTextureBasedModelHandler()
{
   _paramFile = 0;
   _cmdArray = 0;
   _worldDCS = 0;
   _nav = 0;
   _cursor  = 0;
   _activeTextureModel = 0;
}

///////////////////////////////////////////////////////////////////////////////////////////
cfdTextureBasedModelHandler::cfdTextureBasedModelHandler(const cfdTextureBasedModelHandler& tbvh)
{
   int nModels = tbvh._textureModels.size();
   for(int i = 0; i < nModels; i++) {
      _textureModels.push_back(tbvh._textureModels.at(i));   
   }
   _paramFile = new char[strlen(tbvh._paramFile)+1];
   strcpy(_paramFile,tbvh._paramFile);
   _cmdArray = tbvh._cmdArray;
   _worldDCS = tbvh._worldDCS;
   _nav = tbvh._nav;
   _cursor = tbvh._cursor;
   _activeTextureModel = tbvh._activeTextureModel;
}
///////////////////////////////////////////////////////////
cfdTextureBasedModelHandler::~cfdTextureBasedModelHandler()
{
   _textureModels.clear();
   if(_paramFile){
      delete [] _paramFile;
      _paramFile = 0;
   }
   if(_cmdArray){
      delete  _cmdArray;
      _cmdArray = 0;
   }
   if(_worldDCS){
      delete  _worldDCS;
      _worldDCS = 0;
   }
   if(_nav){
      delete  _nav;
      _nav = 0;
   }
   if(_cursor){
      delete  _cursor;
      _cursor = 0;
   }
   if(_activeTextureModel){
      delete _activeTextureModel;
      _activeTextureModel = 0;
   }
}
/////////////////////////////////////////////
void cfdTextureBasedModelHandler::InitScene()
{
}
//////////////////////////////////////////////////
void cfdTextureBasedModelHandler::PreFrameUpdate()
{
}
///////////////////////////////////////////////////////////////////
void cfdTextureBasedModelHandler::SetParameterFile(char* paramFile)
{
   if(_paramFile){
      delete [] _paramFile;
      _paramFile = 0;
   }
   _paramFile = new char[strlen(paramFile)+1];
   strcpy(_paramFile,paramFile);
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedModelHandler::SetCommandArray(cfdCommandArray* cmdArray)
{
   _cmdArray = cmdArray;
}
//////////////////////////////////////////////////////////
void cfdTextureBasedModelHandler::SetWorldDCS(cfdDCS* dcs)
{
   _worldDCS = dcs;
}
////////////////////////////////////////////////////////////////////
void cfdTextureBasedModelHandler::SetNavigate(cfdNavigate* navigate)
{
   _nav = navigate;
}
//////////////////////////////////////////////////////////////
void cfdTextureBasedModelHandler::SetCursor(cfdCursor* cursor)
{
   _cursor = cursor;
}

/////////////////////////////////////////////////////////////////////////////////
cfd3DTextureBasedModel* cfdTextureBasedModelHandler::GetActiveTextureBasedModel()
{
   return _activeTextureModel;
}
/////////////////////////////////////////////////////////////////////////////////////////
cfd3DTextureBasedModel* cfdTextureBasedModelHandler::GetTextureBasedModel(int whichModel)
{
   return &_textureModels.at(whichModel);
}
////////////////////////////////////////////////////////////////
bool cfdTextureBasedModelHandler::InitTextureBasedModelsModels()
{
   if(!_paramFile){
      std::cout<<"Invalid parameter file!"<<std::endl;
      std::cout<<"Cannot initialize texture based models!!"<<std::endl;
      std::cout<<"cfdTextureBasedModelHandler::InitTextureBasedModels()"<<std::endl;
      return false;
   }
   cfdReadParam readParam;
   int numObjects;
   char text[ 256 ];
   //char textLine[ 256 ];
   std::ifstream input;
   input.open( _paramFile);
   input >> numObjects; 
   input.getline( text, 256 );   //skip past remainder of line

    for( int i = 0; i < numObjects; i++ ){
      int id;
      input >> id;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 15 ){
         cfd3DTextureBasedModel textureModel;
         textureModel.SetParameterFileName(_paramFile);
         textureModel.InitializeModel();
         _textureModels.push_back(textureModel);
      }else{
         readParam.ContinueRead(input,id);
      }
   }
   if(_textureModels.size()){
      _activeTextureModel = &_textureModels.at(0);
      return true;
   }else{
      return false;
   }
}
/////////////////////////////////////////////////////////////////////////////////////////////////
cfdTextureBasedModelHandler& cfdTextureBasedModelHandler::operator=(const cfdTextureBasedModelHandler& tbvh)
{
   if(&tbvh != this){
      int nModels = tbvh._textureModels.size();
      for(int i = 0; i < nModels; i++) {
         _textureModels.push_back(tbvh._textureModels.at(i));   
      }
      _paramFile = new char[strlen(tbvh._paramFile)+1];
      strcpy(_paramFile,tbvh._paramFile);
      _cmdArray = tbvh._cmdArray;
      _worldDCS = tbvh._worldDCS;
      _nav = tbvh._nav;
      _cursor = tbvh._cursor;
      _activeTextureModel = tbvh._activeTextureModel;
   }
   return *this;
}
#endif
