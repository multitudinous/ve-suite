#include "cfdTextureBasedVizHandler.h"

#include "cfdCommandArray.h"
#include "cfdReadParam.h"
#include "cfdDCS.h"
#include "cfdCursor.h"
#include "cfdNavigate.h"
#include "cfdVolumeVisualization.h"
#include "cfdTextureManager.h"
#include <fstream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::cfdTextureBasedVizHandler()
{
   _paramFile = 0;
   _cmdArray = 0;
   _worldDCS = 0;
   _nav = 0;
   _cursor  = 0;
   _activeVolumeVizNode = 0;
   _activeTM = 0;
}

///////////////////////////////////////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::cfdTextureBasedVizHandler(const cfdTextureBasedVizHandler& tbvh)
{
   int nNodes = tbvh._volumeVisNodes.size();
   for(int i = 0; i < nNodes; i++) {
      _volumeVisNodes.push_back(tbvh._volumeVisNodes.at(i));   
   }
   _paramFile = new char[strlen(tbvh._paramFile)+1];
   strcpy(_paramFile,tbvh._paramFile);
   _cmdArray = tbvh._cmdArray;
   _worldDCS = tbvh._worldDCS;
   _nav = tbvh._nav;
   _cursor = tbvh._cursor;
   _activeVolumeVizNode = tbvh._activeVolumeVizNode;
   _activeTM = tbvh._activeTM;
}
///////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::~cfdTextureBasedVizHandler()
{
   _volumeVisNodes.clear();
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
   if(_activeVolumeVizNode){
      delete _activeVolumeVizNode;
      _activeVolumeVizNode = 0;
   }
}
/////////////////////////////////////////////
void cfdTextureBasedVizHandler::InitScene()
{
}
//////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PreFrameUpdate()
{
   if ( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) != -1 ){
      vprDEBUG(vprDBG_ALL,2) 
         << "preFrame: id = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID )
         << ", iso = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE )
         << ", scalarIndex = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_SC )
         << ", min = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_MIN )
         << ", max = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_MAX )
         << ", geo_state = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_GEO_STATE )
         << ", pre_state = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_PRE_STATE )
         << ", teacher_state = " << _cmdArray->GetCommandValue( cfdCommandArray::CFD_TEACHER_STATE )
         << std::endl << vprDEBUG_FLUSH;
   }
}
///////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetParameterFile(char* paramFile)
{
   if(_paramFile){
      delete [] _paramFile;
      _paramFile = 0;
   }
   _paramFile = new char[strlen(paramFile)+1];
   strcpy(_paramFile,paramFile);
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCommandArray(cfdCommandArray* cmdArray)
{
   _cmdArray = cmdArray;
}
//////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetWorldDCS(cfdDCS* dcs)
{
   _worldDCS = dcs;
}
////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetNavigate(cfdNavigate* navigate)
{
   _nav = navigate;
}
//////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCursor(cfdCursor* cursor)
{
   _cursor = cursor;
}
//////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetActiveTextureManager(cfdTextureManager* tm)
{
   if(tm != _activeTM){
      _activeTM = tm;
      if(_activeVolumeVizNode){
         _activeVolumeVizNode->SetTextureManager(_activeTM);
         _activeVolumeVizNode->CreateNode();
         //need to move/switch
         if(_worldDCS){
             //this may not be right
             ((osg::Group*)_worldDCS->GetRawNode())->addChild(_activeVolumeVizNode->GetVolumeVisNode().get());
          }
      }
   }
}
/////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetActiveVolumeVizNode()
{
   return _activeVolumeVizNode;
}
/////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetVolumeVizNode(int whichModel)
{
   
   return &_volumeVisNodes.at(whichModel);
}
////////////////////////////////////////////////////
bool cfdTextureBasedVizHandler::InitVolumeVizNodes()
{
   if(!_paramFile){
      std::cout<<"Invalid parameter file!"<<std::endl;
      std::cout<<"Cannot initialize texture based models!!"<<std::endl;
      std::cout<<"cfdTextureBasedVizHandler::InitTextureBasedModels()"<<std::endl;
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
         cfdVolumeVisualization volVizNode;
         volVizNode.SetNumberofSlices(100);
         volVizNode.SetSliceAlpha(.5);
         _volumeVisNodes.push_back(volVizNode);
         
      }else{
         readParam.ContinueRead(input,id);
      }
    }
    if(_volumeVisNodes.size()){
       _activeVolumeVizNode = &_volumeVisNodes.at(0);
    }
    return true;
}
/////////////////////////////////////////////////////////////////////////////////////////////////
cfdTextureBasedVizHandler& cfdTextureBasedVizHandler::operator=(const cfdTextureBasedVizHandler& tbvh)
{
   if(&tbvh != this){
      int nModels = tbvh._volumeVisNodes.size();
      for(int i = 0; i < nModels; i++) {
         _volumeVisNodes.push_back(tbvh._volumeVisNodes.at(i));   
      }
      _paramFile = new char[strlen(tbvh._paramFile)+1];
      strcpy(_paramFile,tbvh._paramFile);
      _cmdArray = tbvh._cmdArray;
      _worldDCS = tbvh._worldDCS;
      _nav = tbvh._nav;
      _cursor = tbvh._cursor;
      _activeVolumeVizNode = tbvh._activeVolumeVizNode;
      _activeTM = tbvh._activeTM;
   }
   return *this;
}
#endif
