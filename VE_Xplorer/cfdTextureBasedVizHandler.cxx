#include "cfdTextureBasedVizHandler.h"

#include "cfdCommandArray.h"
#include "cfdReadParam.h"
#include "cfdDCS.h"
#include "cfdCursor.h"
#include "cfdNavigate.h"
#include "cfdVolumeVisualization.h"
#include "cfdTextureManager.h"
#include "cfdGraphicsObject.h"
#include "cfdEnum.h"
#include "cfdSwitch.h"
#include <fstream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Group>
#include <osgUtil/SceneView>
#include <osg/State>
#include "cfdScalarVolumeVisHandler.h"
#include "cfdVectorVolumeVisHandler.h"

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
   _parent = 0;
   _currentBBox = 0;
   _cleared = true;
   _pbm = 0;
   _svvh = 0;
#ifdef CFD_USE_SHADERS
   _vvvh = 0;
#endif
   _visOptionSwitch = new cfdSwitch();
   _visOptionSwitch->SetName("VVis Switch");
   cfdGroup* scalarGroup = new cfdGroup();
   scalarGroup->SetName("Scalar Vis");
   cfdGroup* vectorGroup = new cfdGroup();
   vectorGroup->SetName("Vector Vis");
   _visOptionSwitch->AddChild(scalarGroup);
   _visOptionSwitch->AddChild(vectorGroup);
   _visOptionSwitch->SetVal(0);
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
   _parent = tbvh._parent;
   _sceneView = tbvh._sceneView;
   _pbm = tbvh._pbm;
   _visOptionSwitch = new cfdSwitch(*tbvh._visOptionSwitch);
   _currentBBox = new float[6];
   _currentBBox[0] = tbvh._currentBBox[0];
   _currentBBox[1] = tbvh._currentBBox[1];
   _currentBBox[2] = tbvh._currentBBox[2];
   _currentBBox[3] = tbvh._currentBBox[3];
   _currentBBox[4] = tbvh._currentBBox[4];
   _currentBBox[5] = tbvh._currentBBox[5];
   _cleared = tbvh._cleared;
   _svvh = new cfdScalarVolumeVisHandler(*tbvh._svvh);
#ifdef CFD_USE_SHADERS
   _vvvh = new cfdVectorVolumeVisHandler(*tbvh._vvvh);
#endif
}
///////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::~cfdTextureBasedVizHandler()
{
   for ( unsigned int i = 0; i < _volumeVisNodes.size(); ++i )
   {
      delete _volumeVisNodes.at( 0 );
   }
   _volumeVisNodes.clear();

   if ( _paramFile )
   {
      delete [] _paramFile;
      _paramFile = 0;
   }

   if ( _cmdArray )
   {
      delete  _cmdArray;
      _cmdArray = 0;
   }

   if ( _worldDCS )
   {
      delete  _worldDCS;
      _worldDCS = 0;
   }

   if ( _nav ) 
   {
      delete  _nav;
      _nav = 0;
   }

   if ( _cursor )
   {
      delete  _cursor;
      _cursor = 0;
   }
   
   if ( _activeVolumeVizNode )
   {
      delete _activeVolumeVizNode;
      _activeVolumeVizNode = 0;
   }

   if ( _parent )
   {
      delete _parent;
      _parent = 0;
   }

   if ( _currentBBox )
   {
      delete [] _currentBBox;
      _currentBBox = 0;
   }
   if( _visOptionSwitch){
      delete  _visOptionSwitch;
       _visOptionSwitch = 0;
   }
   if(_svvh){
      delete _svvh;
      _svvh = 0;
   }
#ifdef CFD_USE_SHADERS
   if(_vvvh){
      delete _svvh;
      _vvvh = 0;
   }
#endif
}
//////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PreFrameUpdate()
{
#ifdef CFD_USE_SHADERS
   //check if we are viewing vectors
   if(_visOptionSwitch->GetRawNode()->getValue(1)){
      if(_vvvh){
         _vvvh->PingPongTextures();
      }
   }
#endif
   //this may need to change 
   if ( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_ACTIVE ){
      //set the transient flag in the callback
      if(_activeVolumeVizNode){
         //testing switch stuff
         //_activeVolumeVizNode->SetPlayMode(cfdVolumeVisualization::PLAY);
         
         //_activeVolumeVizNode->EnableVolumeShader();
         //_activeVolumeVizNode->DeactivateVisualBBox();
         /*if(_svvh){
            _svvh->EnableVolumeShader();
         }*/
      }
   }
   if ( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) != CLEAR_ALL && !_cleared){
      //need to make sure the node is on the graph
      if(_parent->SearchChild(_visOptionSwitch) < 0){
         _parent->AddChild(_visOptionSwitch);
         _cleared = false;
      }
   }
   //biv --need to figure out the correct enum to grab!!!
   //this is the one for all precomputed which isn't what we want but *_CONTOUR doesn't
   //seem to pick up!!!1
   if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == X_CONTOURS){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an x plane
         double xplane[4] = {1,0,0,0};
         float alpha = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the xplane positions
         xplane[3] = _currentBBox[0] + alpha*(_currentBBox[1] - _currentBBox[0]);
         xplane[3] *=-1.0;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE,xplane);
         _cleared = false;
      }
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Y_CONTOURS){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an y plane
         double yplane[4] = {0,1,0,0};
         float alpha = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the yplane positions
         yplane[3] = _currentBBox[2] + alpha*(_currentBBox[3] - _currentBBox[2]);
         yplane[3] *= -1.0;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE,yplane);
         _cleared = false;
      }
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Z_CONTOURS){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an z plane
         double zplane[4] = {0,0,1,0};
         float alpha = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the zplane positions
         zplane[3] = _currentBBox[4] + alpha*(_currentBBox[5] - _currentBBox[4]);
         zplane[3] *= -1.0;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE,zplane);
         _cleared = false;
      }
   /*}else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ARBITRARY){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an arbitrary plane
         double arbPlane[4] = {0,0,0,0};
         //not sure how this is going to work w/ the gui!!!!
         arbPlane
         _activeVolumeVizNode->AddClipPlane(cfdVolumeVisualization::ARBITRARY,arbPlane);
      }*/
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == SHOW_TEXTURE_BBOX){
      //display the bbox
      if(_activeVolumeVizNode){
         int showBBox = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         if(showBBox){
            _svvh->TurnOnBBox();
             
         }else{
            _svvh->TurnOffBBox();
         }
      }
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_STOP){
      if(_activeVolumeVizNode){
         _activeVolumeVizNode->SetPlayMode(cfdVolumeVisualization::STOP);
         _cleared = false;
      }
   }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_FORWARD){
      if(_activeVolumeVizNode){
         _activeVolumeVizNode->SetPlayDirection(cfdVolumeVisualization::FORWARD);
         _cleared = false;
      }
   }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_BACKWARD){
      if(_activeVolumeVizNode){
         _activeVolumeVizNode->SetPlayDirection(cfdVolumeVisualization::BACKWARD);
         _cleared = false;
      }
   }else if ( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TEXTURE_BASED_SHADERS){
      if(_activeVolumeVizNode){
         int useShaders = _cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
      

      }
   }else if ( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL ){ 
      if(_parent){
         //need to remove the clip planes
         if(_activeVolumeVizNode){
           //we can do this because osg checks to see if the plane exists!!!
            _activeVolumeVizNode->RemoveClipPlane(cfdVolumeVisualization::XPLANE);
            _activeVolumeVizNode->RemoveClipPlane(cfdVolumeVisualization::YPLANE);
            _activeVolumeVizNode->RemoveClipPlane(cfdVolumeVisualization::ZPLANE);
            _activeVolumeVizNode->RemoveClipPlane(cfdVolumeVisualization::ARBITRARY);
            //remove the volviz node from the tree. . .
            _parent->RemoveChild(_visOptionSwitch);
         }
         _activeTM = 0;
         _cleared = true;
      }
   }
}
////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetSceneView(osgUtil::SceneView* sv)
{
   _sceneView = sv;
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
//////////////////////////////////////////////////////////
cfdPBufferManager* cfdTextureBasedVizHandler::GetPBuffer()
{
   if(_pbm){
      return _pbm;
   }
   return 0;
}
#ifdef CFD_USE_SHADERS
//////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PingPongTextures()
{
  //don't need this, i think
}
#endif
//////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetPBuffer(cfdPBufferManager* pbm)
{
   _pbm = pbm;
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
///////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetParentNode(cfdGroup* parent)
{
   _parent = parent;
   if(_visOptionSwitch){
     _parent->AddChild(_visOptionSwitch);
   }
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
   if((tm != _activeTM)||!_cleared){
      _activeTM = tm;
      if(!_currentBBox){
         _currentBBox = new float[6];
      }
      _currentBBox[0] = _activeTM->getBoundingBox()[0];
      _currentBBox[1] = _activeTM->getBoundingBox()[1];
      _currentBBox[2] = _activeTM->getBoundingBox()[2];
      _currentBBox[3] = _activeTM->getBoundingBox()[3];
      _currentBBox[4] = _activeTM->getBoundingBox()[4];
      _currentBBox[5] = _activeTM->getBoundingBox()[5];

      if(_activeVolumeVizNode){
         _activeVolumeVizNode->SetTextureManager(_activeTM);
         _activeVolumeVizNode->CreateNode();

         if(_activeTM->GetDataType(0) == cfdTextureManager::SCALAR){
            _updateScalarVisHandler();
         }else if(_activeTM->GetDataType(0) == cfdTextureManager::VECTOR){
            _updateVectorVisHandler();
         }
         //need to move/switch
         /*if(_parent){
            if(!((osg::Group*)_parent->GetRawNode())->containsNode(_activeVolumeVizNode->GetVolumeVisNode().get())&&!_cleared){
               //this may not be right
               ((osg::Group*)_parent->GetRawNode())->addChild(_activeVolumeVizNode->GetVolumeVisNode().get());
               _cleared = false;
            }
         }*/
      }
   }
}
/////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateScalarVisHandler()
{
   if(_activeTM&&_activeVolumeVizNode&&_visOptionSwitch){
      if(!_svvh){
         _svvh = new cfdScalarVolumeVisHandler();
         _svvh->SetBoundingBox(_activeTM->getBoundingBox());
         _svvh->SetVolumeVizNode(_activeVolumeVizNode->GetVolumeVisNode().get());
      }
      _svvh->SetTextureManager(_activeTM);
      _svvh->Init();
      osg::ref_ptr<osg::Group> scalarChild = 
         dynamic_cast<osg::Group*>(_visOptionSwitch->GetChild(0)->GetRawNode());
      if(!scalarChild->containsNode(_svvh->GetVisualization())){
        //this may not be right
        scalarChild->addChild(_svvh->GetVisualization());
      }
   }
}
/////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateVectorVisHandler()
{
#ifdef CFD_USE_SHADERS
   if(_activeTM&&_activeVolumeVizNode&&_visOptionSwitch){
      if(!_vvvh){
         _vvvh = new cfdVectorVolumeVisHandler();
         _vvvh->SetBoundingBox(_activeTM->getBoundingBox());
         _vvvh->SetVolumeVizNode(_activeVolumeVizNode->GetVolumeVisNode().get());
      }
      _vvvh->SetTextureManager(_activeTM);
      _vvvh->Init();
      osg::ref_ptr<osg::Group> vectorChild = 
         dynamic_cast<osg::Group*>(_visOptionSwitch->GetChild(1)->GetRawNode());
      if(!vectorChild->containsNode(_vvvh->GetVisualization())){
        //this may not be right
        vectorChild->addChild(_vvvh->GetVisualization());
      }
   }
#endif
}
/////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetActiveVolumeVizNode()
{
   return _activeVolumeVizNode;
}
/////////////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetVolumeVizNode(int whichModel)
{
   return _volumeVisNodes.at(whichModel);
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

   for( int i = 0; i < numObjects; i++ )
   {
      int id;
      input >> id;
      input.getline( text, 256 );   //skip past remainder of line
      if ( id == 15 )
      {
         cfdVolumeVisualization* volVizNode = new cfdVolumeVisualization();
         volVizNode->SetNumberofSlices(100);
         volVizNode->SetSliceAlpha(.5);
         if(_sceneView){
            volVizNode->SetState(_sceneView->getState());
         }
         _volumeVisNodes.push_back(volVizNode);
      }
      else
      {
         readParam.ContinueRead(input,id);
      }
   }
   
   if ( _volumeVisNodes.size() > 0 )
   {
       _activeVolumeVizNode = _volumeVisNodes.at(0);
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
      _parent = tbvh._parent;
      _sceneView = tbvh._sceneView;
      _pbm = tbvh._pbm;
      _visOptionSwitch = tbvh._visOptionSwitch;
      if(!_currentBBox){
         _currentBBox = new float[6];
      }
      _svvh = tbvh._svvh;
#ifdef CFD_USE_SHADERS
      _vvvh = tbvh._vvvh;
#endif
      _currentBBox[0] = tbvh._currentBBox[0];
      _currentBBox[1] = tbvh._currentBBox[1];
      _currentBBox[2] = tbvh._currentBBox[2];
      _currentBBox[3] = tbvh._currentBBox[3];
      _currentBBox[4] = tbvh._currentBBox[4];
      _currentBBox[5] = tbvh._currentBBox[5];
   }
   return *this;
}
#endif
