/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdTextureBasedVizHandler.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdTextureBasedVizHandler.h"
#ifdef VE_PATENTED
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG 
#include "VE_Xplorer/TextureBased/cfdVolumeVisNodeHandler.h"
#include "VE_Xplorer/TextureBased/cfdTextureDataSet.h"
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdVolumeVisualization.h"


#include <osg/State>
#include <osgUtil/SceneView>
#include <osgDB/WriteFile>
#endif
#include "VE_Xplorer/XplorerHandlers/TBBBoxEH.h"
#include "VE_Xplorer/XplorerHandlers/TBUpdateScalarRangeEH.h"
#include "VE_Xplorer/XplorerHandlers/TBUpdateSolutionEH.h"
#include "VE_Xplorer/XplorerHandlers/TBActivateEH.h"
#include "VE_Xplorer/XplorerHandlers/cfdModelHandler.h"
#include "VE_Xplorer/XplorerHandlers/cfdWriteTraverser.h"
#include "VE_Xplorer/XplorerHandlers/cfdCommandArray.h"
#include "VE_Xplorer/XplorerHandlers/cfdReadParam.h"
#include "VE_Xplorer/XplorerHandlers/cfdCursor.h"
#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdGraphicsObject.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnum.h"
#include "VE_Xplorer/XplorerHandlers/cfdModel.h"

#include "VE_Xplorer/SceneGraph/cfdSwitch.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"

#include "VE_Open/XML/Command.h"

#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"
#include <fstream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include "VE_Xplorer/TextureBased/cfdScalarVolumeVisHandler.h"
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdOSGTransferShaderManager.h"

#include "VE_Xplorer/TextureBased/cfdVectorVolumeVisHandler.h"
#include "VE_Xplorer/TextureBased/cfdOSGAdvectionShaderManager.h"

vprSingletonImp( VE_TextureBased::cfdTextureBasedVizHandler );
using namespace VE_TextureBased;
using namespace VE_Xplorer;
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::cfdTextureBasedVizHandler()
{
   _animationDelay = 1.0;
   _appTime = 0.0;
   _paramFile = '\0';
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
   
   activeVisNodeHdlr = 0;
   _textureBaseSelected = false;
   _activeTDSet = 0;
   _svvh = 0;
   _vvvh = 0;

   _eventHandlers[std::string("TB_ACTIVATE")] = new VE_EVENTS::TextureBasedActivateEventHandler();
   _eventHandlers[std::string("TB_ACTIVE_SOLUTION")] = new VE_EVENTS::TextureBasedUpdateSolutionEventHandler();
   _eventHandlers[std::string("TB_SCALAR_RANGE")] = new VE_EVENTS::TextureBasedUpdateScalarRangeEventHandler();
   _eventHandlers[std::string("TB_BBOX_DISPLAY")] = new VE_EVENTS::TextureBasedBoundingBoxEventHandler();
  
}
///////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::CleanUp( void )
{
   if ( !_paramFile.empty() )
   {
      _paramFile.clear();
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

   if(_svvh){
      delete _svvh;
      _svvh = 0;
   }

   if(_vvvh){
      delete _svvh;
      _vvvh = 0;
   }

   std::map< std::string,VE_EVENTS::TextureBasedEventHandler*>::iterator pos;
   for ( pos = _eventHandlers.begin(); pos != _eventHandlers.end(); )
   {
      delete pos->second;
      _eventHandlers.erase( pos++ );
   }
}
/////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateShaders()
{
   if(_activeTM){
      _activeTM->CalculateUpdateTime(_appTime,_animationDelay);
      if(_activeTM->TimeToUpdate())
      {
         _activeTM->getNextField();
      }
      if(_activeTM->GetDataType(0) == cfdTextureManager::SCALAR){
        _updateScalarVisHandler();
      }else if(_activeTM->GetDataType(0) == cfdTextureManager::VECTOR){
        _updateVectorVisHandler();
      }

   }
   _updateShaderState();
}
//////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateVisualization()
{
   //update analysis techniques
   //if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == X_CONTOUR||
    //  _cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == X_VECTOR){
      if(_activeVolumeVizNode && _currentBBox){
         //create an x plane
         double xplane[4] = {1,0,0,0};
         float alpha = 0;//(float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the xplane positions
         if(alpha < .5)
         {
            xplane[3] = _currentBBox[0] + alpha*(_currentBBox[1] - _currentBBox[0]);
            xplane[3] *=-1.0;
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE_MIN,xplane);
         }else{
            xplane[3] = _currentBBox[0] + alpha*(_currentBBox[1] - _currentBBox[0]);
            xplane[0] *=-1.0;
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE_MAX,xplane);
         }
      }
      //_cleared = false;
   /*}else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Y_CONTOUR||
           _cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Y_VECTOR){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an y plane
         double yplane[4] = {0,1,0,0};
         float alpha = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the yplane positions
         if(alpha <.5)
         {
            yplane[3] = _currentBBox[2] + alpha*(_currentBBox[3] - _currentBBox[2]);
            yplane[3] *= -1.0;
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE_MIN,yplane);
         }else{
            yplane[1] *= -1.0;
            yplane[3] = _currentBBox[2] + alpha*(_currentBBox[3] - _currentBBox[2]);
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE_MAX,yplane);
         }
      }
      _cleared = false;
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Z_CONTOUR||
           _cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == Z_VECTOR){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an z plane
         double zplane[4] = {0,0,1,0};
         float alpha = (float)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         alpha /= 100.0;
         //get the zplane positions
         if(alpha < .5)
         {
            zplane[3] = _currentBBox[4] + alpha*(_currentBBox[5] - _currentBBox[4]);
            zplane[3] *= -1.0;
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE_MIN,zplane);
         }else{
            zplane[3] = _currentBBox[4] + alpha*(_currentBBox[5] - _currentBBox[4]);
            zplane[2] *= -1.0;
            _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE_MAX,zplane);
         }
      }
      _cleared = false;
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ARBITRARY){
      if(_activeVolumeVizNode&&_currentBBox){
         //create an arbitrary plane
         double arbPlane[4] = {0,0,0,0};
         //not sure how this is going to work w/ the gui!!!!
         arbPlane
         _activeVolumeVizNode->AddClipPlane(cfdVolumeVisualization::ARBITRARY,arbPlane);
      }
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_STOP){
      if(_activeTM){
         _activeTM->setPlayMode(cfdTextureManager::STOP);
      }
      _cleared = false;
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_PLAY){
      if(_activeTM){
         _activeTM->setDirection(1);
         _activeTM->setPlayMode(cfdTextureManager::PLAY);
      }
      _cleared = false;
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_DURATION){
      if(_svvh){
         double duration = (double) _cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE);
         unsigned int nTimesteps = _activeTM->numberOfFields();
         _animationDelay = duration/((double)nTimesteps); 
         cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
         if(sShader)
         {
            sShader->SetDelayTime(_animationDelay);
         }
      }
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_SET_FRAME){
      if(_activeTM){
         _activeTM->setPlayMode(cfdTextureManager::STOP);
         _activeTM->SetCurrentFrame((unsigned int)_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE));
      }
      _cleared = false;
   }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_FORWARD){
      if(_activeTM){
         _activeTM->setDirection(1);
         _activeTM->setPlayMode(cfdTextureManager::STOP);
         int curFrame = _activeTM->getNextFrame();
         if(_svvh){
            cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
            if(sShader)
            {
               sShader->SetCurrentTransientTexture(curFrame,false);
            }
         }
         
      }
      _cleared = false;
   }else if(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == TRANSIENT_BACKWARD){
      if(_activeVolumeVizNode&&_activeTM){
         _activeTM->setDirection(-1);
         _activeTM->setPlayMode(cfdTextureManager::STOP);
         int curFrame = _activeTM->getNextFrame();
         if(_svvh){
            cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
            if(sShader)
            {
               sShader->SetCurrentTransientTexture(curFrame,false);
            }
         }
         if(_vvvh){
            _vvvh->SetCurrentTransientTexture(curFrame,false);
         }
      }
      _cleared = false;
   }*/
}
//////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateGraph()
{
   //place vv node on the graph
   if (/* _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) != CLEAR_ALL && !_cleared*/1)
   {
      if(_activeVolumeVizNode){
#ifdef _OSG
         osg::ref_ptr<osg::Group> tParent = dynamic_cast<osg::Group*>(_parent->GetRawNode());
         osg::ref_ptr<osg::Switch> tVV = _activeVolumeVizNode->GetVolumeVisNode();
         if(!tParent->containsNode(tVV.get()))
         {
            tParent->addChild(tVV.get());
            _cleared = false;
         }
#endif
      }
   }else if (/* _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CLEAR_ALL*/0 ){ 
      if(_parent){
         //need to remove the clip planes
         if(_activeVolumeVizNode){
            _activeVolumeVizNode->ResetClipPlanes();
            ((osg::Group*)_parent->GetRawNode())->removeChild(_activeVolumeVizNode->GetVolumeVisNode().get());
         }
         _activeTM = 0;
         _cleared = true;
      }
   }
}
///////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCurrentTime(double time)
{
  _appTime = time; 
}
////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateBoundingBox(bool value)
{
   if(_activeVolumeVizNode && activeVisNodeHdlr)
   {
      if(value)
      {
         activeVisNodeHdlr->TurnOnBBox();
      }
      else
      {
         activeVisNodeHdlr->TurnOffBBox();
      }
   }
}
////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateActiveTextureManager()
{
   if(_activeTDSet)
   {
     _activeVolumeVizNode =  _activeTDSet->GetVolumeVisNode();
     _activeTM = _activeTDSet->GetActiveTextureManager();
     _updateShaders();
     
     if(!_currentBBox)
     {
         _currentBBox = new float[6];
     }
     _currentBBox[0] = _activeTM->getBoundingBox()[0];
     _currentBBox[1] = _activeTM->getBoundingBox()[1];
     _currentBBox[2] = _activeTM->getBoundingBox()[2];
     _currentBBox[3] = _activeTM->getBoundingBox()[3];
     _currentBBox[4] = _activeTM->getBoundingBox()[4];
     _currentBBox[5] = _activeTM->getBoundingBox()[5];
   }
}
////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PreFrameUpdate()
{
   if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      if( cfdModelHandler::instance()->GetActiveModel()->GetVECommand() )
      {
         std::map<std::string,VE_EVENTS::TextureBasedEventHandler*>::iterator currentEventHandler;
         VE_XML::Command* tbvizCommand = cfdModelHandler::instance()->GetActiveModel()->GetVECommand();
         currentEventHandler = _eventHandlers.find( tbvizCommand->GetCommandName() );
         if ( currentEventHandler != _eventHandlers.end() )
         {
            vprDEBUG(vesDBG,0) << "|\tExecuting: "<< tbvizCommand->GetCommandName() 
                                 << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tbvizCommand );
            //_updateShaders();
            _updateVisualization();
            _updateGraph();
         }
      }
   }
   
}
///////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetParameterFile(std::string paramFile)
{
   _paramFile = paramFile;
}
//////////////////////////////////////////////////////////
cfdPBufferManager* cfdTextureBasedVizHandler::GetPBuffer()
{
   if(_pbm){
      return _pbm;
   }
   return 0;
}
////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateShaderState()
{
   //first check which option is active
   /*if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ADVECTION_SHADER){
      if(_vvvh){
         cfdOSGAdvectionShaderManager* aShader = _vvvh->GetAdvectionShaderManager();
         if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == DYE_TRANSLATION){
            float dyeTranslation[3] = {0.0,0.0,0.0};
            dyeTranslation[0] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_SC);
            dyeTranslation[0] /= 100.0;
            dyeTranslation[1] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_MIN);
            dyeTranslation[1] /= 100.0;
            dyeTranslation[2] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_MAX);
            dyeTranslation[2] /= 100.0;
            aShader->UpdateDyeTranslation(dyeTranslation);
         }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == NOISE_SCALE){
            //uniform scaling
            //this will change to allow seperate scaling of injeciton materials
            float noiseScale[3] = {1.0,1.0,1.0};
            noiseScale[0] = (float)_cmdArray->GetCommandValue(cfdCommandArray::CFD_SC);
            noiseScale[0] /= 100.0;
            noiseScale[1] = noiseScale[0];
            noiseScale[2] = noiseScale[0];
            aShader->UpdateNoiseScale(noiseScale);
         }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ISO_VALUE) == WEIGHT){
            float weights[2] = {.8,.2};
            weights[0] = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_SC) );
            weights[0] /= 100.0;
            weights[1] = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_MIN) );
            weights[1] /= 100.0;
            float whichMat = static_cast< float >( _cmdArray->GetCommandValue(cfdCommandArray::CFD_MAX) );
            aShader->UpdateWeight(weights, static_cast< int >( whichMat ) );
         }
         _vvvh->EnableDecorator();
         activeVisNodeHdlr = _vvvh;
      }
   }else if( _cmdArray->GetCommandValue( cfdCommandArray::CFD_ID ) == SHOW_TEXTURE_BBOX){
      //display the bbox
      if(_activeVolumeVizNode&&activeVisNodeHdlr){
         int showBBox = (int)_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE );
         if(showBBox){
            activeVisNodeHdlr->TurnOnBBox();
         }else{
            activeVisNodeHdlr->TurnOffBBox();
         }
      }
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ISOSURFACE){
      if(_svvh){
         cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
         if(sShader)
         {
            float range = 0;
            sShader->ActivateIsoSurface();
            range = (float)(_cmdArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ))/100.0;
            sShader->SetIsoSurfaceValue(range);
         }
      }
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == CHANGE_SCALAR_RANGE){
      if(_svvh){
        cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
           if(sShader)
           {
              float range[2];
              //may need to re-think this
              sShader->DeactivateIsoSurface();
              range[0] = (float)(_cmdArray->GetCommandValue( cfdCommandArray::CFD_MIN ))/100.0;
              range[1] = (float)(_cmdArray->GetCommandValue( cfdCommandArray::CFD_MAX ))/100.0;
              sShader->SetScalarRange(range);
           }
      }
      _svvh->EnableDecorator();
      activeVisNodeHdlr = _svvh;
   }else if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == VOLUME_SHADER){
      if(_svvh)     
      {
         _svvh->EnableDecorator();
         activeVisNodeHdlr = _svvh;
      }
   }else if(activeVisNodeHdlr && _activeTM){
      if(!activeVisNodeHdlr->IsThisActive()){
         activeVisNodeHdlr->EnableDecorator();
      }
   }*/
}
////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateScalarRange(float* range)
{
   if(_svvh)
   {
      _updateScalarVisHandler();

      cfdScalarShaderManager* sShader = _svvh->GetScalarShaderManager();
      if(sShader)
      {
         sShader->DeactivateIsoSurface();
         sShader->SetScalarRange(range);
      }
      _svvh->EnableDecorator();
      activeVisNodeHdlr = _svvh;
   }
}
//////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PingPongTextures()
{
   if(_vvvh && _vvvh->IsThisActive()){
      _vvvh->PingPongTextures();
   }
}
//////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetPBuffer(cfdPBufferManager* pbm)
{
   if(_pbm != pbm)
      _pbm = pbm;
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCommandArray(cfdCommandArray* cmdArray)
{
   _cmdArray = cmdArray;
}
//////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetWorldDCS(VE_SceneGraph::cfdDCS* dcs)
{
   if(_worldDCS != dcs)
      _worldDCS = dcs;
}
///////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetParentNode(VE_SceneGraph::cfdGroup* parent)
{
   if(_parent != parent)
      _parent = parent;
}
////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetNavigate(cfdNavigate* navigate)
{
   if(_nav != navigate)
      _nav = navigate;
}
//////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCursor(cfdCursor* cursor)
{
   if(_cursor != cursor)
      _cursor = cursor;
}
///////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetActiveTextureDataSet(cfdTextureDataSet* tds)
{
   if(tds != _activeTDSet)
   {
      _activeTDSet = tds;
   }
   /*_activeVolumeVizNode =  _activeTDSet->GetVolumeVisNode();
   _activeTM = _activeTDSet->GetActiveTextureManager();

   if(_activeVolumeVizNode && _activeTM){
     //_activeVolumeVizNode->GetVolumeVisNode();
     if(!_currentBBox)
     {
         _currentBBox = new float[6];
     }
     _currentBBox[0] = _activeTM->getBoundingBox()[0];
     _currentBBox[1] = _activeTM->getBoundingBox()[1];
     _currentBBox[2] = _activeTM->getBoundingBox()[2];
     _currentBBox[3] = _activeTM->getBoundingBox()[3];
     _currentBBox[4] = _activeTM->getBoundingBox()[4];
     _currentBBox[5] = _activeTM->getBoundingBox()[5];
   }*/
}
/////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateScalarVisHandler()
{
   if(_activeTM && _activeVolumeVizNode){
      if(!_svvh){
         _svvh = new cfdScalarVolumeVisHandler();
         
      }
      _svvh->SetBoundingBox(_activeTM->getBoundingBox());
      _svvh->SetSwitchNode(_activeVolumeVizNode->GetVolumeVisNode().get());
      _svvh->SetTextureScale(_activeVolumeVizNode->GetTextureScale(),false);
      _svvh->SetCenter(_activeVolumeVizNode->GetBBoxCenter());
      _svvh->SetAttachNode(_activeVolumeVizNode->GetDecoratorAttachNode().get());
      _svvh->SetTextureManager(_activeTM);
      _svvh->Init();
      if(!_svvh->IsThisActive())
         _svvh->EnableDecorator();
      //activeVisNodeHdlr = dynamic_cast<cfdScalarVolumeVisHandler*>(_svvh);
   }
}
/////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateVectorVisHandler()
{
   if(_activeTM && _activeVolumeVizNode && _pbm){
      if(!_vvvh){
         _vvvh = new cfdVectorVolumeVisHandler();
         _vvvh->SetPBufferManager(_pbm);
      }
      _vvvh->SetBoundingBox(_activeTM->getBoundingBox());
      _vvvh->SetSwitchNode(_activeVolumeVizNode->GetVolumeVisNode().get());
      _vvvh->SetTextureScale(_activeVolumeVizNode->GetTextureScale(),false);
      _vvvh->SetCenter(_activeVolumeVizNode->GetBBoxCenter());
      
      _vvvh->SetAttachNode(_activeVolumeVizNode->GetDecoratorAttachNode().get());
      _vvvh->SetTextureManager(_activeTM);
      _vvvh->Init();
      if(!_vvvh->IsThisActive())
         _vvvh->EnableDecorator();
      //_vvvh->EnableDecorator();
      //activeVisNodeHdlr = dynamic_cast<cfdVectorVolumeVisHandler*>(_vvvh);
   }
}
/////////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetActiveVolumeVizNode()
{
   return _activeVolumeVizNode;
}
/////////////////////////////////////////////////////////////////////////////////////////
// Can this function be removed????
cfdVolumeVisualization* cfdTextureBasedVizHandler::GetVolumeVizNode( int whichModel )
{
   std::cout << whichModel << std::endl;
   return 0;
}
///////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::ViewTextureBasedVis(bool trueFalse)
{
   _textureBaseSelected = trueFalse;
}
#endif// VE_PATENTED
#endif

