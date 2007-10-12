/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include <ves/xplorer/cfdTextureBasedVizHandler.h>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG 
#include <ves/xplorer/volume/cfdVolumeVisNodeHandler.h>
#include <ves/xplorer/volume/cfdTextureDataSet.h>
#include <ves/xplorer/volume/cfdTextureManager.h>
#include <ves/xplorer/volume/cfdVolumeVisualization.h>


#include <osg/State>
//#include <osgUtil/SceneView>
#include <osgDB/WriteFile>
#endif
#include <ves/xplorer/event/TBTransientDurationUpdateEH.h>
#include <ves/xplorer/event/TBTransientModeUpdateEH.h>
#include <ves/xplorer/event/TBIsosurfaceUpdateEH.h>
#include <ves/xplorer/event/TBIsosurfaceEnableEH.h>
#include <ves/xplorer/event/TBClipPlaneEH.h>
#include <ves/xplorer/event/TBBBoxEH.h>
#include <ves/xplorer/event/TBUpdateScalarRangeEH.h>
#include <ves/xplorer/event/TBUpdateSolutionEH.h>
#include <ves/xplorer/event/TBActivateEH.h>
#include <ves/xplorer/event/TBSetActiveShaderManagerEH.h>
#include <ves/xplorer/event/TBSliceNumberUpdateEH.h>
#include <ves/xplorer/event/TBPhongShadingEnableEH.h>
#include <ves/xplorer/event/TBPreIntegrateEH.h>

#include <ves/xplorer/ModelHandler.h>
#include <ves/xplorer/event/viz/cfdCommandArray.h>
#include <ves/xplorer/event/viz/cfdReadParam.h>
#include <ves/xplorer/event/viz/cfdGraphicsObject.h>
#include <ves/xplorer/event/viz/cfdEnum.h>
#include <ves/xplorer/event/viz/cfdModel.h>

#include <ves/open/xml/Command.h>

#include <ves/xplorer/cfdDebug.h>
#include <fstream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <ves/xplorer/volume/cfdScalarVolumeVisHandler.h>
#include <ves/xplorer/volume/cfdScalarShaderManager.h>
#include <ves/xplorer/volume/cfdOSGTransferShaderManager.h>

#include <ves/xplorer/volume/cfdVectorVolumeVisHandler.h>
#include <ves/xplorer/volume/cfdOSGAdvectionShaderManager.h>

vprSingletonImpLifetime( VE_TextureBased::cfdTextureBasedVizHandler, 140 );
using namespace VE_TextureBased;
using namespace VE_Xplorer;
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdTextureBasedVizHandler::cfdTextureBasedVizHandler()
{
   _animationDelay = 0.0001f;
   _appTime = 0.0;
   _paramFile = '\0';
   _cmdArray = 0;
   _activeVolumeVizNode = 0;
   _activeTM = 0;
   _parent = 0;
   _currentBBox = 0;
   _cleared = true;
   _pbm = 0;
   
   _activeVisNodeHdlr = 0;
   _textureBaseSelected = false;
   _activeTDSet = 0;
   _svvh = 0;
   _vvvh = 0;
   m_isMaster = false;

   
   _eventHandlers[std::string("TB_SET_ACTIVE_SHADER_MANAGER")] = new VE_EVENTS::TextureBasedSetActiveShaderManagerEventHandler();
   _eventHandlers[std::string("TB_ACTIVATE")] = new VE_EVENTS::TextureBasedActivateEventHandler();
   _eventHandlers[std::string("TB_ACTIVE_SOLUTION")] = new VE_EVENTS::TextureBasedUpdateSolutionEventHandler();
   _eventHandlers[std::string("TB_SCALAR_RANGE")] = new VE_EVENTS::TextureBasedUpdateScalarRangeEventHandler();
   _eventHandlers[std::string("TB_BBOX_DISPLAY")] = new VE_EVENTS::TextureBasedBoundingBoxEventHandler();
   _eventHandlers[std::string("TB_ROI_UPDATE")] = new VE_EVENTS::TextureBasedClipPlaneEventHandler();
   _eventHandlers[std::string("TB_ISOSURFACE_ENABLE")] = new VE_EVENTS::TextureBasedIsosurfaceEnableEventHandler();
   _eventHandlers[std::string("TB_UPDATE_ISOSURFACE")] = new VE_EVENTS::TextureBasedIsosurfaceUpdateEventHandler();
   _eventHandlers[std::string("TB_TRANSIENT_MODE_UPDATE")] = new VE_EVENTS::TextureBasedTransientModeUpdateEventHandler();
   _eventHandlers[std::string("TB_TRANSIENT_DURATION_UPDATE")] = new VE_EVENTS::TextureBasedTransientDurationUpdateEventHandler();
   _eventHandlers[std::string("TB_UPDATE_NUMBER_SLICE_PLANES")] = new VE_EVENTS::TextureBasedSliceNumberUpdateEventHandler();
   _eventHandlers[std::string("TB_PHONG_SHADING_ENABLE")] = new VE_EVENTS::TextureBasedPhongShadingEnableEventHandler();
   _eventHandlers[std::string("TB_FULL_PREINTEGRATE_UPDATE")] = new VE_EVENTS::TextureBasedPreIntegrateEnableEventHandler();

}
///////////////////////////////////////////////
cfdTextureBasedVizHandler::~cfdTextureBasedVizHandler( void )
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
//////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetMasterNode( bool isMaster )
{
    m_isMaster = isMaster;
}
/////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateShaders()
{
   if(_activeTM )
   {
      
      if(_activeTM->GetDataType(0) == cfdTextureManager::SCALAR)
      {
        _updateScalarVisHandler();
      }
      else if(_activeTM->GetDataType(0) == cfdTextureManager::VECTOR)
      {
        _updateVectorVisHandler();
      }
   }
}
//////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateTransientFrame()
{
    if( _activeTM )
    {
        _activeTM->CalculateUpdateTime(_appTime,_animationDelay);
        if( _activeTM->TimeToUpdate() )
        {
            _activeTM->getNextFrame();
        }
    }
}
////////////////////////////////////////////////////////////////////////
cfdTextureManager* cfdTextureBasedVizHandler::GetActiveTextureManager()
{
   return _activeTM;
}
//////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateIsosurface(double value)
{
   if(_svvh)
   {
	   cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
      if(sShader)
      {
         sShader->FastTransferFunctionUpdate();
         sShader->ActivateIsoSurface();
         sShader->SetIsoSurfaceValue(value);
      }
   }
}
////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::EnsureIsosurface(bool onOff)
{
   if(_svvh)
   {
	   cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
      if(sShader)
      {
         if(onOff)
         {
            sShader->ActivateIsoSurface();
         }
         else
         {
            sShader->DeactivateIsoSurface();
         }
         sShader->FullTransferFunctionUpdate();
         sShader->EnsureScalarRange();
      }
   }
}
//////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::EnsurePhongShading(bool onOff)
{
   if(_svvh)
   {
	   cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
      if(sShader)
      {
         //this is hard coded and will need to change--biv
         if(onOff)
         {
            sShader->SetActiveShaderProgram("Phong Lit Volume Render");
         }
         else
         {
            sShader->SetActiveShaderProgram("Basic Volume Render");
         }
         sShader->FullTransferFunctionUpdate();
         sShader->EnsureScalarRange();
         
      }
   }
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateClipPlane(std::string planeCoordinate,
                                           std::string planeDirection,
                                           double alpha)
{   
   double plane[4] = {0,0,0,0};
   if(planeCoordinate == "X")
   {
      plane[0] = 1.0;
      plane[3] = _currentBBox[0] + alpha*(_currentBBox[1] - _currentBBox[0]);
      //get the xplane positions
      if(planeDirection == "Positive")
      {
         plane[3] *=-1.0;
         plane[3] += .001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE_MIN,plane);
      }
      else if(planeDirection == "Negative")
      {
         plane[0] *=-1.0;
         plane[3] -= .001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::XPLANE_MAX,plane);
      }
   }
   else if(planeCoordinate == "Y")
   {
      plane[1] = 1.0;
      plane[3] = _currentBBox[2] + alpha*(_currentBBox[3] - _currentBBox[2]);
      //get the yplane positions
      if(planeDirection == "Positive")
      {
         plane[3] *= -1.0;
         plane[3] +=.001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE_MIN,plane);
      }
      else if(planeDirection == "Negative")
      {
         plane[1] *= -1.0;
         plane[3] -= .001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::YPLANE_MAX,plane);
      }
   }
   else if(planeCoordinate == "Z")
   {    
      //create an z plane
      plane[2] = 1.0;
      plane[3] = _currentBBox[4] + alpha*(_currentBBox[5] - _currentBBox[4]);
      //get the zplane positions
      if(planeDirection == "Positive")
      {
         plane[3] *= -1.0;
         plane[3] +=.001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE_MIN,plane);
      }
      else if(planeDirection == "Negative")
      {
         plane[2] *= -1.0;
         plane[3] -=.001;
         _activeVolumeVizNode->UpdateClipPlanePosition(cfdVolumeVisualization::ZPLANE_MAX,plane);
      }
   }
   else
   {
      std::cout<<"Invalid Clipping plane coordinate specified!!"<<std::endl;
      std::cout<<"cfdTextureBasedVizHandler::UpdateClipPlane"<<std::endl;
   }
}
///////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateNumberOfSlicePlanes(unsigned int nSlices)
{
	if(_activeVolumeVizNode)
	{
		_activeVolumeVizNode->SetNumberOfSlices(nSlices);
	}
}
////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetTransientDirection(std::string direction)
{
   if(_activeTM)
   {
      _activeTM->setDirection((direction == "Forward")?1:-1);
   }
}

////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::StopTransientVisualization()
{
   if(_activeTM)
   {
      _activeTM->SetPlayMode("Stop");
   }

}
////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::PlayTransientVisualization()
{
   if(_activeTM)
   {
      _activeTM->SetPlayMode("Play");
      SetTransientDirection("Forward");
   }
}
////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdateTransientDuration(double duration)
{
   if(_activeTM)
   {
      if(_svvh)
      {
         ///duration calculation
         unsigned int nTimesteps = _activeTM->numberOfFields();
         _animationDelay = duration/((double)nTimesteps); 
		   cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
         if(sShader)
         {
            sShader->SetDelayTime(_animationDelay);
         }
      }
   }
}
/////////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::StepTransientVisualization(std::string direction)
{
   try
   {
      StopTransientVisualization();
      SetTransientDirection(direction);

      int curFrame = _activeTM->getNextFrame();
      if(_svvh)
      {
		  cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
        if(sShader)
        {
           sShader->SetCurrentTransientTexture(curFrame,true);
        }
     }
   }
   catch(...)
   {
      std::cout<<"Texture Manager not set!!"<<std::endl;
      std::cout<<" cfdTextureBasedVizHandler::StepTransientVisualization"<<std::endl;
   }
}
///////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetCurrentFrame(unsigned int frame)
{
   try
   {
      StopTransientVisualization();
      _activeTM->setPlayMode(cfdTextureManager::STOP);
      _activeTM->SetCurrentFrame(frame);
   }
   catch(...)
   { 
      std::cout<<"Texture Manager not set!!"<<std::endl;
      std::cout<<" cfdTextureBasedVizHandler::SetCurrentFrame"<<std::endl;
   }
}
//////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::_updateVisualization()
{

   /*if(_cmdArray->GetCommandValue(cfdCommandArray::CFD_ID) == ARBITRARY){
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
   if(_activeVolumeVizNode)
   {
#ifdef _OSG
      osg::ref_ptr<osg::Group> tParent = _parent.get();
      osg::ref_ptr<osg::Switch> tVV = _activeVolumeVizNode->GetVolumeVisNode();
      if(!tParent->containsNode(tVV.get()))
      {
         tParent->addChild(tVV.get());
#endif
      }
   }
}
/////////////////////////////////////////
void cfdTextureBasedVizHandler::ClearAll()
{
   if(_parent.valid())
   {
      //need to remove the clip planes
      if(_activeVolumeVizNode)
      {
         _activeVolumeVizNode->ResetClipPlanes();
			_parent->removeChild( _activeVolumeVizNode->GetVolumeVisNode().get() );
         _activeVolumeVizNode = 0;
      }
      _activeTM = 0;
      _activeVolumeVizNode = 0;
      _parent = 0;
      _currentBBox = 0;
      _cleared = true;
      _pbm = 0;
   
      _activeVisNodeHdlr = 0;
      _textureBaseSelected = false;
      _activeTDSet = 0;
      _svvh = 0;
      _vvvh = 0;
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
   if(_activeVolumeVizNode && _activeVisNodeHdlr)
   {
      if(value)
      {
         _activeVisNodeHdlr->TurnOnBBox();
      }
      else
      {
         _activeVisNodeHdlr->TurnOffBBox();
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
   //if ( cfdModelHandler::instance()->GetActiveModel() )
   {
      if( cfdModelHandler::instance()->GetXMLCommand()->GetCommandName().compare( "wait" ) )
      {
         std::map<std::string,VE_EVENTS::TextureBasedEventHandler*>::iterator currentEventHandler;
         VE_XML::Command* tbvizCommand = cfdModelHandler::instance()->GetXMLCommand();
         currentEventHandler = _eventHandlers.find( tbvizCommand->GetCommandName() );
         if ( currentEventHandler != _eventHandlers.end() )
         {
            vprDEBUG(vesDBG,2) << "|\tExecuting: "<< tbvizCommand->GetCommandName() 
                                 << std::endl << vprDEBUG_FLUSH;
            currentEventHandler->second->SetGlobalBaseObject();
            currentEventHandler->second->Execute( tbvizCommand );
            _updateGraph();


         }
      }
   }
   _updateShaders();  
   
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
      cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
      if(sShader)
      {
         sShader->FastTransferFunctionUpdate();
         sShader->DeactivateIsoSurface();
         sShader->SetScalarRange(range);
      }
      _activeVisNodeHdlr = _svvh;
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::UpdatePreIntegrationTable(bool trueFalse)
{
   if(_svvh)
   {
	   cfdScalarShaderManager* sShader = dynamic_cast<cfdScalarShaderManager*>(_svvh->GetActiveShader());
      if(sShader)
      {
         //this is hard coded and will need to change--biv
         if(trueFalse)
         {
            sShader->FullTransferFunctionUpdate();
         }
         else
         {
            sShader->FastTransferFunctionUpdate();
         }
		 sShader->EnsureScalarRange();
      }
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
void cfdTextureBasedVizHandler::SetWorldDCS(VE_SceneGraph::DCS* dcs)
{
   if(_worldDCS != dcs)
      _worldDCS = dcs;
}
///////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetParentNode(VE_SceneGraph::Group* parent)
{
   if(_parent != parent)
      _parent = parent;
}
///////////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetActiveTextureDataSet(cfdTextureDataSet* tds)
{
   if(tds != _activeTDSet)
   {
      _activeTDSet = tds;
   }
}
////////////////////////////////////////////////////////////////////////
void cfdTextureBasedVizHandler::SetActiveShaderManager(std::string name)
{
	if(_activeVisNodeHdlr)
	{
	   if(name != "CUSTOM")
	   {
          _activeVisNodeHdlr->SetActiveShader(name);
	   }
	}
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
      //_svvh->SetActiveShader("BLUE_RED_LINEAR_SHADER");
	  _activeVisNodeHdlr = _svvh;
      if(!_svvh->IsThisActive())
      {
         _svvh->EnableDecorator();
      } 
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
	  _activeVisNodeHdlr = _vvvh;
      if(!_vvvh->IsThisActive())
         _vvvh->EnableDecorator();
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
#endif

