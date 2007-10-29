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
#ifdef _OSG
#include <ves/xplorer/volume/cfd3DTextureCullCallback.h>
#include <ves/xplorer/volume/cfdCopyTo3DTextureStage.h>
#include <ves/xplorer/volume/cfdPBufferManager.h>
#include <ves/xplorer/volume/cfdOSGPingPongTexture3d.h>
#include <osg/Node>
#include <osg/NodeVisitor>
#include <osg/Texture3D>
#include <osgUtil/CullVisitor>
#include <osg/Viewport>
#include <osg/Geode>
#include <osg/Geometry>
#include <osgUtil/RenderStage>
#include <osgUtil/UpdateVisitor>
#include <osg/FrameStamp>
//#include <osg/BoundingBox>
using namespace ves::xplorer::volume;

////////////////////////////////////////////////////////////////////////////
cfd3DTextureCullCallback::cfd3DTextureCullCallback(osg::Node* subgraph,
                                             //osg::Texture3D* texture,
                                             unsigned int width,
                                             unsigned int height)
:_subgraph(subgraph)//,_textureToUpdate(texture)
{
   _w = width;
   _h = height;
   _pbuffer = 0;
   _count = 0;
   _pingPonger = 0;
   _uniformUpdater = 0;
}
/////////////////////////////////////////////////////
cfd3DTextureCullCallback::~cfd3DTextureCullCallback()
{
   if(_pingPonger)
   {
      delete _pingPonger;
      _pingPonger = 0;
   }
   if(_uniformUpdater){
      delete _uniformUpdater;
      _uniformUpdater = 0;
   }
}
///////////////////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::SetPingPongTextures(unsigned int tPingUint,
                                             osg::Node* ping,
                                             unsigned int tPongUint, 
                                              osg::Node* pong)
{
   _previous = ping;
   _current = pong;
   if(!_pingPonger)
   {
      _pingPonger = new cfdOSGPingPongTexture3D();
   }
   _pingPonger->SetPingTexture(tPingUint,_previous.get());
   _pingPonger->SetPongTexture(tPongUint,_current.get());
   _textureToUpdate = _pingPonger->GetCurrentTexture();
   if(!_uniformUpdater){
      _uniformUpdater = new osgUtil::UpdateVisitor();
      _fs = new osg::FrameStamp();
      _uniformUpdater->setFrameStamp(_fs.get());;
   }
}
//////////////////////////////////////////////////////////////////
cfdOSGPingPongTexture3D* cfd3DTextureCullCallback::GetPingPonger()
{
   if(_pingPonger)
   {
      return _pingPonger;
   }
   return 0;
}
///////////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::operator()(osg::Node* node, 
                                     osg::NodeVisitor* nv)
{
   osgUtil::CullVisitor* cullVisitor = dynamic_cast<osgUtil::CullVisitor*>(nv); 
   if (cullVisitor && _subgraph.valid() && _pingPonger)
   {  
      //update the uniform updater
      _fs->setReferenceTime(nv->getFrameStamp()->getReferenceTime());
      _fs->setFrameNumber(nv->getFrameStamp()->getFrameNumber());


      //force update the uniforms on the subgraph
      _subgraph->accept(*_uniformUpdater);
      //_pingPonger->PingPongTextures();
      _textureToUpdate = _pingPonger->GetCurrentTexture();
      preRender(*node,*cullVisitor);
      // must traverse the Node's subgraph            
      traverse(node,nv);
      
      //
   }
}
////////////////////////////////////////////////////////
void cfd3DTextureCullCallback::preRender(osg::Node& node,
                                    osgUtil::CullVisitor& cv)
{
   const osg::BoundingSphere& bs = _subgraph->getBound();
   if (!bs.valid())
   {
      osg::notify(osg::WARN) << "bb invalid"<<_subgraph.get()<<std::endl;
      return;
   }

   // create the render to texture stage.
   osg::ref_ptr<cfdCopyTo3DTextureStage> rtts = new cfdCopyTo3DTextureStage;
#ifdef _PBUFFER
   if(!_pbuffer->isCreated()){
      _pbuffer->initializePBuffer(_w,_h);
   }
#endif
   // set up lighting.
   // currently ignore lights in the scene graph itself..
   // will do later.
   osg::ref_ptr<osgUtil::RenderStage> previous_stage = cv.getCurrentRenderBin()->getStage();

   // set up the background color and clear mask.
   rtts->setClearColor(osg::Vec4(0.0f,0.0,0.0f,1.0f));
   rtts->setClearMask(previous_stage->getClearMask());
#ifdef _PBUFFER
   rtts->setPBuffer(_pbuffer);
#endif
   // set up to charge the same RenderStageLighting is the parent previous stage.
   rtts->setPositionalStateContainer(previous_stage->getPositionalStateContainer());

   // record the render bin, to be restored after creation
   // of the render to text
   osgUtil::RenderBin* previousRenderBin = cv.getCurrentRenderBin();
   // set the current renderbin to be the newly created stage.
   cv.setCurrentRenderBin(rtts.get());

   //full screen quad for our pbuffer subgraph
   float znear = 1.0f*bs.radius();
   float zfar  = 3.0f*bs.radius();
        
   // 2:1 aspect ratio as per flag geomtry below.
   float top   = 0.25f*znear;
   float right = 0.5f*znear;

   znear *= 0.9f;
   zfar *= 1.1f;

   if(!_localState) 
      _localState = new osg::StateSet;

   cv.pushStateSet(_localState.get());
   {
      // traverse the subgraph
      _subgraph->accept(cv);
   }
   cv.popStateSet();
  
   // restore the previous renderbin.
   cv.setCurrentRenderBin(previousRenderBin);

   if(rtts->getRenderBinList().size()==0)
   {
      // getting to this point means that all the subgraph has been
      // culled by small feature culling or is beyond LOD ranges.
      return;
   }  
    
   int height = _h;
   int width  = _w;

   osg::ref_ptr<osg::Viewport> new_viewport = new osg::Viewport;
   new_viewport->setViewport(0,0,width,height);
   rtts->setViewport(new_viewport.get());

   _localState->setAttribute(new_viewport.get());    

   // and the render to texture stage to the current stages
   // dependancy list.
   if(_textureToUpdate.valid()){
      rtts->set3DTexture(_textureToUpdate.get());
      cv.getCurrentRenderBin()->getStage()->addToDependencyList(rtts.get());
   }
   _count++;
}
#endif
