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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#include "VE_Xplorer/TextureBased/cfdVolumeVisualization.h"

#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <iostream>

#include "VE_Xplorer/TextureBased/cfdAdvectionSubGraph.h"
#include "VE_Xplorer/TextureBased/cfdTextureMatrixCallback.h"
#include "VE_Xplorer/TextureBased/cfdVolumeCenterCallback.h"
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/TBVolumeSlices.h"

#include <osg/TexMat>
#include <osg/BlendFunc>
#include <osg/ClipPlane>
#include <osg/ClipNode>
#include <osg/Node>
#include <osg/Geometry>
#include <osg/Texture1D>
#include <osg/Texture3D>
#include <osg/TexGen>
#include <osg/TexEnv>
#include <osg/Geode>
#include <osg/Billboard>
#include <osg/ClipNode>
#include <osg/TexGenNode>
#include <osg/Material>
#include <osg/Shape>
#include <osg/Image>
#include <osg/Switch>
#include <osg/BoundingBox>
using namespace VE_TextureBased;

////////////////////////////////////////////////
//Constructor                                 //
////////////////////////////////////////////////
cfdVolumeVisualization::cfdVolumeVisualization()
{
   _diagonal = 0;
   _scale[0] = 0;
   _scale[1] = 0;
   _scale[2] = 0;
   _center[0] = 0;
   _center[1] = 0;
   _center[2] = 0;
   _volumeVizNode  = 0;
   _texGenParams  = 0;
   _utCbk = 0;
   _mode = PLAY; // Need to change this back to stop after gui is wired up
   _traverseDirection = FORWARD;
   _stateSet  = 0;
   _texture  = 0;
   _nSlices = 200;
   _alpha = 0.5;
   _tUnit = 0;
   _verbose = 0;
   _tm = 0;
   _bbox = new osg::BoundingBox();
   _isCreated = false;
   _useShaders = false;
   _shaderDirectory = '\0';
   _volShaderIsActive =  false;
   _transferShaderIsActive = false;
   _vtkBBox = 0;
}
/////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization::cfdVolumeVisualization(const cfdVolumeVisualization& rhs)
{
   _utCbk = rhs._utCbk;
   //_vcCbk = rhs._vcCbk;
   _volumeVizNode = rhs._volumeVizNode;
   _texGenParams = rhs._texGenParams;
   _bbox = rhs._bbox;
   _mode = rhs._mode;
   _traverseDirection = rhs._traverseDirection;
   _stateSet = rhs._stateSet;
   _texture = rhs._texture;
   _billboard = rhs._billboard;
   _nSlices = rhs._nSlices;
   _alpha = rhs._alpha;
   _tUnit = rhs._tUnit;
   _verbose = rhs._verbose;
   _tm = rhs._tm;
   _image = rhs._image;
   _isCreated = rhs._isCreated;
   _vtkBBox = rhs._vtkBBox;

   _noShaderGroup =  rhs._noShaderGroup;
   _shaderDirectory = rhs._shaderDirectory;

}
//////////////////////////////////////////////////
cfdVolumeVisualization::~cfdVolumeVisualization()
{
   //not sure if I should call release here or not
   /*if(_utCbk){
      delete _utCbk;
      _utCbk = 0;
   }*/

   if ( !_shaderDirectory.empty() )
   {
      _shaderDirectory.clear();
   }
   
   if ( _bbox )
   {
      delete _bbox;
      _bbox = 0;
   }
}
////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetState(osg::State* state)
{
   _state = state;
}
//////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetShaderDirectory(std::string shadDir)
{
   _shaderDirectory = shadDir;
   
}
/////////////////////////////////////////////////////////////////
unsigned int cfdVolumeVisualization::GetCurrentTransientTexture()
{
   if(_utCbk.valid()){
      return _utCbk->GetCurrentFrame();
   }
   return 0;
}
////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetCurrentTransientTexture(unsigned int ct)
{
   if(_utCbk.valid()){
     _utCbk->SetCurrentFrame(ct);
   }
}
//////////////////////////////////////////////////////
void cfdVolumeVisualization::SetPlayMode(VisMode mode)
{
   _mode = mode;
   if(_tm){
      switch(_mode){
         case PLAY:       
            _tm->setPlayMode(cfdTextureManager::PLAY);
            break;
         case STOP:
         default:
            _tm->setPlayMode(cfdTextureManager::STOP);
            break;
      };
   }else{
      std::cout<<"Warning!!!!"<<std::endl;
      std::cout<<"Invalid cfdTextureManager!"<<std::endl;
      std::cout<<"Cannot set viz mode of texture manager in:"<<std::endl;
      std::cout<<"cfdVolumeVisualization::SetPlayMode()"<<std::endl;
   }
}
////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetPlayDirection(Direction dir)
{
   _traverseDirection = dir;
   if(_tm){
      switch(_traverseDirection){
         case FORWARD:       
            _tm->setDirection(1);
            break;
         case BACKWARD:
         default:
            _tm->setDirection(-1);
            break;
      };
   }else{
      std::cout<<"Warning!!!!"<<std::endl;
      std::cout<<"Invalid cfdTextureManager!"<<std::endl;
      std::cout<<"Cannot set direction of texture manager in:"<<std::endl;
      std::cout<<"cfdVolumeVisualization::SetPlayDirection()"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::Set3DTextureData(osg::Texture3D* texture)
{
   //not sure if this is needed 
   if(!_texture.valid()){
      _texture = new osg::Texture3D(*texture);
      _texture->setDataVariance(osg::Object::DYNAMIC);
      
      
   }else{
      if(_texture != texture){
         _texture = texture; 
      }
   }
   if(_stateSet.valid()){
      _attachTextureToStateSet(_stateSet.get());
   }
}
////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetBoundingBox(float* bbox)
{
   _vtkBBox = bbox;

   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = bbox[0]; 
   minBBox[1] = bbox[2]; 
   minBBox[2] = bbox[4]; 
   maxBBox[0] = bbox[1]; 
   maxBBox[1] = bbox[3]; 
   maxBBox[2] = bbox[5]; 
   
   _diagonal = 0;
   _scale[0] = fabs(maxBBox[0] - minBBox[0]);
   _scale[1] = fabs(maxBBox[1] - minBBox[1]);
   _scale[2] = fabs(maxBBox[2] - minBBox[2]);

   _bbox->set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));


   float radius = _bbox->radius();
   _diagonal = 2.0*radius;
   _center[0] = _bbox->center()[0];
   _center[1] = _bbox->center()[1];
   _center[2] = _bbox->center()[2];

   
   //recalculate the bbox that is going to be used for
   //the volume vis
   /*minBBox[0] = _center[0] - radius;
   minBBox[1] = _center[1] - radius;
   minBBox[2] = _center[2] - radius;

   maxBBox[0] = _center[0] + radius;
   maxBBox[1] = _center[1] + radius;
   maxBBox[2] = _center[2] + radius;

   _bbox->set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
*/
}
//////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetTextureManager(cfdTextureManager* tm)
{
   if(tm->GetDataType(0) == cfdTextureManager::VECTOR)
      return;

   _tm = tm;

   if(!_image.valid()){
      _image = new osg::Image();
   

     _image->allocateImage(_tm->fieldResolution()[0],
                     _tm->fieldResolution()[1],
                     _tm->fieldResolution()[2],
                     GL_RGBA,GL_UNSIGNED_BYTE);

      _image->setImage(_tm->fieldResolution()[0],_tm->fieldResolution()[1],
                     _tm->fieldResolution()[2],GL_RGBA,GL_RGBA, 
                     GL_UNSIGNED_BYTE,
                     _tm->dataField(0),
                     osg::Image::NO_DELETE,1);
      _image->setDataVariance(osg::Object::DYNAMIC);
   }
   if(!_texture.valid()){
      _texture = new osg::Texture3D();
      _texture->setDataVariance(osg::Object::DYNAMIC);
   

      _texture->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _texture->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _texture->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
      _texture->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
      _texture->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
      _texture->setTextureSize(_tm->fieldResolution()[0],
                     _tm->fieldResolution()[1],
                     _tm->fieldResolution()[2]);
       _texture->setInternalFormat(GL_ALPHA);
      //_texture->setInternalFormat(GL_RGBA);
      _texture->setImage(_image.get());
   }
   SetBoundingBox(_tm->getBoundingBox());
   if(_utCbk.valid()){
      _utCbk->SetTextureManager(_tm);
   }
}
///////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetNumberOfSlices(unsigned int nSlices)
{
   /*_nSlices = nSlices*3;*/
	_slices->SetNumberOfSlices(nSlices);
}
///////////////////////////////////////////////////////
void cfdVolumeVisualization::SetSliceAlpha(float alpha)
{
   _alpha = alpha;
}
/////////////////////////////////////////////////////////////////   
osg::ref_ptr<osg::StateSet> cfdVolumeVisualization::GetStateSet()
{
   if(_stateSet.valid()){
      return _stateSet;
   }else{
      if(_verbose)
         std::cout<<"Invalid state set in cfdVolumeVisualization::GetStateSet!"<<std::endl;
      return 0;
   }
}
///////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::Switch> cfdVolumeVisualization::GetVolumeVisNode()
{
   if(!_volumeVizNode.valid()){
      _buildGraph();
   }
   return _volumeVizNode;
}
/////////////////////////////////////////////
void cfdVolumeVisualization::DisableShaders()
{
   if(_volumeVizNode.valid()){
      _volumeVizNode->setSingleChildOn(0);
   }
}
/////////////////////////////////////////////////////////////////////   
osg::ref_ptr<osg::Texture3D> cfdVolumeVisualization::GetTextureData()
{
   if(_texture.valid()){
      return _texture;
   }else{
      if(_verbose)
         std::cout<<"Invalid texture data in cfdVolumeVisualization::GetTextureData()!"<<std::endl;
      return 0;
   }
}
/////////////////////////////////////////////////////
void cfdVolumeVisualization::SetVeboseFlag(bool flag)
{
   _verbose = flag;
}
//////////////////////////////////////////////
void cfdVolumeVisualization::_createClipNode()
{
   if(!_clipNode.valid()){
      _clipNode = new osg::ClipNode();
      _clipNode->setDataVariance(osg::Object::DYNAMIC);
   }
}
//////////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::AddClipPlane(ClipPlane direction,double* position)
{
   //biv -- check the logic here if we add more than one plane in
   //the same direction  but w/ different equation. . .this method shouldn't
   //be used but it might mistakenly be called. .. 
   if(_clipNode.valid()){
      _clipNode->addClipPlane(new osg::ClipPlane(direction,
                                            position[0],
                                            position[1],
                                            position[2],
                                            position[3]));
   }else{
      std::cout<<"Error!!!"<<std::endl;
      std::cout<<"Invalid osg::ClipNode in cfdVolumeVisualization::AddClipPlane!!"<<std::endl;
   }
}
//////////////////////////////////////////////
void cfdVolumeVisualization::ResetClipPlanes()
{
   if(_clipNode.valid())
   {
      double position[4] = {0,0,0,0};
      position[0] = 1.0;
      position[3] = -_bbox->xMin();
      UpdateClipPlanePosition(XPLANE_MIN,position);

      position[0] = -1.0;
      position[3] = _bbox->xMax();
      UpdateClipPlanePosition(XPLANE_MAX,position);
      
      position[0] = 0.0;
      position[1] = 1.0;
      position[3] = -_bbox->yMin();
      UpdateClipPlanePosition(YPLANE_MIN,position);
      
      position[1] = -1.0;
      position[3] = _bbox->yMax();
      UpdateClipPlanePosition(YPLANE_MAX,position);
      
      position[1] = 0.0;
      position[2] = 1.0;
      position[3] = -_bbox->zMin();
      UpdateClipPlanePosition(ZPLANE_MIN,position);
      
      position[2] = -1.0;
      position[3] = _bbox->zMax();
      UpdateClipPlanePosition(ZPLANE_MAX,position);
   }
}
/////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::RemoveClipPlane(ClipPlane direction)
{
   if ( _clipNode.valid() )
   {
      osg::ref_ptr< osg::ClipPlane > plane = 0;
      unsigned int planeIndex = 0;
      if ( _clipNode->getNumClipPlanes() )
      {
         for(unsigned int i = 0; i< (unsigned int)_clipNode->getNumClipPlanes();i++)
         {
            plane = _clipNode->getClipPlane(i);
            if ( plane->getClipPlaneNum() == (unsigned int)direction )
            {
               planeIndex = i;
               break;
            }
            else
            {
               plane = 0;
            }
         }
         
         if ( plane.valid() )
         {
            _clipNode->removeClipPlane(planeIndex);
         }
         else
         {
            std::cout<<"Plane not found!"<<std::endl;
            std::cout<<"cfdVolumeVisualization::RemoveClipPlanePosition."<<std::endl;
         }
      }
      else
      {
         std::cout<<"No planes on clip node!"<<std::endl;
         std::cout<<"cfdVolumeVisualization::RemoveClipPlanePosition."<<std::endl;
      }
      plane = 0;
   }  
}
/////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::UpdateClipPlanePosition(ClipPlane direction,
                                               double* newPosition)
{
   osg::ref_ptr<osg::ClipPlane> plane =0;
   if ( _clipNode.valid() )
   {
      if ( _clipNode->getNumClipPlanes() )
      {
         for(unsigned int i = 0; i < (unsigned int)_clipNode->getNumClipPlanes();i++)
         {
            plane = _clipNode->getClipPlane(i);
            if ( plane->getClipPlaneNum() == (unsigned int)direction )
            {
               break;
            }
            else
            {
               plane = 0;
            }
         }
         //plane = _clipNode->getClipPlane(direction);
         if ( plane.valid() )
         {
            if ( newPosition )
            {
               plane->setClipPlane(newPosition[0],newPosition[1],newPosition[2],newPosition[3]);
            }
            else
            {
               std::cout<<"Invalid plane position!"<<std::endl;
               std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
            }
         }
         else
         {
            
            std::cout<<"Invalid plane!"<<std::endl;
            std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
            std::cout<<"Adding new clip plane via::AddClipPlane."<<std::endl;
            AddClipPlane(direction,newPosition);
         }
      }
      else
      {
         std::cout<<"Invalid plane!"<<std::endl;
         std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
         std::cout<<"Adding new clip plane via::AddClipPlane."<<std::endl;
         AddClipPlane(direction,newPosition);
      
      }
   }
   plane = 0;
}


////////////////////////////////////////////////
void cfdVolumeVisualization::_createStateSet()
{
   if(_noShaderGroup.valid()){
      if(!_stateSet.valid()){
         _stateSet = _noShaderGroup->getOrCreateStateSet();;
         _stateSet->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
         _stateSet->setMode(GL_BLEND,osg::StateAttribute::ON);
         
         osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
         tMat->setMatrix(osg::Matrix::identity());
         _stateSet->setTextureAttributeAndModes(0,tMat.get());
         
         osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
			 bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
			 
         _stateSet->setAttributeAndModes(bf.get());
			 _stateSet->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
         _attachTextureToStateSet(_stateSet.get());
      }else{
         //state set is already created/set by user
         if(_verbose){
            std::cout<<"Using user defined state set!!!!"<<std::endl;
         }
      }
   }else{
      if(_verbose)
         std::cout<<"Invalid TexGenNode in cfdVolumeVisualization::_createStateSet!"<<std::endl;
   } 
}
////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::_attachTextureToStateSet(osg::StateSet* ss)
{
   if(ss){
      if(_texture.valid()){

         if(!_utCbk.valid()){
            _utCbk =  new cfdUpdateTextureCallback();
            _utCbk->SetIsLuminance(true);
            //_utCbk->SetIsLuminance(false);
            _utCbk->SetTextureManager(_tm);
            _utCbk->SetDelayTime(0.1);
         
            int* res = _tm->fieldResolution();
            _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);
            _texture->setSubloadCallback(_utCbk.get());
         }

         ss->setTextureAttributeAndModes(0,_texture.get(),
			                        osg::StateAttribute::ON);
         ss->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
         ss->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
         ss->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
         ss->setTextureAttributeAndModes(0,new osg::TexEnv(osg::TexEnv::MODULATE),
		                                osg::StateAttribute::ON);
      }
   }else{
      if(_verbose)
         std::cout<<"Invalid state set in cfdVolumeVisualization::GetStateSet!"<<std::endl;
   }
}
////////////////////////////////////////////////
//may need to modify this to use the bbox     //
////////////////////////////////////////////////
void cfdVolumeVisualization::_createTexGenNode()
{
   //if ( _bbox ){
      osg::Vec4 sPlane(1,0,0,0);
      osg::Vec4 tPlane(0,1,0,0);
      osg::Vec4 rPlane(0,0,1,0);
            
      _texGenParams = new osg::TexGenNode();
      _texGenParams->setName("VV TexGenNode");
      _texGenParams->setTextureUnit(0);
      _texGenParams->getTexGen()->setMode(osg::TexGen::EYE_LINEAR);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::S,sPlane); 
      _texGenParams->getTexGen()->setPlane(osg::TexGen::T,tPlane);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::R,rPlane);
   //}
   //else
   /*{
      if(_verbose)
         std::cout<<"Invalid bbox in cfdVolumeVisualization::_createTexGenNode!"<<std::endl;
   }*/
}
////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::TranslateCenterBy(float* translate)
{
   /*if(_vcCbk.valid())
   {
      _vcCbk->Translate(translate);
   }*/
}
///////////////////////////////////////////
void cfdVolumeVisualization::_buildSlices()
{
   
   _slices = new VE_TextureBased::TextureBasedVolumeSlices(_vtkBBox,100);
   _slices->setUseDisplayList(false);
   _billboard = new osg::Geode();
   _billboard->addDrawable(_slices.get());
   return;
    // set up the slices.
   osg::ref_ptr<osg::Geometry> geom = new osg::Geometry;

    //the slices are y slices because the camera is looking
    //down the y axis in eye space
    float halfSize = _diagonal*0.5f;
    float y = halfSize;
    float dy =-_diagonal/(float)(_nSlices-1);

    osg::ref_ptr<osg::Vec3Array> ycoords = new osg::Vec3Array(4*_nSlices);
    geom->setVertexArray(ycoords.get());
    
    for(unsigned int i = 0; i< _nSlices; ++i, y+=dy)
    {
        (*ycoords)[i*4+0].set(-halfSize,y,halfSize);
        (*ycoords)[i*4+1].set(-halfSize,y,-halfSize);
        (*ycoords)[i*4+2].set(halfSize,y,-halfSize);
        (*ycoords)[i*4+3].set(halfSize,y,halfSize);
    }
   
    
    osg::ref_ptr<osg::Vec3Array> normals = new osg::Vec3Array(1);
    (*normals)[0].set(0.0f,-1.0f,0.0f);
    geom->setNormalArray(normals.get());
    geom->setNormalBinding(osg::Geometry::BIND_OVERALL);

    osg::ref_ptr<osg::Vec4Array> colors = new osg::Vec4Array(1);
    (*colors)[0].set(1.0f,1.0f,1.0f,.2);
    geom->setColorArray(colors.get());
    geom->setColorBinding(osg::Geometry::BIND_OVERALL);

    geom->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,ycoords->size()));
    geom->setUseDisplayList(false);
    //_billboard = new osg::Billboard;
    //_billboard = new cfdVolumeBillboard();
    //_billboard->setMode(osg::Billboard::POINT_ROT_WORLD);
    //_billboard->addDrawable(geom.get());

    
    
    //position the slices in the scene
    //_billboard->setPosition(0,osg::Vec3(_center[0],_center[1],_center[2]));

}
///////////////////////////////////////////////////////////
void cfdVolumeVisualization::_createVolumeSlices()
{
   std::cout<<"Creating Slices"<<std::endl;
   _slices = new VE_TextureBased::TextureBasedVolumeSlices(_vtkBBox,100);
   _slices->setUseDisplayList(false);
   _billboard = new osg::Geode();
   _billboard->addDrawable(_slices.get());
   //_buildSlices();
}
//////////////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::Group> cfdVolumeVisualization::GetDecoratorAttachNode()
{
   if(_decoratorAttachNode.valid()){
      return _decoratorAttachNode;
   }
   return 0;
}
/////////////////////////////////////////
void cfdVolumeVisualization::CreateNode()
{
   if(!_isCreated){
      _buildGraph();
   }
}
//////////////////////////////////////////
void cfdVolumeVisualization::_buildGraph()
{
   if(!_tm){
      std::cout<<"Warning: Texture Manager not set!!!"<<std::endl;
      std::cout<<"cfdVolumeVisualization::_buildGraph..."<<std::endl;
   }
   _volumeVizNode = new osg::Switch();
   _volumeVizNode->setName("Volume Viz Node");
   _volumeVizNode->setAllChildrenOff();
   _volumeVizNode->setDataVariance(osg::Object::DYNAMIC);

   if(!_noShaderGroup.valid()){
      _noShaderGroup = new osg::Group();
      _noShaderGroup->setName("VViz Node:R");
      _volumeVizNode->addChild(_noShaderGroup.get());
      
   }
   _createTexGenNode();
   _createStateSet();
   _createVolumeSlices();
   _createClipNode();
   _isCreated = true;

   if ( _stateSet.valid() )
   {
      _noShaderGroup->setStateSet(_stateSet.get());
   }

   if ( _texGenParams.valid() )
   {
      _noShaderGroup->addChild(_texGenParams.get());
      if(_stateSet.valid()){
         float trans[3] = {.5,.5,.5};
         float scale[3] = {0,0,0};
         scale[0] = 1.0/_scale[0];
         scale[1] = 1.0/_scale[1];
         scale[2] = 1.0/_scale[2];

         osg::ref_ptr<osg::TexMat> tmat =
             dynamic_cast<osg::TexMat*>(_stateSet->getTextureAttribute(0,osg::StateAttribute::TEXMAT));
         _texGenParams->setUpdateCallback(new cfdTextureMatrixCallback(tmat.get(),
                                                           _center,
                                                            scale,trans));
      }
      if(!_decoratorAttachNode){
         _decoratorAttachNode = new osg::Group();
         _decoratorAttachNode->setName("VViz Decorator Attach");
         _texGenParams->addChild(_decoratorAttachNode.get());
      }
      if(_billboard.valid()){
         if(_clipNode.valid()){
            _clipNode->createClipBox(*_bbox,XPLANE_MIN);
            _decoratorAttachNode->addChild(_clipNode.get());
            _clipNode->addChild(_billboard.get());
         }else{
            _texGenParams->addChild(_billboard.get());
         }
      }else{
         _isCreated = false;
      }
   }else{
      _isCreated = false;
   }
}
////////////////////////////////////////////////////////////////////
cfdVolumeVisualization&
cfdVolumeVisualization::operator=(const cfdVolumeVisualization& rhs)
{
   if(&rhs != this){
      _utCbk = rhs._utCbk;
      //_vcCbk = rhs._vcCbk;
      _volumeVizNode = rhs._volumeVizNode;
      _texGenParams = rhs._texGenParams;

      _stateSet = rhs._stateSet;
      _texture = rhs._texture;
      _billboard = rhs._billboard;
      _nSlices = rhs._nSlices;
      _alpha = rhs._alpha;
      _tUnit = rhs._tUnit;
      _tm = rhs._tm;
      
      _decoratorAttachNode = rhs._decoratorAttachNode;
      _mode = rhs._mode;
      _traverseDirection = rhs._traverseDirection;
      _stateSet = rhs._stateSet;
   
      _verbose = rhs._verbose;
   
      _image = rhs._image;
      _isCreated = rhs._isCreated;
      
      _shaderDirectory = rhs._shaderDirectory;
      _noShaderGroup =  rhs._noShaderGroup;

   }
   return *this;
}


#endif
#endif
