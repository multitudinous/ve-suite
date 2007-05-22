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
#ifdef _OSG

#include <osg/Geometry>
#include <osg/Geode>
#include <osg/Group>
#include <osg/TexGenNode>
#include <osg/TexMat>
#include <osg/Switch>
#include <iostream>
#include <osg/Vec3f>
#include "VE_Xplorer/TextureBased/cfdVolumeVisNodeHandler.h"
#include "VE_Xplorer/TextureBased/cfdOSGShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdTextureMatrixCallback.h"
using namespace VE_TextureBased;
//////////////////////////////////////////////////
//Constructors                                  //
//////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler()
{
   _whichChildIsThis = 0;
   _whichTexture = 0;
   _tm = 0;
   _center[0] = 0;
   _center[1] = 0;
   _center[2] = 0;

   _scale[0] = 1;
   _scale[1] = 1;
   _scale[2] = 1;
   _autoTexGen = true;
   _activeShader = "";
}
//////////////////////////////////////////////////////
cfdVolumeVisNodeHandler::cfdVolumeVisNodeHandler(const
                          cfdVolumeVisNodeHandler& vvnh)
{
   _activeShader = vvnh._activeShader;
   _bbox = vvnh._bbox;
   _bboxSwitch = new osg::Switch(*vvnh._bboxSwitch);
   _byPassNode = new osg::Group(*vvnh._byPassNode);
   _vvN = new osg::Switch(*vvnh._vvN);
   _whichChildIsThis = vvnh._whichChildIsThis;
   _decoratorGroup = new osg::Group(*vvnh._decoratorGroup);
   _tm = new cfdTextureManager(*vvnh._tm);
   _center = osg::Vec3f(vvnh._center[0],vvnh._center[1],vvnh._center[2]);

   _scale[0] = vvnh._scale[0];
   _scale[1] = vvnh._scale[1];
   _scale[2] = vvnh._scale[2];
   _texGenParams = new osg::TexGenNode(*vvnh._texGenParams);
   _autoTexGen = vvnh._autoTexGen;
   _shaderManagers = vvnh._shaderManagers;
}
///////////////////////////////////////////////////
//Destructor                                     //
///////////////////////////////////////////////////
cfdVolumeVisNodeHandler::~cfdVolumeVisNodeHandler()
{
   for ( std::map<std::string ,
         VE_TextureBased::cfdOSGShaderManager*>::iterator itr = _shaderManagers.begin();
         itr != _shaderManagers.end(); itr++ )
   {
      delete itr->second;
      itr->second = 0;
   }
   _shaderManagers.clear();
}
////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetBoundingBox(float* bbox)
{
   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = bbox[0]; 
   minBBox[1] = bbox[2]; 
   minBBox[2] = bbox[4]; 
   maxBBox[0] = bbox[1]; 
   maxBBox[1] = bbox[3]; 
   maxBBox[2] = bbox[5]; 
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));
  
}
///////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetSwitchNode(osg::Switch* vvn)
{
   _vvN = vvn;
}
////////////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetTextureManager(cfdTextureManager* tm)
{
   if(_tm != tm)
      _tm = tm;
   /*if(!_tm)
      _tm = new cfdTextureManager(*tm);
   else
      _tm->operator =(*tm);*/
}
///////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetAttachNode(osg::Group* attachNode)
{
   _byPassNode = attachNode;
}
////////////////////////////////////
void cfdVolumeVisNodeHandler::Init()
{
   if(!_bbox.valid()){
      std::cout<<"Invalid bounding box!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_vvN.valid()){
      std::cout<<"Invalid volume viz node!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_tm){
      std::cout<<"Invalid TextureManager!!"<<std::endl;
      std::cout<<"cfdVolumeVisNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_byPassNode){
      std::cout<<"Decorator attachment node not set!!"<<std::endl;
      std::cout<<"cfdVolumeVisNodeHandler::Init!!"<<std::endl;
   }
   //create the texture generation param node
   _createTexGenNode();
   
   if(!_bboxSwitch.valid()){
      _createVisualBBox();
      _whichChildIsThis = _vvN->getNumChildren();
      //be able to turn the bounding box off/on
      _bboxSwitch = new osg::Switch();
      _bboxSwitch->setName("VVNH BBox Switch");
      _bboxSwitch->addChild(_visualBoundingBox.get());
      
      _vvN->addChild(_bboxSwitch.get());

      //set up the decorator nodes
      if(!_decoratorGroup.valid()){
         _decoratorGroup = new osg::Group();
      }

      _visualBoundingBox->addChild(_decoratorGroup.get());
      _bboxSwitch->addChild(_decoratorGroup.get());
      _bboxSwitch->setSingleChildOn(1);
      _decoratorGroup->addChild(_texGenParams.get());
      _texGenParams->addChild(_byPassNode.get());
     
      
      //NOTE -- In derived classes, must override this call
      //to setup the stateset for the decorator
      _setUpDecorator();
      _applyTextureMatrix();

      //must do this to make sure switch is initially
      //traversing the "undecorated" node
      _vvN->setSingleChildOn(0);
   }
}
//////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetCenter(osg::Vec3f center)
{
   _center[0] = center[0];
   _center[1] = center[1];
   _center[2] = center[2];
}
///////////////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetTextureScale(float* scale,bool isInverted)
{
   _scale[0] = scale[0];
   _scale[1] = scale[1];
   _scale[2] = scale[2];

   if(!isInverted){
      _scale[0] = 1.0/scale[0];
      _scale[1] = 1.0/scale[1];
      _scale[2] = 1.0/scale[2];

   }
}
///////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_updateTexGenUnit(unsigned int unit)
{
   if(_texGenParams.valid())
   {
      _texGenParams->setTextureUnit(unit);
   }
}
////////////////////////////////////////////////
//may need to modify this to use the bbox     //
////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_createTexGenNode()
{
   if(!_texGenParams.valid())
   {
      osg::Vec4 sPlane(1,0,0,0);
      osg::Vec4 tPlane(0,1,0,0);
      osg::Vec4 rPlane(0,0,1,0);

      sPlane[3] = -_bbox.xMin(); 
      tPlane[3] = -_bbox.yMin();
      rPlane[3] = -_bbox.zMin();
            
      _texGenParams = new osg::TexGenNode();
      _texGenParams->setTextureUnit(0);
      _texGenParams->getTexGen()->setMode(osg::TexGen::EYE_LINEAR);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::S,sPlane); 
      _texGenParams->getTexGen()->setPlane(osg::TexGen::T,tPlane);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::R,rPlane);
   }
}
////////////////////////////////////////////
bool cfdVolumeVisNodeHandler::IsThisActive()
{
   if(_vvN.valid())
   {
      int index = _whichChildIsThis ;
      return _vvN->getValue(index);
   }
   return false;
}
////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetBoundingBoxName(std::string name)
{
   if((!name.empty()) && _bboxSwitch.valid())
   {
      _bboxSwitch->setName(name);
   }
}
////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetDecoratorName(std::string name)
{
   if((!name.empty()) && _decoratorGroup.valid())
   {
      _decoratorGroup->setName(name);
   }
}
///////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::SetActiveShader(std::string name)
{
   if((!name.empty()) && _decoratorGroup.valid())
   {
      _activeShader = name;
      _decoratorGroup->setStateSet(GetShaderManager(name)->GetShaderStateSet());
      _applyTextureMatrix();
   }
}
///////////////////////////////////////////////
void cfdVolumeVisNodeHandler::EnableDecorator()
{
   if(_vvN.valid())
   {
      _vvN->setSingleChildOn(_whichChildIsThis);
   }
}
//////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOnBBox()
{
   if(_bboxSwitch.valid())
   {
      _bboxSwitch->setSingleChildOn(0);
   } 
}
///////////////////////////////////////////
void cfdVolumeVisNodeHandler::TurnOffBBox()
{
   if(_bboxSwitch.valid())
   {
      _bboxSwitch->setSingleChildOn(1);
   } 
}
/////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::_createVisualBBox()
{
   if(_bbox.valid()){
      _visualBoundingBox = new osg::Group();
      _visualBoundingBox->setName("VisHandler Bounding Box");

      osg::ref_ptr<osg::StateSet> ss = _visualBoundingBox->getOrCreateStateSet();
      ss->setRenderingHint(osg::StateSet::OPAQUE_BIN);
      ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
      ss->setMode(GL_DEPTH_TEST,osg::StateAttribute::ON);
	  osg::ref_ptr<osg::Geometry> bboxCube = new osg::Geometry;

	  osg::ref_ptr<osg::Vec3Array> coords = new osg::Vec3Array();
      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(7));

      bboxCube->setVertexArray(coords.get());

      osg::ref_ptr<osg::Vec4Array> colors = new osg::Vec4Array(1);
      (*colors)[0].set(1.0f,1.0f,0.0f,1.0f);
      bboxCube->setColorArray(colors.get());
      bboxCube->setColorBinding(osg::Geometry::BIND_OVERALL);
      bboxCube->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES,
			       0,coords->size()));
      
	  osg::ref_ptr<osg::Geode> geode = new osg::Geode;
      geode->addDrawable(bboxCube.get());
      geode->setName("Visual BBox");
      _visualBoundingBox->addChild(geode.get());
      
   }else{
      std::cout<<"Invalid bbox in cfdVolumeVisNodeHandler::_createVisualBBox!"<<std::endl;
   }
}
////////////////////////////////////////////////////////////////
void cfdVolumeVisNodeHandler::AddShaderManager(std::string name,
                               VE_TextureBased::cfdOSGShaderManager* newShader,
                               bool isScalar)
{
   int* fieldSize = _tm->fieldResolution();
   if(isScalar)
   {
      dynamic_cast<cfdScalarShaderManager*>(newShader)->SetUseTextureManagerForProperty(true);
      dynamic_cast<cfdScalarShaderManager*>(newShader)->SetFieldSize(fieldSize[0],fieldSize[1],fieldSize[2]);
      dynamic_cast<cfdScalarShaderManager*>(newShader)->InitTextureManager(_tm);
   }
   newShader->Init();
   _shaderManagers[name] = newShader;
}
//////////////////////////////////////////////////////////
VE_TextureBased::cfdOSGShaderManager* 
cfdVolumeVisNodeHandler::GetShaderManager(std::string name)
{
   try
   {
      return _shaderManagers[name];
   }
   catch(...)
   {
      std::cout<<"The shader:"<<name<<" was not added!!"<<std::endl; 
   }
   return 0;
}
//////////////////////////////////////////////////////////
std::string cfdVolumeVisNodeHandler::GetActiveShaderName()
{
   return _activeShader;
}
//////////////////////////////////////////////////////////
VE_TextureBased::cfdOSGShaderManager* 
cfdVolumeVisNodeHandler::GetActiveShader()
{
   return GetShaderManager(_activeShader);
}
///////////////////////////////////////////////////////////////////////
//equal operator                                                     //
///////////////////////////////////////////////////////////////////////
cfdVolumeVisNodeHandler&
cfdVolumeVisNodeHandler::operator=(const cfdVolumeVisNodeHandler& vvnh)
{
   if(this != &vvnh){
      _bbox = vvnh._bbox;
      _vvN = vvnh._vvN;
      _decoratorGroup = vvnh._decoratorGroup;
      _visualBoundingBox = vvnh._visualBoundingBox;
      _bboxSwitch = vvnh._bboxSwitch;
      _tm = vvnh._tm;
      _byPassNode = vvnh._byPassNode;
      _center = vvnh._center;
      _scale[0] = vvnh._scale[0];
      _scale[1] = vvnh._scale[1];
      _scale[2] = vvnh._scale[2];
      _texGenParams = vvnh._texGenParams;
      _shaderManagers = vvnh._shaderManagers;
      _activeShader = vvnh._activeShader;
   }
   return *this;
}

#endif //_OSG
