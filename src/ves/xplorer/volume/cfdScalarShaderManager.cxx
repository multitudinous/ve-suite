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
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>
#include "ves/xplorer/volume/cfdTextureManager.h"
#include "ves/xplorer/volume/cfdUpdateTextureCallback.h"
#include "ves/xplorer/volume/cfdOSGTransferShaderManager.h"
#include "ves/xplorer/volume/cfdScalarShaderManager.h"
#include "ves/xplorer/volume/RedYellowGreenCyanBlueTransferFunction.h"
#include "ves/xplorer/volume/PreIntegrationTexture.h"
#include "ves/xplorer/volume/NoiseTexture2D.h"
using namespace VE_TextureBased;
//the shader inline source
#include "ves/xplorer/volume/volumeRenderBasicShader.h"
#include "ves/xplorer/volume/volumeRenderPhongShader.h"
////////////////////////////////////////////////
cfdScalarShaderManager::cfdScalarShaderManager()
{
   _useTM = true;
   _isoSurface = false;
   _preIntegrate = true;
   _percentScalarRange = 0;
   _stepSize[0] = .0001;
   _stepSize[1] = .0001;
   _stepSize[2] = .0001;
   _tUnit = 0;
}
///////////////////////////////////////////
void cfdScalarShaderManager::Init()
{
   _initTransferFunctions();
   _initPropertyTexture();
   if(_tm)
   {
      SetScalarRange(_tm->dataRange(_tm->GetCurrentFrame()).range);
      
   }
   if(!_ss.valid() && _reinit &&_property.valid()){
      _ss = new osg::StateSet();
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _ss->setMode(GL_BLEND,osg::StateAttribute::ON);

      osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      bf->setFunction(osg::BlendFunc::SRC_ALPHA, 
                      osg::BlendFunc::ONE_MINUS_SRC_ALPHA);

      _ss->setAttributeAndModes(bf.get(),osg::StateAttribute::ON);
      _ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
      _ss->setTextureAttributeAndModes(0,_property.get(),
                                osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_S,
                     osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_T,
                     osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_R,
                     osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(0,
                                new osg::TexEnv(osg::TexEnv::REPLACE),
                             osg::StateAttribute::ON);
      int nTransferFunctions = _transferFunctions.size();
      //for(int i =0; i < nTransferFunctions; i++){
         _ss->setTextureAttributeAndModes(1,_transferFunctions.at(0).get(),
                                   osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE); 
         /*_ss->setTextureMode(i+1,GL_TEXTURE_2D,
                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::OFF);*/
         _ss->setTextureMode(1,GL_TEXTURE_GEN_S,
                        osg::StateAttribute::OFF);
         _ss->setTextureMode(1,GL_TEXTURE_GEN_T,
                        osg::StateAttribute::OFF);
         _ss->setTextureMode(1,GL_TEXTURE_GEN_R,
                        osg::StateAttribute::OFF);
      //}
      _ss->setTextureAttributeAndModes(3,_jitterTexture->GetNoiseTexture(),
                                   osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE); 
      _tUnit = 0;
      _setupStateSetForGLSL();
   }
   _reinit = false;
}
////////////////////////////////////////////////////
void cfdScalarShaderManager::_setupStateSetForGLSL()
{
   std::cout<<"Using glsl..."<<std::endl;
   _ss->addUniform(new osg::Uniform("volumeData",0));
   _ss->addUniform(new osg::Uniform("fastUpdate",_preIntegrate));
   _ss->addUniform(new osg::Uniform("transferFunction",1)); 
   _ss->addUniform(new osg::Uniform("jitter2D",3));
   _ss->addUniform(new osg::Uniform("viewRay",osg::Vec3(0,1,0)));
   _ss->addUniform(new osg::Uniform("alphaRatio",1.f));
   _ss->addUniform(new osg::Uniform("jitterSize",osg::Vec2(_jitterTexture->GetResolutionX(),_jitterTexture->GetResolutionY())));
   //_ss->addUniform(new osg::Uniform("deltaSlice",osg::Vec3(1.f,1.f,1.f)));
   _ss->addUniform(new osg::Uniform("stepSize",osg::Vec3f(_stepSize[0],_stepSize[1],_stepSize[2])));
   _ss->addUniform(new osg::Uniform("datacenter",osg::Vec3f(0,0,0)));
   _tUnit = 0;
   osg::ref_ptr<osg::Shader> basicVertShader = _createGLSLShaderFromInline(vrBasicVertSource,false);                          
   osg::ref_ptr<osg::Shader> basicFragShader = _createGLSLShaderFromInline(vrBasicFragSource,true);
   osg::ref_ptr<osg::Program> vrBasic = new osg::Program();
   vrBasic->addShader(basicFragShader.get());
   vrBasic->addShader(basicVertShader.get());
   AddShaderProgram("Basic Volume Render",vrBasic);

   osg::ref_ptr<osg::Shader> vrPhongVertShader = _createGLSLShaderFromInline(vrPhongVertSource,false);
   osg::ref_ptr<osg::Shader> vrPhongFragShader = _createGLSLShaderFromInline(vrPhongFragSource,true);
   osg::ref_ptr<osg::Program> vrPhong = new osg::Program();
   
   vrPhong->addShader(vrPhongVertShader.get());
   vrPhong->addShader(vrPhongFragShader.get());
   AddShaderProgram("Phong Lit Volume Render",vrPhong);

   SetActiveShaderProgram("Basic Volume Render");
}
//////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::FullTransferFunctionUpdate()
{
   _preIntegrate = true;
}
//////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::FastTransferFunctionUpdate()
{
   _preIntegrate = false;
}
//////////////////////////////////////////////////////////////
void cfdScalarShaderManager::ActivateIsoSurface()
{
   _tf->SetIsoSurface(true);
   //_preIntegrate = true;
}
///////////////////////////////////////////////////
void cfdScalarShaderManager::DeactivateIsoSurface()
{
   _tf->SetIsoSurface(false);
   //_preIntegrate = true;
}
/////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::SetIsoSurfaceValue(float percentScalarRange)
{
   _percentScalarRange = percentScalarRange;
   
   _tf->SetIsoSurfaceValue(percentScalarRange);
   _updateTransferFunction();
}
/////////////////////////////////////////////////////
void cfdScalarShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty())
   {
      //create transfer functions
      //_createTransferFunction(); 
      if(_tf)
	  {
		  delete _tf;
		  _tf = 0;
	  }
	  if(_preIntTexture)
	  {
		  delete _preIntTexture;
		  _preIntTexture = 0;
	  }
	  _tf = new VE_TextureBased::RYGCBLinearTF();
	  _tf->InitializeData();
	  _preIntTexture = new VE_TextureBased::PreIntegrationTexture2D();
	  _preIntTexture->SetTransferFunction(_tf);
	  _preIntTexture->FullUpdate();
      _transferFunctions.push_back(_preIntTexture->GetPreIntegratedTexture());
   }
}
/////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::_updateTransferFunction(bool fastUpdate)
{
   if(!_tf)
   {
	  std::cout<<"Transfer function not set!"<<std::endl;
	  std::cout<<"cfdScalarShaderManager::_updateTransferFunction"<<std::endl;
      return;
   }
   if(_ss.valid())
   {
      _ss->getUniform("fastUpdate")->set(!_preIntegrate);
   }
   if(_preIntegrate)
   {
	   _preIntTexture->FullUpdate();
	   _preIntegrate = false;
   }
   else
   { 
	   _preIntTexture->FastUpdate();
   }
}
//////////////////////////////////////////////////////////////
void cfdScalarShaderManager::EnsureScalarRange()
{
   _updateTransferFunction();
}
/////////////////////////////////////////////////////////
void cfdScalarShaderManager::SetScalarRange(float* range)
{ 
   if(!_tm)
   {
      return;
   }
   
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _tf->SetFullScalarRange(originalRange.range[0],originalRange.range[1]);
   float adjustedRange[2] = {0.,0.};
   adjustedRange[0] = originalRange.range[0] + range[0]*(originalRange.range[1] - originalRange.range[0]);
   adjustedRange[1] = originalRange.range[0] + range[1]*(originalRange.range[1] - originalRange.range[0]);
   _tf->AdjustScalarMaximum(adjustedRange[1]);
   _tf->AdjustScalarMinimum(adjustedRange[0]);
   _updateTransferFunction();
}
/////////////////////////////////////////////////////////////
void cfdScalarShaderManager::UpdateScalarMin(float minScalar)
{
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[0] = originalRange.range[0]*minScalar;
   _tf->AdjustScalarMinimum(_scalarRange[0]);
   _updateTransferFunction();
}
/////////////////////////////////////////////////////////////
void cfdScalarShaderManager::UpdateScalarMax(float maxScalar)
{
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[1] = originalRange.range[1]*maxScalar;
   _tf->AdjustScalarMinimum(_scalarRange[1]);
   _updateTransferFunction();
}
///////////////////////////////////////////////////////
void cfdScalarShaderManager::SetDelayTime(double delay)
{
   if(_utCbk.valid()){
     _utCbk->SetDelayTime(delay);
   }
}
///////////////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::SetCurrentTransientTexture(unsigned int whichTimeStep,
                                                  bool makeSlave )
{
   if(_utCbk.valid())
   {
     _utCbk->SetCurrentFrame(whichTimeStep,makeSlave);
   }
}
//////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::UpdateTextureManager(cfdTextureManager* tm)
{
   if(_tm != tm){
      _tm = tm;
      if(_utCbk.valid()){
         _utCbk->SetIsLuminance(true);
         _utCbk->SetTextureManager(_tm);
         SetScalarRange(_tm->dataRange(_tm->GetCurrentFrame()).range);
      }
   }
   _reinit = false;
}
///////////////////////////////////////////////////////
void cfdScalarShaderManager::_initPropertyTexture()
{
   if(!_utCbk){
      _utCbk =  new cfdUpdateTextureCallback();
      _utCbk->SetIsLuminance(true);
   }
   int* res = _tm->fieldResolution();
   _stepSize[0] = 1.0/float(res[0]);
   _stepSize[1] = 1.0/float(res[1]);
   _stepSize[2] = 1.0/float(res[2]);
   _utCbk->SetTextureManager(_tm);
   _utCbk->SetDelayTime(.1);
   _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);
   
      
   if(!_property.valid()){
      osg::ref_ptr<osg::Image> propertyField = new osg::Image();
      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_LUMINANCE_ALPHA,GL_UNSIGNED_BYTE);
      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
                           GL_LUMINANCE_ALPHA,
	                        GL_LUMINANCE_ALPHA,
	                        GL_UNSIGNED_BYTE,
                           _tm->dataField(0),
                           osg::Image::NO_DELETE,1);

      propertyField->setDataVariance(osg::Object::DYNAMIC);
      _property = new osg::Texture3D();
      _property->setDataVariance(osg::Object::DYNAMIC);
      _property->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _property->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setInternalFormat(GL_LUMINANCE_ALPHA);
      _property->setImage(propertyField.get());
      _property->setSubloadCallback(_utCbk.get());
   } 

   _jitterTexture = new VE_TextureBased::NoiseTexture2D();
}
#endif//_OSG
