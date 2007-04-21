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
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/Texture2D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"
#include "VE_Xplorer/TextureBased/cfdOSGTransferShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
#include "VE_Xplorer/TextureBased/RedYellowGreenCyanBlueTransferFunction.h"
#include "VE_Xplorer/TextureBased/PreIntegrationTexture.h"
using namespace VE_TextureBased;
//the shader inline source
#include "VE_Xplorer/TextureBased/volumeRenderBasicShader.h"
#include "VE_Xplorer/TextureBased/volumeRenderPhongShader.h"
////////////////////////////////////////////////
cfdScalarShaderManager::cfdScalarShaderManager()
{
   _useTM = true;
   _isoSurface = false;
   _preIntegrate = true;
   _percentScalarRange = 0;
   _stepSize[0] = .001;
   _stepSize[1] = .001;
   _stepSize[2] = .001;
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
      for(int i =0; i < nTransferFunctions; i++){
         _ss->setTextureAttributeAndModes(i+1,_transferFunctions.at(i).get(),
                                   osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE); 
         /*_ss->setTextureMode(i+1,GL_TEXTURE_2D,
                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::OFF);*/
         _ss->setTextureMode(i+1,GL_TEXTURE_GEN_S,
                        osg::StateAttribute::OFF);
         _ss->setTextureMode(i+1,GL_TEXTURE_GEN_T,
                        osg::StateAttribute::OFF);
         _ss->setTextureMode(i+1,GL_TEXTURE_GEN_R,
                        osg::StateAttribute::OFF);
      }
      _tUnit = 0;
      _setupStateSetForGLSL();
   }
   _reinit = false;
}
////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::_setupStateSetForGLSL()
{
   std::cout<<"Using glsl..."<<std::endl;
   _ss->addUniform(new osg::Uniform("volumeData",0));
   _ss->addUniform(new osg::Uniform("fastUpdate",_preIntegrate));
   _ss->addUniform(new osg::Uniform("transferFunction",1)); 
   //_ss->addUniform(new osg::Uniform("deltaSlice",osg::Vec3(1.f,1.f,1.f)));
   _ss->addUniform(new osg::Uniform("stepSize",osg::Vec3f(_stepSize[0],_stepSize[1],_stepSize[2])));

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
   /*
   GLubyte* lutex =0;
   GLfloat R,G,B,A;
   GLfloat newMid = 0;
   GLfloat newRange[2];
   ScalarRange origRange ;
   GLfloat alpha = 0;
   GLfloat isoVal = 0;
   float invSRange = 0;
   osg::ref_ptr<osg::Texture2D> tFunc = _transferFunctions.at(0);
   if(tFunc.valid())
   {
      lutex = tFunc->getImage()->data();
      if(!lutex)
      {
         std::cout<<"ERROR!"<<std::endl;
         std::cout<<"Invalid data for transfer function!!"<<std::endl;
         std::cout<<"cfdOSGGammaShader::_updateTransferFunction()"<<std::endl;
         return;
      }
   }
   origRange = _tm->dataRange(_tm->GetCurrentFrame());
   if((origRange.range[1]-origRange.range[0]) == 0.0)
   {
      newRange[0] = 0.0;
      newRange[1] = 255.0;
   }
   else
   {
      newRange[0] = (_scalarRange[0] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
      newRange[0] *= 255.0;
      newRange[1] = (_scalarRange[1] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
      newRange[1] *= 255.0;
   }
   newMid = newRange[0] + .5*(newRange[1] - newRange[0]);
   invSRange =  1.0/(newRange[1]-newRange[0]);
   float opacity = 1.0/128;
   //only update the diagonal so that our transfer function mods are still fast
   {
      for(int i = 0; i < 256; i++)
      {
         {
            if(i < newRange[0])
            {
               if(_isoSurface)
               {
                  lutex[i*(256*4) + i*4 ] = 0;
                  lutex[i*(256*4) + i*4 + 1] = 0;
                  lutex[i*(256*4) + i*4 + 2] = 0;
                  lutex[i*(256*4) + i*4 + 3] = 0;
               }else{
                  lutex[i*(256*4) + i*4     ] = 0;
                  lutex[i*(256*4) + i*4  + 1] = 0;
                  lutex[i*(256*4) + i*4  + 2] = 0;
                  lutex[i*(256*4) + i*4  + 3] = 0;
               }
            }
            else if( i > newRange[1])
            {
               if(_isoSurface)
               {
                  lutex[i*(256*4) + i*4 ] = 0;
                  lutex[i*(256*4) + i*4 + 1] = 0;
                  lutex[i*(256*4) + i*4 + 2] = 0;
                  lutex[i*(256*4) + i*4 + 3] = 0;
               }
               else
               {
                  lutex[i*(256*4) + i*4 ] = 0;//255;
                  lutex[i*(256*4) + i*4 + 1] = 0;
                  lutex[i*(256*4) + i*4 + 2] = 0;
                  lutex[i*(256*4) + i*4 + 3] = 0;
               }
            }
            else
            {
               if(_isoSurface)
               {
                  GLfloat isoRange [2];
                  isoVal = newRange[0] + _percentScalarRange*(newRange[1] - newRange[0]);
                  isoRange[0] = isoVal - 4.0;
                  isoRange[1] = isoVal + 4.0;
                  if(i >= isoRange[0] && i <= isoRange[1]){
                     alpha = (i - newRange[0])*invSRange;
                     if(alpha <= .25)
                     {
                        R = 0;
                        G = (4.0*alpha)*255,      
                        B = (1.0)*255;
                        A = alpha*255.0*.5;
                     }
                     else if(alpha <= .5)
                     {
                        R = 0;
                        G = (1.0)*255,      
                        B = (2.0-4.0*alpha)*255;
                        A = alpha*255.0*.5;
                     }
                     else if(alpha <= .75)
                     {
                        R = (4.0*alpha-2.0)*255;
                        G = (1.0)*255;       
                        B = 0.;
                        A = alpha*255.0*.5;
                     }
                     else if(alpha <= 1.0)
                     {
                        R = (1.0)*255;
                        G = (4.0-4.0*alpha)*255;       
                        B = 0.;
                        A = alpha*255.0*.5;
                     }
                  }else{
                     R = 0;
                     G = 0;
                     B = 0;
                     A = 0;
                  }
               }
               else
               {
                  alpha = (i - newRange[0])*invSRange;
                  if(alpha <= .25)
                  {
                     R = 0;
                     G = (4.0*alpha)*255,      
                     B = (1.0)*255;
                     A = alpha*255.0*.5;
                  }
                  else if(alpha <= .5)
                  {
                     R = 0;
                     G = (1.0)*255,      
                     B = (2.0-4.0*alpha)*255;
                     A = alpha*255.0;
                  }
                  else if(alpha <= .75)
                  {
                     R = (4.0*alpha-2.0)*255;
                     G = (1.0)*255;       
                     B = 0.;
                     A = alpha*255.0*.5;
                  }
                  else if(alpha <= 1.0)
                  {
                     R = (1.0)*255;
                     G = (4.0-4.0*alpha)*255;       
                     B = 0.;
                     A = alpha*255.0*.5;
                  }
               }
               lutex[i*(256*4) + i*4    ]  = (unsigned char)R;
               lutex[i*(256*4) + i*4  + 1] = (unsigned char)G;
               lutex[i*(256*4) + i*4  + 2] = (unsigned char)B;
               lutex[i*(256*4) + i*4  + 3] = (unsigned char)A; 
            }
         }
      }
   }
   tFunc->dirtyTextureParameters();
   tFunc->dirtyTextureObject();*/
   
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
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
      _property->setInternalFormat(GL_LUMINANCE_ALPHA);
      _property->setImage(propertyField.get());
      _property->setSubloadCallback(_utCbk.get());
   } 
}
#endif//_OSG
#endif
