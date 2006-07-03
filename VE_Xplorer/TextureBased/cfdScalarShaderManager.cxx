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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifdef VE_PATENTED
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>
#include "VE_Xplorer/TextureBased/cfdTextureManager.h"
#include "VE_Xplorer/TextureBased/cfdUpdateTextureCallback.h"
#include "VE_Xplorer/TextureBased/cfdOSGTransferShaderManager.h"
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
using namespace VE_TextureBased;
//the shader inline source
static const char* scalarFragSource = {
   //a volume rendering shader which applies a 1D transfer function
   "uniform sampler3D volumeData;\n"
   "uniform sampler1D transferFunction;\n"
   "void main(void)\n"
   "{\n"
      "//dependent texture look up in transfer function\n"
      "float scalar = texture3D(volumeData,gl_TexCoord[0].xyz).a;\n"
      "gl_FragColor = texture1D(transferFunction,scalar);\n"

      "//set the opacity to .2 for all fragments\n"
      "gl_FragColor.a *= gl_Color.a;\n"
   "}\n"
};
////////////////////////////////////////////////
cfdScalarShaderManager::cfdScalarShaderManager()
{
   _useTM = true;
   _isoSurface = false;
   _percentScalarRange = 0;
}
////////////////////////////////////////
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

      _ss->setAttributeAndModes(bf.get());
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
//////////////////////////////////////////////////
void cfdScalarShaderManager::_setupStateSetForGLSL()
{
   std::cout<<"Using glsl..."<<std::endl;
   _ss->addUniform(new osg::Uniform("volumeData",0));
   _ss->addUniform(new osg::Uniform("transferFunction",1));
   _tUnit = 0;
   /*char* fullPath = _createShaderPathForFile("scalarAdjuster.glsl");
   if(!fullPath)
   {
      return;
   }*/
   //osg::ref_ptr<osg::Shader> scalarShader = _createGLSLShaderFromFile(fullPath,
   //                                                                  true);
   osg::ref_ptr<osg::Shader> scalarShader = _createGLSLShaderFromInline(scalarFragSource,true);
   osg::ref_ptr<osg::Program> glslProgram = new osg::Program();
   glslProgram->addShader(scalarShader.get());
   _setupGLSLShaderProgram(_ss.get(),glslProgram.get(),std::string("scalarAdjuster"));
   /*if(fullPath)
   {
      delete [] fullPath;
      fullPath = 0;
   }*/
}
/////////////////////////////////////////////////
void cfdScalarShaderManager::ActivateIsoSurface()
{
   _isoSurface = true;
}
///////////////////////////////////////////////////
void cfdScalarShaderManager::DeactivateIsoSurface()
{
   _isoSurface = false;
}
/////////////////////////////////////////////////////////////////////////
void cfdScalarShaderManager::SetIsoSurfaceValue(float percentScalarRange)
{
   _percentScalarRange = percentScalarRange;
   _updateTransferFunction();
}
//////////////////////////////////////////////////////////
void cfdScalarShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty()){
      //create transfer functions
      _createTransferFunction(); 
   }
}
////////////////////////////////////////////////////////
void cfdScalarShaderManager::_updateTransferFunction()
{
   GLubyte* lutex =0;
   GLfloat R,G,B,A;
   GLfloat newMid = 0;
   GLfloat newRange[2];
   ScalarRange origRange ;
   GLfloat alpha = 0;
   GLfloat isoVal = 0;
   float invSRange = 0;
   osg::ref_ptr<osg::Texture1D> tFunc = _transferFunctions.at(0);
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
   newRange[0] = (_scalarRange[0] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
   newRange[0] *= 255.0;
   newRange[1] = (_scalarRange[1] - origRange.range[0])/
                (origRange.range[1]-origRange.range[0]);
   newRange[1] *= 255.0;

   newMid = newRange[0] + .5*(newRange[1] - newRange[0]);
   invSRange =  1.0/(newRange[1]-newRange[0]);
   
   //make the RGBA values from the scalar range
   for(int i = 0; i < 256; i++){
      if(i < newRange[0])
      {
         if(_isoSurface)
         {
            lutex[i*4    ] = 0;
            lutex[i*4 + 1] = 0;
            lutex[i*4 + 2] = 0;
            lutex[i*4 + 3] = 0;
         }else{
            lutex[i*4    ] = 0;
            lutex[i*4 + 1] = 0;
            lutex[i*4 + 2] = 255;
            lutex[i*4 + 3] = 0;
         }
      }else if( i > newRange[1]){
         if(_isoSurface)
         {
            lutex[i*4    ] = 0;
            lutex[i*4 + 1] = 0;
            lutex[i*4 + 2] = 0;
            lutex[i*4 + 3] = 0;
         }else{
            lutex[i*4    ] = 255;
            lutex[i*4 + 1] = 0;
            lutex[i*4 + 2] = 0;
            lutex[i*4 + 3] = 127;
         }
      }else{
         if(_isoSurface)
         {
            GLfloat isoRange [2];
            isoVal = newRange[0] + _percentScalarRange*(newRange[1] - newRange[0]);
            isoRange[0] = isoVal - 10.0;
            isoRange[1] = isoVal + 10.0;
            if(i >= isoRange[0] && i <= isoRange[1]){
               alpha = (i - newRange[0])*invSRange;
               if(alpha <= .5){
                  R = 0;
                  G = (2.0*alpha)*255,      
                  B = (1.0-2.0*alpha)*255;
                  A = alpha*255.0*.5;
               }else{
                  R = (2.0*alpha-1.0)*255;
                  G = (2.0 - 2.0*alpha)*255;       
                  B = 0.;
                  A = alpha*255.0*.5;
               }
            }else{
               R = 0;
               G = 0;
               B = 0;
               A = 0;
            }
         }else{
            alpha = (i - newRange[0])*invSRange;
            if(alpha <= .5){
               R = 0;
               G = (2.0*alpha)*255,      
               B = (1.0-2.0*alpha)*255;
               A = alpha*255.0*.5;
            }else{
               R = (2.0*alpha-1.0)*255;
               G = (2.0 - 2.0*alpha)*255;       
               B = 0.;
               A = alpha*255.0*.5;
            }
         }
         lutex[i*4   ]  = (unsigned char)R;
         lutex[i*4 + 1] = (unsigned char)G;
         lutex[i*4 + 2] = (unsigned char)B;
         lutex[i*4 + 3] = (unsigned char)A; 
      }
   }
   tFunc->dirtyTextureParameters();
   tFunc->dirtyTextureObject();
   
}
////////////////////////////////////////////////
void cfdScalarShaderManager::EnsureScalarRange()
{
   _updateTransferFunction();
}
///////////////////////////////////////////////////////////
void cfdScalarShaderManager::SetScalarRange(float* range)
{
   //a percentage value is passed in 
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[0] = originalRange.range[0] 
                 + range[0]*(originalRange.range[1] - originalRange.range[0]);
   _scalarRange[1]  = originalRange.range[0] 
                 + range[1]*(originalRange.range[1] - originalRange.range[0]);
   _updateTransferFunction();
}
///////////////////////////////////////////////////////////////
void cfdScalarShaderManager::UpdateScalarMin(float minScalar)
{
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[0] = originalRange.range[0]*minScalar;
   _updateTransferFunction();
}
///////////////////////////////////////////////////////////////
void cfdScalarShaderManager::UpdateScalarMax(float maxScalar)
{
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[1] = originalRange.range[1]*maxScalar;
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
   _utCbk->SetTextureManager(_tm);
   _utCbk->SetDelayTime(.1);
   _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);
      
   if(!_property.valid()){
      osg::ref_ptr<osg::Image> propertyField = new osg::Image();
      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_ALPHA,GL_UNSIGNED_BYTE);
      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
                           GL_ALPHA,
	                        GL_ALPHA,
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
      _property->setInternalFormat(GL_ALPHA);
      _property->setImage(propertyField.get());
      _property->setSubloadCallback(_utCbk.get());
   } 
}
#endif//_OSG
#endif
