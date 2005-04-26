#ifdef VE_PATENTED
#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/BlendFunc>
#include <osg/TexEnv>
#include <osg/TexMat>
#include <osg/TexGen>

#ifdef CFD_USE_SHADERS
#include "cfdTextureManager.h"
#include "cfdUpdateTextureCallback.h"
#include "cfdOSGTransferShaderManager.h"
#include "cfdOSGGammaShaderManager.h"
////////////////////////////////////////
void cfdOSGGammaShaderManager::Init()
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
      
      //load the shader file 
      char directory[1024];
      if(_shaderDirectory){
         strcpy(directory,_shaderDirectory);
      }else{
         char* vesuitehome = getenv("VE_SUITE_HOME");
         strcpy(directory,vesuitehome);
        strcat(directory,"/VE_VolumeVis/cg_shaders/");
      }
      //strcat(directory,"gammaCorrection.cg");
      //_setupCGShaderProgram(_ss.get(),directory,"gammaCorrection");
      strcat(directory,"scalarAdjuster.cg");
      _setupCGShaderProgram(_ss.get(),directory,"scalarAdjustment");
   }
   _reinit = false;
}
//////////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty()){
      //create transfer functions
      _createTransferFunction(); 
   }
}
////////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::_updateTransferFunction()
{
   GLubyte* lutex =0;
   GLfloat R,G,B,A;
   GLfloat newMid = 0;
   GLfloat newRange[2];
   ScalarRange origRange ;
   GLfloat alpha = 0;
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
         lutex[i*4    ] = 0;
         lutex[i*4 + 1] = 0;
         lutex[i*4 + 2] = 255;
         lutex[i*4 + 3] = 0;
      }else if( i > newRange[1]){
         lutex[i*4    ] = 255;
         lutex[i*4 + 1] = 0;
         lutex[i*4 + 2] = 0;
         lutex[i*4 + 3] = 255*.5;
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
         lutex[i*4   ]  = (unsigned char)R;
         lutex[i*4 + 1] = (unsigned char)G;
         lutex[i*4 + 2] = (unsigned char)B;
         lutex[i*4 + 3] = (unsigned char)A; 
      }
   }
   tFunc->dirtyTextureParameters();
   tFunc->dirtyTextureObject();
   
}
///////////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::SetScalarRange(float* range)
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
void cfdOSGGammaShaderManager::UpdateScalarMin(float minScalar)
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
void cfdOSGGammaShaderManager::UpdateScalarMax(float maxScalar)
{
   if(!_tm)
   {
      return;
   }
   ScalarRange originalRange = _tm->dataRange(_tm->GetCurrentFrame());
   _scalarRange[1] = originalRange.range[1]*maxScalar;
   _updateTransferFunction();
}
//////////////////////////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::UpdateTextureManager(cfdTextureManager* tm)
{
   if(_tm != tm){
      _tm = tm;
      if(_utCbk){
         _utCbk->SetIsLuminance(true);
         _utCbk->SetTextureManager(_tm);
         SetScalarRange(_tm->dataRange(_tm->GetCurrentFrame()).range);
      }
   }
   _reinit = false;
}
///////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::_initPropertyTexture()
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
                           osg::Image::USE_NEW_DELETE,1);

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
      _property->setSubloadCallback(_utCbk);
   } 
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif
