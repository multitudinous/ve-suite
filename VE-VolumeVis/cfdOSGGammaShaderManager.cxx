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
      _ss->setTextureMode(0,GL_TEXTURE_3D,
                        osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(0,
                                   new osg::TexEnv(osg::TexEnv::MODULATE),
		                              osg::StateAttribute::ON);
      int nTransferFunctions = _transferFunctions.size();
      for(int i =0; i < nTransferFunctions; i++){
         _ss->setTextureAttributeAndModes(i+1,_transferFunctions.at(i).get(),
                                      osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE);
         _ss->setTextureMode(i+1,GL_TEXTURE_1D,
                          osg::StateAttribute::ON);
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
        strcat(directory,"/VE_Xplorer/cg_shaders/");
      }
      strcat(directory,"gammaCorrection.cg");
      _setupCGShaderProgram(_ss.get(),directory,"gammaCorrection");
   }
   _reinit = false;
}
///////////////////////////////////////////////////////
void cfdOSGGammaShaderManager::_initPropertyTexture()
{
   if(!_utCbk){
      _utCbk =  new cfdUpdateTextureCallback();
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
                                GL_RGBA,GL_UNSIGNED_BYTE);
      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
                           GL_RGBA,
	                        GL_RGBA,
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
      _property->setInternalFormat(GL_RGBA);
      _property->setImage(propertyField.get());
      _property->setSubloadCallback(_utCbk);
   } 
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif