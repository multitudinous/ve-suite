#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Image>
#include <osg/BlendFunc>
#include <osg/StateSet>
#include <osg/TexEnv>
#include "cfdTextureManager.h"
#include "cfdUpdateTextureCallback.h"
#include "cfdOSGShaderManager.h"
#include "cfdOSGScalarShaderManager.h"
#ifdef CFD_USE_SHADERS
//////////////////////////////////////////////////////
//Constructor                                       //
//////////////////////////////////////////////////////
cfdOSGScalarShaderManager::cfdOSGScalarShaderManager()
:cfdOSGShaderManager()
{
   _tm = 0;
   _utCbk = 0;
   _reinit = true;
}
////////////////////////////////////////////////////////////////////////////////
cfdOSGScalarShaderManager::cfdOSGScalarShaderManager(const
                                                 cfdOSGScalarShaderManager& sm)
:cfdOSGShaderManager(sm)
{
   if(this != &sm){
      //we only want a pointer to the tm!!!!!
      _tm = sm._tm; 
      _utCbk = new cfdUpdateTextureCallback(*sm._utCbk);
      _scalarProp = new osg::Texture3D(*(sm._scalarProp.get()));
      _image = new osg::Image(*(sm._image.get()));
   }
}
///////////////////////////////////////////////////////
cfdOSGScalarShaderManager::~cfdOSGScalarShaderManager()
{
   if(_tm){
      delete _tm;
      _tm = 0;
   }
}
//////////////////////////////////////
void cfdOSGScalarShaderManager::Init()
{
   if(!_tm){
      std::cout<<"Need to set the texture manager!!!"<<std::endl;
      std::cout<<"cfdOSGScalarShaderManager::Init()"<<std::endl;
      return;
   }
   int* res = _tm->fieldResolution();

   if(!_ss.valid()){
      _ss = new osg::StateSet();
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
      _ss->setMode(GL_BLEND,osg::StateAttribute::ON);

      osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);

      _ss->setAttributeAndModes(bf.get());
      _ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
   }
   if(!_utCbk){
      _utCbk =  new cfdUpdateTextureCallback();
   }
   _utCbk->SetTextureManager(_tm);
   _utCbk->SetDelayTime(1.0);
   _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);

   if(!_image.valid()){
      _image = new osg::Image();
      _image->allocateImage(res[0],res[1],res[2],
                     GL_RGBA,GL_UNSIGNED_BYTE);
      _image->setImage(res[0],res[1],res[2],GL_RGBA,GL_RGBA, 
                     GL_UNSIGNED_BYTE,
                     _tm->dataField(0),
                     osg::Image::USE_NEW_DELETE,1);
      _image->setDataVariance(osg::Object::DYNAMIC);
      
   } 
   
   if(!_scalarProp.valid()){
      _scalarProp = new osg::Texture3D();
      _scalarProp->setDataVariance(osg::Object::DYNAMIC);
      _scalarProp->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _scalarProp->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _scalarProp->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
      _scalarProp->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
      _scalarProp->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
      _scalarProp->setInternalFormat(GL_RGBA);
     
      _scalarProp->setImage(_image.get());
      _scalarProp->setSubloadCallback(_utCbk);
   }
   if(_reinit){
      _ss->setTextureAttributeAndModes(0,_scalarProp.get(),osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(0,new osg::TexEnv(osg::TexEnv::REPLACE),
		                    osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON);
      //load the shader file 
      char directory[1024];
      if(_shaderDirectory){
         strcpy(directory,_shaderDirectory);
      }else{
        strcpy(directory,"../cg_shaders/");
      }
      strcat(directory,"fragVol.cg");
      _setupCGShaderProgram(_ss.get(),directory,"fp_volume");
   }
   _reinit = false;
}

////////////////////////////////////////////////////////////////////////
void cfdOSGScalarShaderManager::InitTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   _reinit = true;
}
///////////////////////////////////////////////////////////////////////////
void cfdOSGScalarShaderManager::UpdateTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   _reinit = false;
}
///////////////////////////////////////////////////////////////////////////////
//equal operator                                                             //
///////////////////////////////////////////////////////////////////////////////
cfdOSGScalarShaderManager& cfdOSGScalarShaderManager::operator=(const 
                                                cfdOSGScalarShaderManager& sm)
{
   if(this != &sm){
      cfdOSGShaderManager::operator=(sm);
      _tm = sm._tm;
      if(_utCbk){
         delete _utCbk;
         _utCbk = 0;
      }
      _utCbk = new cfdUpdateTextureCallback(*sm._utCbk);
      _scalarProp = sm._scalarProp;
      _reinit = sm._reinit;
   }
   return *this;
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
