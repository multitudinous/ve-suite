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
#include "cfdSimpleTextureCallback.h"
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::cfdOSGTransferShaderManager()
:cfdOSGShaderManager()
{
   _reinit = true;
   _useTM = false;
   _fieldSize[0] = 0;
   _fieldSize[1] = 0;
   _fieldSize[2] = 0;
   _tm = 0;
   _utCbk = 0;
}
//////////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::cfdOSGTransferShaderManager(const
                               cfdOSGTransferShaderManager& sm)
:cfdOSGShaderManager(sm)
{
   _property = new osg::Texture3D(*(sm._property.get()));
   int nTransferFuncts = sm._transferFunctions.size();
   for(int i = 0; i < nTransferFuncts; i++){
      _transferFunctions.push_back(sm._transferFunctions.at(i));
   }
   _fieldSize[0] = sm._fieldSize[0];
   _fieldSize[1] = sm._fieldSize[1];
   _fieldSize[2] = sm._fieldSize[2];
   _reinit = sm._reinit;
   _texMat = sm._texMat;
   _useTM = sm._useTM;
   _tm = sm._tm;
   _utCbk = sm._utCbk;
}
///////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::~cfdOSGTransferShaderManager()
{
   if(_transferFunctions.size()){
      _transferFunctions.clear();
   }
   if(_utCbk){
      delete _utCbk;
      _utCbk = 0;
   }
}
////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::InitTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   _reinit = true;
}
///////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::UpdateTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   if(_utCbk){
      _utCbk->SetTextureManager(_tm);
   }
   _reinit = false;
}
/////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetTextureMatrix(osg::TexMat* tmat)
{
   _texMat = tmat;
   if(_ss.valid()){
      unsigned int ntf = _transferFunctions.size();
      _ss->setTextureAttributeAndModes(ntf,_texMat.get(),
                                   osg::StateAttribute::ON);
   }
}
///////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetPropertyTexture(osg::Texture3D* property)
{
   //_property = property;
}
////////////////////////////////////////
void cfdOSGTransferShaderManager::Init()
{
   _initTransferFunctions();
   _initPropertyTexture();
   if(!_ss.valid() && _reinit &&_property.valid()){
      _ss = new osg::StateSet();
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _ss->setMode(GL_BLEND,osg::StateAttribute::ON);
      _ss->setMode(GL_LIGHTING,osg::StateAttribute::OFF|osg::StateAttribute::OVERRIDE);
      osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
      bf->setFunction(osg::BlendFunc::SRC_ALPHA, 
                    osg::BlendFunc::ONE_MINUS_SRC_ALPHA);

      _ss->setAttributeAndModes(bf.get());
      _ss->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
      
      int nTransferFunctions = _transferFunctions.size();
      for(int i =0; i < nTransferFunctions; i++){
         _ss->setTextureAttributeAndModes(i,_transferFunctions.at(i).get(),
                                      osg::StateAttribute::ON| osg::StateAttribute::OVERRIDE);
          _ss->setTextureMode(i,GL_TEXTURE_1D,
                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::OFF);
         _ss->setTextureMode(i,GL_TEXTURE_3D,
                          osg::StateAttribute::OVERRIDE|osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_S,
                           osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_T,
                           osg::StateAttribute::ON);
         _ss->setTextureMode(i,GL_TEXTURE_GEN_R,
                           osg::StateAttribute::ON);
     
      }
      if(_useTM){
         _ss->setTextureAttributeAndModes(nTransferFunctions,_property.get(),
                                   osg::StateAttribute::OVERRIDE|osg::StateAttribute::ON);
      }
      _ss->setDataVariance(osg::Object::DYNAMIC);
      _tUnit = nTransferFunctions;
      
      //load the shader file 
      char directory[1024];
      if(_shaderDirectory){
         strcpy(directory,_shaderDirectory);
      }else{
         char* vesuitehome = getenv("VE_SUITE_HOME");
         strcpy(directory,vesuitehome);
        strcat(directory,"/VE_VolumeVis/cg_shaders/");
      }
      if(!_useTM){
         strcat(directory,"volumeTransferFunctions.cg");
         _setupCGShaderProgram(_ss.get(),directory,"densityTransfer");
      }else{
         strcat(directory,"gammaCorrection.cg");
         _setupCGShaderProgram(_ss.get(),directory,"gammaCorrection");
      }
   }
   _reinit = false;
}
/////////////////////////////////////////////////////////////////
osg::Texture3D* cfdOSGTransferShaderManager::GetPropertyTexture()
{
   if(_property.valid()){
      return _property.get();
   }
   return 0;
}
//////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_initTransferFunctions()
{
   if(_transferFunctions.empty()){
      //create 4 transfer functions
      _createTransferFunction(); 
      _createTransferFunction(); 
      _createTransferFunction(); 
      _createTransferFunction(true);
   }
}
///////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_createTransferFunction(bool useGamma,
                                                    bool clearList)
{
   if(clearList){
      if(_transferFunctions.size())
      {
         _transferFunctions.clear();
      }
   }
   GLint ga[256];
   GLfloat taoA = .10;
   for(unsigned int i = 0; i < 256; i++){
     if(i < taoA*255){
        ga[i] = 0;
     }else{
        ga[i] = 255.0*(i-/*255  */taoA*255)/(255.0-taoA*255);
     }
  }
   GLubyte* lutex = new GLubyte[256*4];
   //gamma table
   GLubyte gTable[256];
   double gamma = 2.4;
   //double y = 0;
   for (int i=0; i<256; i++) {       
      double y = (double)(i)/255.0;   
      y = pow(y, 1.0/gamma);     
      gTable[i] = (int) floor(255.0 * y + 0.5);  
   }
   if(useGamma){
      for (int i = 0; i < 256; i++){
        lutex[i*4    ] = 
        lutex[i*4 + 1] = 
        lutex[i*4 + 2] = 
        lutex[i*4 + 3] = (GLubyte)gTable[i];
      }
   }else{
      for (int i = 0; i < 256; i++){
         /*if(i < 75){
           lutex[i*4    ] =
           lutex[i*4 + 1] = 
           lutex[i*4 + 2] = (GLubyte)0;
         }else{*/
           lutex[i*4    ] =
           lutex[i*4 + 1] = 
           lutex[i*4 + 2] = (GLubyte)gTable[i];
         //}
        lutex[i*4 + 3] = (GLubyte)i;//ga[i];
      }
   }

   osg::ref_ptr<osg::Image> imageField = new osg::Image();
   imageField->allocateImage(256,1,1,GL_RGBA,GL_UNSIGNED_BYTE);
   imageField->setImage(256,1,1,GL_RGBA,GL_RGBA,GL_UNSIGNED_BYTE,lutex,
                osg::Image::USE_NEW_DELETE,1);
   imageField->setDataVariance(osg::Object::DYNAMIC);

   osg::ref_ptr<osg::Texture1D> trans = new osg::Texture1D;
   trans->setTextureSize(256);
   trans->setDataVariance(osg::Object::DYNAMIC);
   trans->setFilter(osg::Texture1D::MIN_FILTER,
                    osg::Texture1D::LINEAR);

   trans->setFilter(osg::Texture1D::MAG_FILTER,
                    osg::Texture1D::LINEAR);

   trans->setWrap(osg::Texture1D::WRAP_S,
                  osg::Texture1D::CLAMP);

   trans->setInternalFormat(GL_RGBA);
   trans->setImage(imageField.get());

   /*cfdUpdateableOSGTexture1d ut;
   ut.SetTransferFunctionType(cfdUpdateableOSGTexture1d::GAMMA_CORRECTION);

   ut.UpdateParam(cfdUpdateableOSGTexture1d::GAMMA_CORRECTION,1.4);*/
   //trans->setSubloadCallback(&ut);
   _transferFunctions.push_back(trans);
}
//////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetUseTextureManagerForProperty(bool tf)
{
   _useTM = tf;
}
///////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_initPropertyTexture()
{
   if(_fieldSize[0] && 
      _fieldSize[1] &&
      _fieldSize[2])
   { 
      unsigned int dataSize = _fieldSize[0]*_fieldSize[1]*_fieldSize[2];
      unsigned char* data = new unsigned char[dataSize*4];
      unsigned int i=0;
      unsigned int j=0;
      unsigned int k = 0;
      
      for(unsigned int p = 0; p < dataSize; p++){
         data[p*4   ] = (unsigned char)0;
         data[p*4 + 1] = (unsigned char)0;
         data[p*4 + 2] = (unsigned char)0;
         data[p*4 + 3] = (unsigned char)0;      
      }
      osg::ref_ptr<osg::Image> propertyField = new osg::Image();

      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_RGBA,GL_UNSIGNED_BYTE);

      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
		                      GL_RGBA,
		                      GL_RGBA,
			                   GL_UNSIGNED_BYTE,
                           data,
                           osg::Image::USE_NEW_DELETE,1);
      propertyField->setDataVariance(osg::Object::DYNAMIC);
      _property = new osg::Texture3D();
      _property->setDataVariance(osg::Object::DYNAMIC);
      
      _property->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _property->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
      _property->setInternalFormat(GL_RGBA);
      _property->setTextureSize(_fieldSize[0],
                             _fieldSize[1],
                             _fieldSize[2]);
      _property->setImage(propertyField.get());
      osg::ref_ptr<cfdSimpleTextureCallback> sCallback = new cfdSimpleTextureCallback();
      sCallback->setTextureSize(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2]);
      _property->setSubloadCallback(sCallback.get());
      
   }else{
      std::cout<<"Invalid field size!!"<<std::endl;
      std::cout<<"cfdOSGTransferShaderManager::_initPropertyTexture"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction)
{
   cfdUpdateableOSGTexture1d* temp;
   if(!_transferFunctions.size()){
      std::cout<<"Transfer functions not initialized!!!"<<std::endl;
      return;
   }
   switch(whichFunction){
      case 0:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(0)->getSubloadCallback());
         break;
      case 1:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(1)->getSubloadCallback());
         break;
      case 2:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(2)->getSubloadCallback());
         break;
      case 3:
      default:
         temp = dynamic_cast<cfdUpdateableOSGTexture1d*>(_transferFunctions.at(3)->getSubloadCallback());
         break;
   };
   temp->UpdateParam(type,param);
}
/////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetFieldSize(unsigned int x,unsigned int y,unsigned  int z)
{
   _fieldSize[0] = x;
   _fieldSize[1] = y;
   _fieldSize[2] = z;
}
/////////////////////////////////////////////////////////////////////////
cfdOSGTransferShaderManager& cfdOSGTransferShaderManager::operator=(const cfdOSGTransferShaderManager& sm)

{
   if(this != &sm){
      cfdOSGShaderManager::operator =(sm);
      _property = sm._property;
      int nTransferFuncts = sm._transferFunctions.size();
      for(int i = 0; i < nTransferFuncts; i++){
         _transferFunctions.push_back(sm._transferFunctions.at(i));
      }
      
      _reinit = sm._reinit;
      _fieldSize[0] = sm._fieldSize[0];
      _fieldSize[1] = sm._fieldSize[1];
      _fieldSize[2] = sm._fieldSize[2];
      _tm = sm._tm;
      _utCbk = sm._utCbk;
   }
   return *this;
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
#endif
