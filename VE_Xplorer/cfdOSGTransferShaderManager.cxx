#include <iostream>
#ifdef _OSG
#include <osg/Texture3D>
#include <osg/Texture1D>
#include <osg/BlendFunc>
#include <osg/TexEnv>

#ifdef CFD_USE_SHADERS
#include "cfdOSGTransferShaderManager.h"
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::cfdOSGTransferShaderManager()
:cfdOSGShaderManager()
{
   _reinit = true;
   _fieldSize[0] = 0;
   _fieldSize[1] = 0;
   _fieldSize[2] = 0;
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
}
///////////////////////////////////////////////////////////
cfdOSGTransferShaderManager::~cfdOSGTransferShaderManager()
{
   if(_transferFunctions.size()){
      _transferFunctions.clear();
   }
}
///////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::SetPropertyTexture(osg::Texture3D* property)
{
   _property = property;
}
////////////////////////////////////////
void cfdOSGTransferShaderManager::Init()
{
   _initTransferFunctions();
   //_initPropertyTexture();
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
   if(_reinit&& _property.valid()){
      _ss->setTextureAttributeAndModes(0,_property.get(),osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
      _ss->setTextureMode(0,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
      _ss->setTextureAttributeAndModes(0,new osg::TexEnv(osg::TexEnv::REPLACE),
		                    osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON);
      int nTransferFunctions = _transferFunctions.size();
      for(int i =0; i < nTransferFunctions; i++){
         osg::ref_ptr<osg::Texture1D> trans = _transferFunctions.at(i).GetTexture();
         _ss->setTextureAttributeAndModes(i+1,trans.get(),osg::StateAttribute::ON);
      }
      //load the shader file 
      char directory[1024];
      if(_shaderDirectory){
         strcpy(directory,_shaderDirectory);
      }else{
         char* vesuitehome = getenv("VE_SUITE_HOME");
         strcpy(directory,vesuitehome);
        strcat(directory,"/VE_Xplorer/cg_shaders/");
      }
      strcat(directory,"volumeTransferFunctions.cg");
      _setupCGShaderProgram(_ss.get(),directory,"densityTransfer");
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
   //create 4 transfer functions
   _createTransferFunction(false); 
   _createTransferFunction(false); 
   _createTransferFunction(false); 
   _createTransferFunction(false); 
}
/////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_createTransferFunction(bool clearList)
{
   if(clearList){
      if(_transferFunctions.size())
	       _transferFunctions.clear();
   }
   GLubyte lutex[256*2];
   //gamma table
   GLubyte gTable[256];
   double gamma = 2.5;
   double y = 0;
   for (int i=0; i<256; i++) {       
      double y = (double)(i)/255.0;   
      y = pow(y, 1.0/gamma);     
      gTable[i] = (int) floor(255.0 * y + 0.5);  
   }
   for (int i = 0; i < 256; i++){
     lutex[i*2    ] = (GLubyte)gTable[i];
     lutex[i*2 + 1] = (GLubyte)i;
   }

   osg::ref_ptr<osg::Image> data = new osg::Image();
   data->allocateImage(256,1,1,GL_LUMINANCE_ALPHA,GL_UNSIGNED_BYTE);
   data->setImage(256,1,1,GL_LUMINANCE_ALPHA,GL_LUMINANCE_ALPHA,GL_UNSIGNED_BYTE,lutex,
                osg::Image::USE_NEW_DELETE);

   osg::ref_ptr<osg::Texture1D>trans = new osg::Texture1D;
   trans->setTextureSize(256);
   trans->setFilter(osg::Texture1D::MIN_FILTER,
                    osg::Texture1D::LINEAR);

   trans->setFilter(osg::Texture1D::MAG_FILTER,
                    osg::Texture1D::LINEAR);

   trans->setWrap(osg::Texture1D::WRAP_S,
                  osg::Texture3D::CLAMP);

   trans->setInternalFormat(GL_LUMINANCE);
   trans->setImage(data.get());

   cfdUpdateableOSGTexture1d ut;
   ut.SetTransferFunctionType(cfdUpdateableOSGTexture1d::GAMMA_CORRECTION);
   ut.SetTexture1D(trans.get());

   ut.UpdateParam(cfdUpdateableOSGTexture1d::GAMMA_CORRECTION,1.4);
   _transferFunctions.push_back(ut);
}
///////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::_initPropertyTexture()
{
   if(_fieldSize[0] &&
      _fieldSize[1] &&
      _fieldSize[2]){ 
      int dataSize = _fieldSize[0]*_fieldSize[1]*_fieldSize[2];
      unsigned char* data = new unsigned char[dataSize*4];
   
      for(int p = 0; p < dataSize; p++){
      
         data[p*4   ] = (unsigned char)0;

         data[p*4 + 1] = (unsigned char)0;
         data[p*4 + 2] = (unsigned char)0;
     
         data[p*4 + 3] = (unsigned char)0;      
      }
      osg::Image* propertyField = new osg::Image();

      propertyField->allocateImage(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2],
                                GL_RGBA,GL_UNSIGNED_BYTE);

      propertyField->setImage(_fieldSize[0], _fieldSize[1], _fieldSize[2],
		                      GL_RGBA,
		                      GL_RGBA,
			                   GL_UNSIGNED_BYTE,
                           data,/*may need a function to init empty data*/
                           osg::Image::USE_NEW_DELETE,1);

      _property = new osg::Texture3D();
      _property->setDataVariance(osg::Object::DYNAMIC);
      
      osg::TexEnv* texEnv = new osg::TexEnv();
      texEnv->setMode(osg::TexEnv::REPLACE);

      _property->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _property->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
      _property->setInternalFormat(GL_RGBA);
      _property->setTextureSize(_fieldSize[0],
                                _fieldSize[1],
                                _fieldSize[2]);
      _property->setImage(propertyField);
      if(data){
         delete [] data;
         data = 0;
      }
   }else{
      std::cout<<"Invalid field size!!"<<std::endl;
      std::cout<<"cfdOSGTransferShaderManager::_initPropertyTexture"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////////////////////////////////
void cfdOSGTransferShaderManager::UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction)
{
   cfdUpdateableOSGTexture1d temp;
   if(!_transferFunctions.size()){
      std::cout<<"Transfer functions not initialized!!!"<<std::endl;
      return;
   }
   switch(whichFunction){
      case 0:
         temp = _transferFunctions.at(0);
         break;
      case 1:
         temp = _transferFunctions.at(1);
         break;
      case 2:
         temp = _transferFunctions.at(2);
         break;
      case 3:
      default:
         temp = _transferFunctions.at(3);
         break;
   };
   temp.UpdateParam(type,param);
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
   }
   return *this;
}
#endif// _CFD_USE_SHADERS
#endif//_OSG
