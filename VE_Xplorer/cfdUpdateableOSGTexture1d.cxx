#ifdef _OSG
#include <osg/Texture1D>
#include <osg/Image>
#include <osg/State>
#include "cfdUpdateableOSGTexture1d.h"
#include <iostream>
//////////////////////////////////////////////////////////
//Constructors                                          //
//////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::cfdUpdateableOSGTexture1d()
{
   _alphaCutoff = .1;
   _gamma = 1.4;
   _lastAlpha = 0.0;
   _lastGamma = 0.0;
   _type = GAMMA_CORRECTION;
   _textureWidth = 0;
   _oWidth = -1;
   _data = new unsigned char[256];
   for(int i = 0; i < 256; i++){
      _data[i] = 0;
   }
}
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::cfdUpdateableOSGTexture1d(const
                                                cfdUpdateableOSGTexture1d& cb)
{
   _lastAlpha = cb._lastAlpha;
   _lastGamma = cb._lastGamma;
   _alphaCutoff = cb._alphaCutoff;
   _gamma = cb._gamma;
   _type = cb._type;
   _textureWidth = cb._textureWidth;
   _oWidth = cb._oWidth;
   _data = 0;
   _data = new unsigned char[_textureWidth];
   for(int i = 0; i < _textureWidth; i++){
      _data[i] = cb._data[i];
   }
   _texture1d = cb._texture1d;
   
}
//////////////////////////////////////////////////////////
//Destructor                                            //
//////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d::~cfdUpdateableOSGTexture1d()
{
   if(_data){
      delete [] _data;
      _data = 0;
   }
   /*if(_texture1d.valid()){
      _texture1d.release();
   }*/
}
/////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetTexture1D(osg::Texture1D* texture)
{
   _texture1d = texture;
   _texture1d->getTextureSize(_textureWidth);
}
/////////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::UpdateParam(TransType type,GLfloat param)
{
   if(type == GAMMA_CORRECTION){
      SetGamma(param);
   }else{
      SetAlphaCutoff(param);
   }
}
///////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetGamma(GLfloat gamma)
{  
   _gamma = gamma;   
   if(_needsUpdate()){
      _updateData();
      _lastGamma = _gamma;
   }
}
///////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetAlphaCutoff(GLfloat aCutoff)
{
   _alphaCutoff = aCutoff;
   if(_needsUpdate()){
      _updateData();
      _lastAlpha = _alphaCutoff;
   }
}
///////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::SetTransferFunctionType(TransType type)
{
   _type = type;
}
////////////////////////////////////////////////
bool cfdUpdateableOSGTexture1d::_needsUpdate()
{
   if(_type == GAMMA_CORRECTION){
      return ((_lastGamma == _gamma)?false:true);
   }else{
      return ((_lastAlpha == _alphaCutoff)?false:true);
   }
   return false;
}
/////////////////////////////////////////////
void cfdUpdateableOSGTexture1d::_updateData()
{
   if(_texture1d.valid()){
      if(_data){
         if(_textureWidth != _oWidth){
            delete [] _data;
           _data = 0;
           _data = new unsigned char[_textureWidth];
           _oWidth = _textureWidth;
         }
      }else{
         _data = new unsigned char[_textureWidth];
      }
      
      if(_type == GAMMA_CORRECTION){
         int* gTable = new int[_textureWidth];
         for (int i=0; i<_textureWidth; i++) {       
            double y = (double)(i)/((double)(_textureWidth-1.0));   
            y = pow(y, 1.0/_gamma);     
            gTable[i] = (int) floor((_textureWidth-1.0) * y + 0.5);  
         }
         for (int i = 0; i < _textureWidth; i++){
            _data[i] = (GLubyte)gTable[i]; 
         }
         if(gTable){
            delete [] gTable;
            gTable = 0;
         }
      }else{
         int cutoff = _alphaCutoff*_textureWidth;
         int twminusone = _textureWidth-1;
         for(int i = 0; i < _textureWidth; i++){
            if(i < cutoff){
               _data[i] = (unsigned char)0;
            }else{
              _data[i] = (unsigned char)(twminusone*(twminusone - i)/(twminusone-cutoff));
            }
         }
      }
      //update the texture
      _texture1d->getImage()->setImage(_textureWidth,0,0,
                                   GL_RGBA,GL_RGBA, 
                                   GL_UNSIGNED_BYTE,
                                   _data,
                                   osg::Image::USE_NEW_DELETE,1);
   }else{
      std::cout<<"Invalid 1d texture!!"<<std::endl;
      std::cout<<"cfdUpdateableOSGTexture1d::_updateData()"<<std::endl;
   }
   
}
////////////////////////////////////////////////////////////////////////////////
//equal operator
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGTexture1d& cfdUpdateableOSGTexture1d::operator=(const cfdUpdateableOSGTexture1d& cb)
{
   if(this != &cb){
      _lastAlpha = cb._lastAlpha;
      _lastGamma = cb._lastGamma;
      _alphaCutoff = cb._alphaCutoff;
      _gamma = cb._gamma;
      _type = cb._type;
      _textureWidth = cb._textureWidth;
      if(_data){
         if(_textureWidth != _oWidth){
            delete [] _data;
           _data = 0;
         }
      }else{
         _data = new unsigned char[_textureWidth];
      }
      
      _oWidth = cb._oWidth;
      for(int i = 0; i < _textureWidth; i++){
         _data[i] = cb._data[i];
      }
      _texture1d = cb._texture1d;
   }
   return *this;
}
#endif
