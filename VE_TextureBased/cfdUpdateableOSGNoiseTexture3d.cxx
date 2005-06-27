#ifdef VE_PATENTED
#include <iostream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG

#include <osg/State>
#include "VE_TextureBased/cfdUpdateableOSGNoiseTexture3d.h" 
using namespace VE_TextureBased;
////////////////////////////////////////////////////////////////
//Constructors                                                //
////////////////////////////////////////////////////////////////
cfdUpdateableOSGNoiseTexture3d::cfdUpdateableOSGNoiseTexture3d()
{
   _textureWidth = 32;
   _textureHeight = 32;
   _textureDepth = 32;
   _taoH = .01;
   _taoI = .1;
   _taoA = .9;
   _lastH = _taoH;
   _lastI = _taoI;
   _data = 0;
   _updateData();
}
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGNoiseTexture3d::cfdUpdateableOSGNoiseTexture3d(
const cfdUpdateableOSGNoiseTexture3d& uNT)
{
   _textureWidth = uNT._textureWidth;
   _textureHeight = uNT._textureHeight;
   _textureDepth = uNT._textureDepth;
   _taoH = uNT._taoH;
   _taoI = uNT._taoI;
   _lastH = uNT._lastH;
   _lastI = uNT._lastI;
   int nPixels = _textureWidth*_textureHeight*_textureDepth;
   if(_data){
      delete [] _data;
      _data = 0;
   }
   if(nPixels){
      _data = new unsigned char[nPixels];
   }
   for(int i = 0; i < nPixels; i++){
      _data[i] = uNT._data[i];
   }
}
/////////////////////////////////////////////////////////////////
//Destructor                                                   //
/////////////////////////////////////////////////////////////////
cfdUpdateableOSGNoiseTexture3d::~cfdUpdateableOSGNoiseTexture3d()
{
   if(_data){
      delete [] _data;
      _data = 0;
   }
}
///////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::_updateData()
{
   int nPixels = 32*32*32;
   if(!_data)
      _data = new unsigned char[nPixels*4];
	GLint hI[256];
   GLint gI[256];
   GLint ga[256];
   for(int i = 0; i < 256; i++){
      if(i < _taoI*255){
         gI[i] = 0;
      }else{
         gI[i] = 255;
      }
   }
   float taoAlpha = .9;
   //build ga
  for(unsigned int i = 0; i < 256; i++){
     if(i < _taoA*255){
        ga[i] = 0;
     }else{
        ga[i] = 255.0*(i-/*255  */_taoA*255)/(255.0-_taoA*255);
     }
  }

   //build hI transfer
   for(unsigned int i = 0; i < 256; i++){
      if(i < _taoH*255){
         hI[i] = 0;
      }else{
         hI[i] = i;
      }
   }
   srand( (unsigned)time( NULL ) );
	int phase[32*32*32];
   int index = 0;
   for(int i = 0; i <32; i++)
      for(int j = 0; j <32; j++)
         for(int k = 0; k <32; k++)
            phase[index++] = rand()%256;

   unsigned int w[256];
   for (int i = 0; i < 256; i++) 
      w[i] = i < 127 ? 0 : 255;

   GLint t = 0;

   int pCount = 0;
   for(int i = 0; i < 32; i++){
      t = i*256/32;
      for(int j = 0; j < 32; j++){
         for(int k = 0; k < 32; k++){
            _data[pCount*4    ] = (unsigned char)((w[(phase[rand()%index]) ]));                          
            _data[pCount*4 + 1] = (unsigned char)(phase[rand()%index]);
            _data[pCount*4  +2] = (unsigned char)(w[(phase[rand()%index])]);
            _data[pCount*4  +3] = (unsigned char)(phase[rand()%index]);//(ga[(phase[k][i][j]+t) % 255]);
            pCount++;
	       }
	    }
   } 
}
/////////////////////////////////////////////////////////
bool cfdUpdateableOSGNoiseTexture3d::_needsUpdate() const
{
   return (_lastI ==_taoI)?((_lastH ==_taoH)?false:true):true;
}
/////////////////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::UpdateTaoH(GLfloat taoH)
{
   _taoH = taoH;
   if(_needsUpdate()){
      _updateData();
      _lastH = taoH;
   }
}
/////////////////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::UpdateTaoI(GLfloat taoI)
{
   _taoI = taoI;
   if(_needsUpdate()){
      _updateData();
      _lastI = taoI;
   }
}
////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::load(const osg::Texture3D& texture,osg::State& state )const 
{
   //if(_needsUpdate()){
      texture.getExtensions(state.getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          (unsigned char*)_data);
   //}
}
//////////////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::subload(const osg::Texture3D& texture,osg::State& state) const
{
   if(_data&&_needsUpdate()){
      texture.getExtensions(state.getContextID(),false)->glTexSubImage3D(GL_TEXTURE_3D,
                          0,
                          0,0,0, 
                          _textureWidth,
                          _textureHeight,
                          _textureDepth, 
                          GL_RGBA, 
                          GL_UNSIGNED_BYTE,
                          (unsigned char*)_data);
   }   
}
////////////////////////////////////////////////////////////////////////////////
//equal operator                                                              //
////////////////////////////////////////////////////////////////////////////////
cfdUpdateableOSGNoiseTexture3d&
cfdUpdateableOSGNoiseTexture3d::operator=(const cfdUpdateableOSGNoiseTexture3d& uNT)
{
   if(this != &uNT){
      _textureWidth = uNT._textureWidth;
      _textureHeight = uNT._textureHeight;
      _textureDepth = uNT._textureDepth;
      _taoH = uNT._taoH;
      _taoI = uNT._taoI;
      _lastH = uNT._lastH;
      _lastI = uNT._lastI;
      int nPixels = _textureWidth*_textureHeight*_textureDepth;
      if(_data){
         delete [] _data;
         _data = 0;
      }
      if(nPixels){
         _data = new unsigned char[nPixels];
      }
      for(int i = 0; i < nPixels; i++){
         _data[i] = uNT._data[i];
      }
   }
   return *this;
}
#endif 
#endif
