#include <iostream>
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG

#include <osg/State>
#include "cfdUpdateableOSGNoiseTexture3d.h" 
////////////////////////////////////////////////////////////////
//Constructors                                                //
////////////////////////////////////////////////////////////////
cfdUpdateableOSGNoiseTexture3d::cfdUpdateableOSGNoiseTexture3d()
{
   _textureWidth = 0;
   _textureHeight = 0;
   _textureDepth = 0;
   _taoH = .9;
   _taoI = .1;
   _lastH = -1;
   _lastI = -1;
   _data = 0;
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
   _noise = uNT._noise;
   _lastH = uNT._lastH;
   _lastI = uNT._lastI;
   _state = uNT._state;
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
   if(_noise.valid()){
      if(_data){
         delete [] _data;
	      _data = 0;
      }
      int nPixels = _textureWidth*_textureHeight*_textureDepth;
      if(nPixels){
         _data = new unsigned char[nPixels*4];
	      GLint hI[256];
         GLint gI[256];
         for(int i = 0; i < 256; i++){
            if(i < _taoI*255){
               gI[i] = 0;
            }else{
               gI[i] = 255;
            }
         }

         //build hI transfer
         for(i = 0; i < 256; i++){
            if(i < _taoH*255){
               hI[i] = 0;
            }else{
               hI[i] = i;
            }
         }
	      int phase[32][32][32];
         for(int i = 0; i <32; i++)
            for(int j = 0; j <32; j++)
               for(int k = 0; k <32; k++)
                  phase[i][j][k] = rand()%256;

        GLint t = 0;
        int pCount = 0;
        for(int i = 0; i < 32; i++){
            t = i*256/32;
            for(int j = 0; j < 32; j++){
               for(int k = 0; k < 32; k++){
      
                  _data[pCount*4    ] = (GLubyte)((hI[(phase[i][j][k]+t) % 255])*
                                           (gI[(phase[k][j][i]+t) % 255])/*
                                           (ga[(phase[k][j][i]+t) % 255])*/);          
                  _data[pCount*4 + 1] = (GLubyte)phase[i][j][k];

                  _data[pCount*4 + 2] = (GLubyte) ((gI[(phase[i][j][k] + t) % 255])*
			                                       (hI[(phase[k][j][i] + t) % 255]));
                  _data[pCount*4 + 3] = (GLubyte)phase[k][j][i];
                  pCount++;
	       }
	    }
	 } 
	 _noise->getExtensions(_state.get()->getContextID(),false)->glTexImage3D(GL_TEXTURE_3D, 0, 
                                          GL_RGBA, 
                                          _textureWidth,
                                          _textureHeight,
                                          _textureDepth,
                                          0, GL_RGBA, 
                                          GL_UNSIGNED_BYTE, 
                                          _data);
	 
      }else{
         std::cout<<"Texture dimensions invalid!!"<<std::endl;
         std::cout<<"cfdUpdateableOSGNoiseTexture3d::_updateData()"<<std::endl;
      }
   }else{
      std::cout<<"Invalid noise texture!!"<<std::endl;
      std::cout<<"cfdUpdateableOSGNoiseTexture3d::_updateData()"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////
osg::Texture3D* cfdUpdateableOSGNoiseTexture3d::GetNoiseTexture()
{
   if(_noise.valid()){
      return _noise.get();
   }
   return 0;
}
///////////////////////////////////////////////////
bool cfdUpdateableOSGNoiseTexture3d::_needsUpdate()
{
   return (_lastI ==_taoI)?((_lastH ==_taoH)?false:true):true;
}
//////////////////////////////////////////////////////////////////////////////   
void cfdUpdateableOSGNoiseTexture3d::SetNoiseTexture(osg::Texture3D* noiseTex)
{
   _noise = noiseTex;
   _noise->getTextureSize(_textureWidth,_textureHeight,_textureDepth);
}
////////////////////////////////////////////////////////////////
void cfdUpdateableOSGNoiseTexture3d::SetState(osg::State* state)
{
   _state = state;
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
      _noise = uNT._noise;
      _lastH = uNT._lastH;
      _lastI = uNT._lastI;
      _state = uNT._state;
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
