#ifndef CFD_SIMPLE_TEXTURE_CALLBACK_H
#define CFD_SIMPLE_TEXTURE_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <osg/Texture3D>

class cfdSimpleTextureCallback : public  osg::Texture3D::SubloadCallback{
public:
   cfdSimpleTextureCallback();
   virtual ~cfdSimpleTextureCallback(){}
        
   void setTextureSize(const int width, const int height, const int depth)
   {
      _textureWidth = width;
      _textureHeight = height;
      _textureDepth= depth;
   }
   void SetIsLuminance(bool isLum){_isLuminance = isLum;} 
   void subload(const osg::Texture3D& texture,osg::State& state) const;
   void load(const osg::Texture3D& texture,osg::State&) const;
   
protected:
   bool _isLuminance;
   mutable GLsizei _textureWidth, _textureHeight,_textureDepth;
};
#endif //OSG
#endif //CFD_SIMPLE_TEXTURE_CALLBACK_H
#endif

