#ifndef CFD_COPY_TEXTURE_CALLBACK_H
#define CFD_COPY_TEXTURE_CALLBACK_H
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg{
   class Node;
   class NodeVisitor;
   class State;
}
#include <osg/Texture3D>

//textures need to have a subload callback to work correctly
//with pbuffers
class cfdCopyTextureCallback : public  osg::Texture3D::SubloadCallback{
public:
   cfdCopyTextureCallback();
   cfdCopyTextureCallback(const cfdCopyTextureCallback& cb);
   virtual ~cfdCopyTextureCallback(){};
   enum SubloadMode {OFF,AUTO,IF_DIRTY};

   inline void setSubloadMode(const SubloadMode mode) { _subloadMode = mode; }
   inline const SubloadMode getSubloadMode() const { return _subloadMode; }
   inline void setSubloadTextureOffset(const int x, const int y,const int z)
   {
       _subloadTextureOffsetX = x;
       _subloadTextureOffsetY = y;
       _subloadTextureOffsetZ = z;
   }

   /** Get the texture subload texture offsets. */
   inline void getSubloadTextureOffset(int& x, int& y, int& z) const
   {
      x = _subloadTextureOffsetX;
      y = _subloadTextureOffsetY;
      z = _subloadTextureOffsetZ; 
   }
        
   /** Set the texture subload width. If width or height are zero then
   * the repsective size value is calculated from the source image sizes. */
   inline void setSubloadTextureSize(const int width, const int height, const int depth)
   {
      _textureWidth = width;
      _textureHeight = height;
      _textureDepth= depth;
   }

   /** Get the texture subload width. */
   inline void getSubloadTextureSize(int& width, int& height, int& depth) const
   {
      width = _textureWidth;
      height = _textureHeight;
      depth = _textureHeight;
   }
   /** Set the subload image offsets. */
   inline void setSubloadImageOffset(const int x, const int y, const int z)
   {
      _subloadImageOffsetX = x;
      _subloadImageOffsetY = y;
      _subloadImageOffsetZ = z;
   }

   /** Get the subload image offsets. */
   inline void getSubloadImageOffset(int& x, int& y, int& z) const
   {
      x = _subloadImageOffsetX;
      y = _subloadImageOffsetY;
      z = _subloadImageOffsetZ;
   }

   /** Set the image subload width. If width or height are zero then
   * the repsective size value is calculated from the source image sizes. */
  inline void setSubloadImageSize(const int width, const int height,const int depth)
  {
     _subloadImageWidth = width;
     _subloadImageHeight = height;
     _subloadImageDepth= depth;
  }

  /** Get the image subload width. */
  inline void getSubloadImageSize(int& width, int& height,int& depth) const
  {
     width = _subloadImageWidth;
     height = _subloadImageHeight;
     depth = _subloadImageDepth;
  }
    
   void subload(const osg::Texture3D& texture,osg::State& state) const{;}
   void load(const osg::Texture3D& texture,osg::State&) const;
   
protected:
   SubloadMode _subloadMode;
   mutable GLsizei _textureWidth, _textureHeight,_textureDepth;
   GLint _subloadTextureOffsetX, _subloadTextureOffsetY,_subloadTextureOffsetZ;
   GLint _subloadImageOffsetX, _subloadImageOffsetY,_subloadImageOffsetZ;
   GLsizei _subloadImageWidth, _subloadImageHeight,_subloadImageDepth;
};
#endif //OSG
#endif //CFD_COPY_TEXTURE_CALLBACK_H

