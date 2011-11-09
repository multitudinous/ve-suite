#ifndef TEXTURESUBLOADER_H
#define TEXTURESUBLOADER_H

#include <osg/Texture2D>

// --------------------------------------------------------------------------
// This is the necessary callback class for doing an image overlay directly
// onto a texture (aka subload).

class TextureSubloader : public osg::Texture2D::SubloadCallback
   {
   public:
      TextureSubloader() : xOffset(0), yOffset(0), doSubload(false) {}
      ~TextureSubloader() {}

      // create the OpenGL texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
      virtual void load(const osg::Texture2D &texture, osg::State &state) const
         {
         const osg::Image *image = texture.getImage();
         if (image)  // texture must have an image to work with.
            glTexImage2D(GL_TEXTURE_2D, 0, image->getPixelFormat(), image->s(), image->t(), 0, image->getPixelFormat(), image->getDataType(), image->data());
         }

      // overlay the image onto the texture. A necessary override of osg::Texture2D::SubloadCallback (overrides a pure virtual).
      virtual void subload(const osg::Texture2D &texture, osg::State &state) const
         {
         if ((doSubload == false) || (subImg.valid() == false))
            return;
         // copy the image into the current texture (the one currently bound / selected) at the given offsets.
         glEnable(GL_TEXTURE_2D);         // make sure 2D textures are enabled.
         glPixelStorei(GL_UNPACK_ALIGNMENT, subImg->getPacking());      // image unpacking.
         // it probably doesn't make sense to subload to a mipmap, but in any event, select the nearest one and operate on that.
         // in the case of a mipmapped texture, it is especially important to set these filters, or the result won't be shown.
         // a texture that doesn't have defined filters is treated as mipmapped.
         glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
         glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
         // copy the image into place.
         glTexSubImage2D(GL_TEXTURE_2D, 0, xOffset, yOffset, subImg->s(), subImg->t(), subImg->getPixelFormat(), subImg->getDataType(), subImg->data());
         doSubload = false;               // completed the subload. Need another call to Update() before the next one.
         subImg = 0;                      // don't hold on to the image pointer any longer.
         }

      // tell the next subload callback to copy the input image to the specified offsets
      // in the texture for this.
      bool Update(osg::Image *img, int xOff, int yOff)
         {
         if (LoadPending())
            return false;
         subImg = img;
         xOffset = xOff;
         yOffset = yOff;
         doSubload = true;
         return true;
         }

      bool LoadPending() {if (doSubload && subImg) return true; return false;}

   protected:
      int xOffset;                              // the X offset for the next subload operation.
      int yOffset;                              // the Y offset for the next subload operation.
      mutable bool doSubload;                   // true if should do a subload copy with next subload() callback for the texture.
      mutable osg::ref_ptr<osg::Image> subImg;  // a pointer to an image to overlay onto the texture for this.
   };

#endif