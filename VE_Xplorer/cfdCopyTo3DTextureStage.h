#ifndef CFD_COPY_TO_3DTEXTURE_STAGE_H
#define CFD_COPY_TO_3DTEXTURE_STAGE_H

#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osg/Texture3D>
#include <osg/State>
#include <osgUtil/RenderStage>
#include "cfdPBufferManager.h"


class cfdCopyTo3DTextureStage: public osgUtil::RenderStage
{
public:
   cfdCopyTo3DTextureStage();

   virtual osg::Object* cloneType() const { return new cfdCopyTo3DTextureStage(); }
   virtual osg::Object* clone(const osg::CopyOp&) const { return new cfdCopyTo3DTextureStage(); } // note only implements a clone of type.
   virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const cfdCopyTo3DTextureStage*>(obj)!=0L; }
   virtual const char* libraryName() const { return ""; }
   virtual const char* className() const { return "cfdCopyTo3DTextureStage"; }

   inline void setPBuffer(cfdPBufferManager* pbuffer) { _pbuffer = pbuffer; }
        
   //whichSlice corresponds to offset in the 3d texture
   //whichDir corresponds to the direction (x =0,y=1,z=2)
   inline void setSliceToUpdate(int whichSlice=0,int whichDir=2)
   {
      _whichSlice = whichSlice;
      _whichDir = whichDir;
   }

   virtual void reset();
        
   void set3DTexture(osg::Texture3D* texture) { _texture = texture; }
   osg::Texture3D* getTexture() { return _texture.get(); }

   virtual void draw(osg::State& state,osgUtil::RenderLeaf*& previous);

protected:   
   virtual ~cfdCopyTo3DTextureStage();
        
   osg::ref_ptr<osg::Texture3D> _texture;
   osg::ref_ptr<osg::State> _localState;
   cfdPBufferManager* _pbuffer;

   int _whichSlice;
   int _whichDir;
   int _width;
   int _height;
   int _nSlices;
};
#endif
#endif //OSG
#endif// CFD_COPY_TO_3DTEXTURE_STAGE_H

