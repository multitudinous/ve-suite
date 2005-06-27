#ifndef CFD_COPY_TO_3DTEXTURE_STAGE_H
#define CFD_COPY_TO_3DTEXTURE_STAGE_H
#ifdef VE_PATENTED
#ifdef _OSG

#include <osg/Texture3D>
#include <osg/State>
#include <osg/StateSet>
#include <osgUtil/RenderStage>
#include <osg/FrameStamp>
#include "VE_TextureBased/cfdPBufferManager.h"
#include "VE_Installer/VEConfig.h"
namespace VE_TextureBased{
   class VE_TEXTURE_BASED_EXPORTS cfdCopyTo3DTextureStage:
      public osgUtil::RenderStage
   {
      public:
         cfdCopyTo3DTextureStage();
         virtual ~cfdCopyTo3DTextureStage();
         virtual osg::Object* cloneType() const { return new cfdCopyTo3DTextureStage(); }
         virtual osg::Object* clone(const osg::CopyOp&) const { return new cfdCopyTo3DTextureStage(); } // note only implements a clone of type.
         virtual bool isSameKindAs(const osg::Object* obj) const { return dynamic_cast<const cfdCopyTo3DTextureStage*>(obj)!=0L; }
         virtual const char* libraryName() const { return ""; }
         virtual const char* className() const { return "cfdCopyTo3DTextureStage"; }

         inline void setPBuffer(cfdPBufferManager* pbuffer) { _pbuffer = pbuffer; }
  
         void SetWhichSliceToUpdate(unsigned int whichSlice);

         //whichSlice corresponds to offset in the 3d texture
         //whichDir corresponds to the direction (x =0,y=1,z=2)
         inline void setSliceToUpdate(int whichSlice=0,int whichDir=2)
         {
            _whichSlice = whichSlice;
            _whichDir = whichDir;
         }
         void SetViewDirection(int whichDir=2){_whichDir = whichDir;}
         virtual void reset();
        
         void set3DTexture(osg::Texture3D* texture) { _texture = texture; }
         void SetShaderStateSet(osg::StateSet* ss);
         osg::Texture3D* getTexture() { return _texture.get(); }

         virtual void draw(osg::State& state,osgUtil::RenderLeaf*& previous);

      protected:   
  
         osg::ref_ptr<osg::FrameStamp> _fs;
         osg::ref_ptr<osg::Texture3D> _texture;
         osg::ref_ptr<osg::State> _localState;
         osg::ref_ptr<osg::StateSet> _shader;
         cfdPBufferManager* _pbuffer;

         unsigned int _whichSlice;
         unsigned int _whichDir;
         int _width;
         int _height;
        int _nSlices;
   };
}
#endif //OSG
#endif
#endif// CFD_COPY_TO_3DTEXTURE_STAGE_H

