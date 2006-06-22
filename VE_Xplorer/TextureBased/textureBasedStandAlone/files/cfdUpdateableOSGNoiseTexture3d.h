#ifndef CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
#define CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
#ifdef VE_PATENTED
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg{
   class State;
};
#include <osg/Texture3D>

class cfdUpdateableOSGNoiseTexture3d : public  osg::Texture3D::SubloadCallback{
public:
   cfdUpdateableOSGNoiseTexture3d();
   cfdUpdateableOSGNoiseTexture3d(const cfdUpdateableOSGNoiseTexture3d& );
   virtual ~cfdUpdateableOSGNoiseTexture3d();

   void UpdateTaoH(GLfloat taoH);
   void UpdateTaoI(GLfloat taoI);
   void UpdateTaoA(GLfloat taoA);

   void subload(const osg::Texture3D& texture,osg::State& state) const;
   void load(const osg::Texture3D& texture,osg::State&) const;

   cfdUpdateableOSGNoiseTexture3d& operator=(const cfdUpdateableOSGNoiseTexture3d&);
protected:
   bool _needsUpdate() const;
   void _updateData();
   mutable GLsizei _textureWidth,_textureHeight,_textureDepth;

   GLfloat _taoH;
   GLfloat _taoI;
   GLfloat _taoA;
   GLfloat _lastH;
   GLfloat _lastI;

   unsigned char* _data;
};
#endif //_OSG
#endif
#endif// CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
