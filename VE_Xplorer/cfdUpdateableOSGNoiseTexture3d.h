#ifndef CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
#define CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg{
   class State;
};
#include <osg/Texture3d>

class cfdUpdateableOSGNoiseTexture3d{
public:
   cfdUpdateableOSGNoiseTexture3d();
   cfdUpdateableOSGNoiseTexture3d(const cfdUpdateableOSGNoiseTexture3d& );
   virtual ~cfdUpdateableOSGNoiseTexture3d();
   void SetState(osg::State* state);
   void SetNoiseTexture(osg::Texture3D* noise);
   void UpdateTaoH(GLfloat taoH);
   void UpdateTaoI(GLfloat taoI);
   osg::Texture3D* GetNoiseTexture();

   cfdUpdateableOSGNoiseTexture3d& operator=(const cfdUpdateableOSGNoiseTexture3d&);
protected:
   bool _needsUpdate();
   void _updateData();
   mutable GLsizei _textureWidth,_textureHeight,_textureDepth;

   GLfloat _taoH;
   GLfloat _taoI;
   GLfloat _lastH;
   GLfloat _lastI;

   unsigned char* _data;

   osg::ref_ptr<osg::State> _state;
   osg::ref_ptr<osg::Texture3D> _noise;
};
#endif //_OSG
#endif// CFD_UPDATEABLE_OSG_NOISE_TEXTURE_3D_H
