#ifndef CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
#define CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
#ifdef _PERFORMER
#elif _OSG
#include <osg/Texture1D>

class cfdUpdateableOSGTexture1d: public  osg::Texture1D::SubloadCallback{
public:
   cfdUpdateableOSGTexture1d();
   cfdUpdateableOSGTexture1d(const cfdUpdateableOSGTexture1d& cb);
   virtual ~cfdUpdateableOSGTexture1d();

   enum TransType{ALPHA_CUTOFF,GAMMA_CORRECTION};
   
   void UpdateParam(TransType type,GLfloat param);
   void SetGamma(GLfloat gamma);
   void SetAlphaCutoff(GLfloat aCutoff);
   void SetTransferFunctionType(TransType type);
   
   void subload(const osg::Texture1D& texture,osg::State& state) const;
   void load(const osg::Texture1D& texture,osg::State&) const;

   cfdUpdateableOSGTexture1d& operator=(const cfdUpdateableOSGTexture1d& cb);
protected:	
   void _updateData();
   bool _needsUpdate()const;

   unsigned char* _data;
   GLfloat _alphaCutoff;
   GLfloat _gamma;
   GLfloat _lastAlpha;
   GLfloat _lastGamma;

   TransType _type;
   mutable GLsizei _textureWidth,_oWidth;
};
#endif //_OSG
#endif //CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
