#ifndef CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
#define CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
#ifdef _PERFORMER
#elif _OSG
namespace osg{
   class Texture1D;
}

class cfdUpdateableOSGTexture1d{
public:
   cfdUpdateableOSGTexture1d();
   cfdUpdateableOSGTexture1d(const cfdUpdateableOSGTexture1d& cb);
   virtual ~cfdUpdateableOSGTexture1d();

   enum TransType{ALPHA_CUTOFF,GAMMA_CORRECTION};
   
   void UpdateParam(TransType type,GLfloat param);
   void SetTexture1D(osg::Texture1D* texture);
   void SetGamma(GLfloat gamma);
   void SetAlphaCutoff(GLfloat aCutoff);
   void SetTransferFunctionType(TransType type);

   cfdUpdateableOSGTexture1d& operator=(const cfdUpdateableOSGTexture1d& cb);
protected:	
   void _updateData();
   bool _needsUpdate();

   unsigned char* _data;
   osg::ref_ptr<osg::Texture1D> _texture1d;
   GLfloat _alphaCutoff;
   GLfloat _gamma;
   GLfloat _lastAlpha;
   GLfloat _lastGamma;

   TransType _type;
   mutable GLsizei _textureWidth,_oWidth;
};
#endif //_OSG
#endif //CFD_UPDATEABLE_TRANSFER_FUNCTION_TEXTURE_1D_H
