#ifndef CFD_PBUFFER_QUAD_H
#define CFD_PBUFFER_QUAD_H
#ifdef _OSG
#include <osg/Drawable>
#ifdef CFD_USE_SHADERS
#include <osgNVCg/CgGeometry>
namespace osg
{
   class Texture3D;
   class State;
}
class cfdPBufferQuad :public osg::Drawable{//public osgNVCg::CgGeometry{
public:
   cfdPBufferQuad();
   cfdPBufferQuad(const cfdPBufferQuad& pbQuad,
                  const osg::CopyOp& copyop=osg::CopyOp::SHALLOW_COPY);
   META_Object(PBufferQuad,cfdPBufferQuad);

   void SetBBox(float* bbox);
   void SetNumberOfSlices(unsigned int ns);
   void SetUseAutoTexCoords(bool useAutoTCoords = false);
   void SetTextureDimensions(unsigned int w,unsigned int h,unsigned int d);
   void CalculateSlices();

   void SetTextureToUpdate(osg::Texture3D* texture);

   virtual void drawImplementation(osg::State& state)const;
protected:
   virtual bool computeBound() const;
   void _drawAutoTexCoords()const;
   void _drawHardCodedTCoords(osg::State& state)const;

   virtual ~cfdPBufferQuad();
   bool _useAutoTexCoords;
   bool _bbSet;
   bool _sliceCountSet;

   unsigned int _nSlices;
   unsigned int _curSlice;
   unsigned int _w;
   unsigned int _h;
   unsigned int _d;
   float* _nearFarSlices;
   float* _slices;
   float _bounds[6];
   float _eye[3];
   float _lookAt[3];
   float _deltaZ;
   osg::ref_ptr<osg::Texture3D> _texture;
};
#endif 
#endif// _OSG
#endif// CFD_PBUFFER_QUAD_H
