#ifndef CFD_VOLUME_VISUALIZATION_H
#define CFD_VOLUME_VISUALIZATION_H
class cfdGroup;
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg
{
   class Node;
   class Geometry;
   class Texture1D;
   class Texture3D;
   class TexGen;
   class TexEnv;
   class Geode;
   class ClipNode;
   class TexGenNode;
   class Material;
   class Shape;
   class Image;
   class Switch;
   class StateSet;
   class Group;
   class BoundingBox;
   class Billboard;
}
class cfdTextureMatrixCallback;
#include <osgUtil/CullVisitor>
#include <osg/TexMat>
#include <osg/Vec3>
#include "cfdUpdateTextureCallback.h"
#include "cfdTextureManager.h"
#ifdef CFD_USE_SHADERS
#include "cfdUpdateableOSGTexture1d.h"
#endif
class cfdVolumeVisualization{
public:
   cfdVolumeVisualization();
   cfdVolumeVisualization(const cfdVolumeVisualization&);
   virtual ~cfdVolumeVisualization();
   enum CfdTexUnit{PLAIN = 0,TRANS_1,TRANS_2,
                 TRANS_3,TRANS_4,PROPERTY,VELOCITY,NOISE};
   enum VisMode{PLAY,STOP};
   enum Direction{FORWARD,BACKWARD};
   enum ClipPlane{XPLANE=0,YPLANE,ZPLANE,ARBITRARY};

   void SetPlayDirection(Direction dir);
   void SetPlayMode(VisMode mode);
   void SetSliceAlpha(float alpha = .5);
   void SetVeboseFlag(bool flag);
   void SetShaderDirectory(char* shadDir);
#ifdef _OSG
   void SetStateSet(osg::StateSet* ss);
   void SetState(osg::State* state);
   void Set3DTextureData(osg::Texture3D* texture);
   void SetBoundingBox(float* bbox);
   void SetNumberofSlices(int nSlices = 100);
   void SetTextureManager(cfdTextureManager* tm);
   void SetCurrentTransientTexture(unsigned int ct);
   void DisableShaders();
   void CreateNode();
   void AddClipPlane(ClipPlane direction,double* position);
   void RemoveClipPlane(ClipPlane direction);
   void UpdateClipPlanePosition(ClipPlane direction,double* newPosition);
   

   bool isCreated(){return _isCreated;}
   unsigned int GetCurrentTransientTexture();
   cfdUpdateTextureCallback* GetUpdateCallback(){return _utCbk;}
   osg::Vec3f GetBBoxCenter(){return _center;}
   float* GetTextureScale(){return _scale;}
   osg::ref_ptr<osg::StateSet> GetStateSet();
   osg::ref_ptr<osg::Texture3D> GetTextureData();
   osg::ref_ptr<osg::Switch> GetVolumeVisNode();
   osg::ref_ptr<osg::Group> GetDecoratorAttachNode();
   

   cfdVolumeVisualization& operator=(const cfdVolumeVisualization& rhs);
#endif
  

protected:
   VisMode _mode;
   Direction _traverseDirection;
   bool _verbose;
   bool _isCreated;
   bool _useShaders;
   bool _volShaderIsActive;
   bool _transferShaderIsActive;
   unsigned int _nSlices;
   unsigned int _tUnit;
   float _alpha;
   void _createVisualBBox();
   void _createClipCube();
   void _buildGraph();
   void _createClipNode();
   void _createStateSet();
   void _attachTextureToStateSet(osg::StateSet* ss);
   void _createTexGenNode();
   void _createVolumeSlices();
   void _buildAxisDependentGeometry();
   void _buildSlices();

   char* _shaderDirectory;
   cfdTextureManager* _tm;
   osg::Vec3 _center;
   float _transRatio[3];
   float _diagonal;
   float _scale[3];
#ifdef _OSG

   osg::ref_ptr<osg::Switch> _volumeVizNode;
   osg::ref_ptr<osg::TexGenNode> _texGenParams;
   osg::BoundingBox* _bbox;
   osg::ref_ptr<osg::ClipNode> _clipNode;
   osg::ref_ptr<osg::StateSet> _stateSet;
   osg::ref_ptr<osg::Billboard> _billboard;

   osg::ref_ptr<osg::Group> _noShaderGroup;
   osg::ref_ptr<osg::Group> _decoratorAttachNode;
   osg::ref_ptr<osg::Texture3D> _texture;
   osg::ref_ptr<osg::Image> _image;
   osg::ref_ptr<osg::State> _state;
   cfdUpdateTextureCallback* _utCbk;
#endif

};
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
