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
}


#include "cfdVolumeSliceSwitchCallback.h"
#include "cfdUpdateTextureCallback.h"
#include "cfdTextureManager.h"
#include "cfdUpdateableOSGTexture1d.h"

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
   
   void CreateNode();
   void AddClipPlane(ClipPlane direction,double* position);
   void RemoveClipPlane(ClipPlane direction);
   void UpdateClipPlanePosition(ClipPlane direction,double* newPosition);
   

   bool isCreated(){return _isCreated;}

   cfdUpdateTextureCallback* GetUpdateCallback(){return _utCbk;}
   
   osg::ref_ptr<osg::StateSet> GetStateSet();
   osg::ref_ptr<osg::Texture3D> GetTextureData();
   osg::ref_ptr<osg::Group> GetVolumeVisNode();
   

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
   int _nSlices;
   int _tUnit;
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

   char* _shaderDirectory;
   cfdTextureManager* _tm;
#ifdef _OSG

   osg::ref_ptr<osg::Group>_volumeVizNode;
   osg::ref_ptr<osg::TexGenNode> _texGenParams;
   osg::BoundingBox* _bbox;
   osg::ref_ptr<osg::ClipNode> _clipNode;
   osg::ref_ptr<osg::StateSet> _stateSet;

   
   osg::ref_ptr<osg::Group> _noShaderGroup;

   osg::ref_ptr<osg::Material> _material;
  
   osg::ref_ptr<osg::Texture3D> _texture;

  
   osg::ref_ptr<osg::Image> _image;
   osg::ref_ptr<osg::Geode> _slices;

   osg::ref_ptr<osg::Geometry> _posXSlices;
   osg::ref_ptr<osg::Geometry> _negXSlices;
   osg::ref_ptr<osg::Geometry> _posYSlices;
   osg::ref_ptr<osg::Geometry> _negYSlices;
   osg::ref_ptr<osg::Geometry> _posZSlices;
   osg::ref_ptr<osg::Geometry> _negZSlices;

   osg::ref_ptr<osg::State> _state;

   cfdVolumeSliceSwitchCallback* _vSSCbk;
   cfdUpdateTextureCallback* _utCbk;
   

#endif

};
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
