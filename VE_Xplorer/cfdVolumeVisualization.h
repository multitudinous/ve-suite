#ifndef CFD_VOLUME_VISUALIZATION_H
#define CFD_VOLUME_VISUALIZATION_H
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG

#include <osg/Node>
#include <osg/Geometry>
#include <osg/Texture3D>
#include <osg/TexGen>
#include <osg/TexEnv>
#include <osg/Geode>
#include <osg/Billboard>
#include <osg/ClipNode>
#include <osg/TexGenNode>
#include <osg/Material>
#include <osg/Shape>
#include <osg/Image>

#include "cfdVolumeSliceSwitchCallback.h"
#include "cfdUpdateTextureCallback.h"
#include "cfdTextureManager.h"

class cfdVolumeVisualization{
public:
   cfdVolumeVisualization();
   cfdVolumeVisualization(const cfdVolumeVisualization&);
   virtual ~cfdVolumeVisualization();

   enum VisMode{PLAY,STOP};

   void SetPlayMode(VisMode mode);
   void SetStateSet(osg::StateSet* ss);
   void Set3DTextureData(osg::Texture3D* texture);
   void SetBoundingBox(float* bbox);
   void SetNumberofSlices(int nSlices = 100);
   void SetSliceAlpha(float alpha = .5);
   void SetTextureUnit(int tUnit = 0);
   void SetVeboseFlag(bool flag);
   void SetTextureManager(cfdTextureManager* tm);
   void UpdateStateSet(osg::StateSet* ss);
   void CreateNode();

   cfdUpdateTextureCallback* GetUpdateCallback(){return _utCbk;}
   
   osg::ref_ptr<osg::StateSet> GetStateSet();
   osg::ref_ptr<osg::Texture3D> GetTextureData();
   osg::ref_ptr<osg::Group> GetVolumeVisNode();

   cfdVolumeVisualization& operator=(const cfdVolumeVisualization& rhs);
protected:
   VisMode _mode;
   bool _verbose;
   int _nSlices;
   int _tUnit;
   float _alpha;
   void _createClipCube();
   void _buildGraph();
   void _createClipNode();
   void _createStateSet();
   void _attachTextureToStateSet();
   void _createTexGenNode();
   void _createVolumeBillboardSlices();
   void _buildAxisDependentGeometry();

   cfdTextureManager* _tm;

   osg::ref_ptr<osg::Group> _volumeVizNode;
   osg::ref_ptr<osg::TexGenNode> _texGenParams;
   osg::BoundingBox _bbox;
   osg::ref_ptr<osg::StateSet> _stateSet;
   osg::ref_ptr<osg::Material> _material;
   osg::ref_ptr<osg::Texture3D> _texture;
   osg::ref_ptr<osg::Image> _image;
   osg::ref_ptr<osg::Geode> _slices;
   osg::ref_ptr<osg::Group> _visualBoundingBox;
   osg::ref_ptr<osg::Geometry> _posXSlices;
   osg::ref_ptr<osg::Geometry> _negXSlices;
   osg::ref_ptr<osg::Geometry> _posYSlices;
   osg::ref_ptr<osg::Geometry> _negYSlices;
   osg::ref_ptr<osg::Geometry> _posZSlices;
   osg::ref_ptr<osg::Geometry> _negZSlices;
   cfdVolumeSliceSwitchCallback* _vSSCbk;
   cfdUpdateTextureCallback* _utCbk;

};
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
