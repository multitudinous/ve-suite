#ifndef CFD_VOLUME_VISUALIZATION_H
#define CFD_VOLUME_VISUALIZATION_H
class cfdGroup;
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
namespace osg{
   class Node;
   class Geometry;
   class Texture1D;
   class Texture3D;
   class TexGen;
   class TexEnv;
   class Geode;
   class Billboard;
   class ClipNode;
   class TexGenNode;
   class Material;
   class Shape;
   class Image;
   class Switch;
}
#include "cfdVolumeSliceSwitchCallback.h"
#include "cfdUpdateTextureCallback.h"
#include "cfdTextureManager.h"

class cfdVolumeVisualization{
public:
   cfdVolumeVisualization();
   cfdVolumeVisualization(const cfdVolumeVisualization&);
   virtual ~cfdVolumeVisualization();

   enum VisMode{PLAY,STOP};
   enum Direction{FORWARD,BACKWARD};
   enum ClipPlane{XPLANE=0,YPLANE,ZPLANE,ARBITRARY};

   void SetPlayDirection(Direction dir);
   void SetPlayMode(VisMode mode);
   void SetSliceAlpha(float alpha = .5);
   void SetTextureUnit(int tUnit = 0);
   void SetVeboseFlag(bool flag);
   void SetShaderDirectory(char* shadDir);
#ifdef _OSG
   void SetStateSet(osg::StateSet* ss);
   void Set3DTextureData(osg::Texture3D* texture);
   void SetBoundingBox(float* bbox);
   void SetNumberofSlices(int nSlices = 100);
   
   void SetTextureManager(cfdTextureManager* tm);
   void UpdateStateSet(osg::StateSet* ss);
   void CreateNode();
   void AddClipPlane(ClipPlane direction,double* position);
   void RemoveClipPlane(ClipPlane direction);
   void UpdateClipPlanePosition(ClipPlane direction,double* newPosition);
   void ActivateVisualBBox();  
   void DeactivateVisualBBox();
   void UseNormalGraphicsPipeline();

   bool isCreated(){return _isCreated;}
#ifdef CFD_USE_SHADERS
   void EnableVolumeShader();
   void DisableVolumeShader();
   void EnableTransferShader();
   void DisableTransferShader();
#endif
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
#ifdef CFD_USE_SHADERS
   
   void _setupCGShaderPrograms(osg::StateSet *ss, 
                     char* fragProgramFileName,
                     char* fragFunctionName);
   void _initVolumeShader();
   void _initTransferFunctionShader();
   
   void _attachTransferFunctionsToStateSet(osg::StateSet* ss);
   void _attachPropertyTextureToStateSet(osg::StateSet* ss);
#endif
   osg::ref_ptr<osg::Group>_volumeVizNode;
   osg::ref_ptr<osg::TexGenNode> _texGenParams;
   osg::BoundingBox _bbox;
   osg::ref_ptr<osg::ClipNode> _clipNode;
   osg::ref_ptr<osg::StateSet> _stateSet;

   osg::ref_ptr<osg::StateSet> _scalarFragSS;
   osg::ref_ptr<osg::StateSet> _transferFunctionFragSS;
   osg::ref_ptr<osg::StateSet> _advectionFragSS;

   osg::ref_ptr<osg::Switch> _bboxSwitch;
   osg::ref_ptr<osg::Switch> _shaderSwitch;
   osg::ref_ptr<osg::Group> _noShaderGroup;
   osg::ref_ptr<osg::Group> _scalarFragGroup;
   osg::ref_ptr<osg::Group> _advectionVectorGroup;

   osg::ref_ptr<osg::Material> _material;
  
   osg::ref_ptr<osg::Texture3D> _texture;
   osg::ref_ptr<osg::Texture3D> _property;

   osg::ref_ptr<osg::Texture1D> _trans1;
   osg::ref_ptr<osg::Texture1D> _trans2;
   osg::ref_ptr<osg::Texture1D> _trans3;
   osg::ref_ptr<osg::Texture1D> _trans4;

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
#endif

};
#endif//OSG
#endif// CFD_VOLUME_VISUALIZATION_H
