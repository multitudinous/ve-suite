#include "cfdVolumeVisualization.h"

#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <iostream>
#ifdef CFD_USE_SHADERS
#include <osgNVCg/Context>
#include <osgNVCg/Program>
#include <osgNVCg/CgGeometry>
#include "cfdAdvectionSubGraph.h"
#endif
#include <osg/BlendFunc>
#include <osg/ClipPlane>
#include <osg/ClipNode>
#include <osg/Node>
#include <osg/Geometry>
#include <osg/Texture1D>
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
#include <osg/Switch>

////////////////////////////////////////////////
//Constructor                                 //
////////////////////////////////////////////////
cfdVolumeVisualization::cfdVolumeVisualization()
{
   _volumeVizNode  = 0;
   _texGenParams  = 0;
   _vSSCbk = 0;
   _utCbk = 0;
   _mode = STOP;
   _traverseDirection = FORWARD;
   _stateSet  = 0;
   _material  = 0;
   _texture  = 0;
   _slices  = 0;
   _nSlices = 100;
   _alpha = 0.5;
   _tUnit = 0;
   _verbose = 0;
   _tm = 0;
   _isCreated = false;
   _useShaders = false;
   _shaderDirectory = 0;
   _volShaderIsActive =  false;
   _transferShaderIsActive = false;

}
/////////////////////////////////////////////////////////////////////////////
cfdVolumeVisualization::cfdVolumeVisualization(const cfdVolumeVisualization& rhs)
{
   _utCbk = rhs._utCbk;
   _vSSCbk = rhs._vSSCbk;
   _volumeVizNode = rhs._volumeVizNode;
   _texGenParams = rhs._texGenParams;
   _bbox = rhs._bbox;
   _mode = rhs._mode;
   _traverseDirection = rhs._traverseDirection;
   _stateSet = rhs._stateSet;
   _material = rhs._material;
   _texture = rhs._texture;
   _slices = rhs._slices;
   _nSlices = rhs._nSlices;
   _alpha = rhs._alpha;
   _tUnit = rhs._tUnit;
   _verbose = rhs._verbose;
   _tm = rhs._tm;
   _image = rhs._image;
   _isCreated = rhs._isCreated;

   
   _noShaderGroup =  rhs._noShaderGroup;
   
   _shaderDirectory = new char[strlen(rhs._shaderDirectory)+1];
   strcpy(_shaderDirectory,rhs._shaderDirectory);

}
//////////////////////////////////////////////////
cfdVolumeVisualization::~cfdVolumeVisualization()
{
   //not sure if I should call release here or not
   if(_tm){
      delete [] _tm;
      _tm = 0;
   }
   if(_utCbk){
      delete _utCbk;
      _utCbk = 0;
   }
   if(_shaderDirectory){
      delete [] _shaderDirectory;
      _shaderDirectory = 0;
   }
   

}
////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetState(osg::State* state)
{
   _state = state;
}
//////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetShaderDirectory(char* shadDir)
{
   if(_shaderDirectory){
      delete [] _shaderDirectory;
      _shaderDirectory = 0;
   }
   _shaderDirectory = new char[strlen(shadDir)+1];
   strcpy(_shaderDirectory,shadDir);
}

#ifdef CFD_USE_SHADERS0

///////////////////////////////////////////////////
void cfdVolumeVisualization::EnableTransferShader()
{
   _useShaders = true;
   if(_tSM->GetShaderStateSet()&&(!_transferShaderIsActive)){
      _transferShaderIsActive = true;
      _volShaderIsActive = false;
   }
   _shaderSwitch->setSingleChildOn(2);
}
////////////////////////////////////////////////////
void cfdVolumeVisualization::DisableTransferShader()
{
   if(_transferShaderIsActive){
      _transferShaderIsActive = false;
      UseNormalGraphicsPipeline();
   }
}

////////////////////////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::UpdateTransferFunction(cfdUpdateableOSGTexture1d::TransType type,
                                              float param,int whichFunction)
{
   if(_tSM){
      _tSM->UpdateTransferFunction(type,param,whichFunction);
   }
   
}
#endif

//////////////////////////////////////////////////////
void cfdVolumeVisualization::SetPlayMode(VisMode mode)
{
   _mode = mode;
   if(_tm){
      switch(_mode){
         case PLAY:       
            _tm->setPlayMode(cfdTextureManager::PLAY);
            break;
         case STOP:
         default:
            _tm->setPlayMode(cfdTextureManager::STOP);
            break;
      };
   }else{
      std::cout<<"Warning!!!!"<<std::endl;
      std::cout<<"Invalid cfdTextureManager!"<<std::endl;
      std::cout<<"Cannot set viz mode of texture manager in:"<<std::endl;
      std::cout<<"cfdVolumeVisualization::SetPlayMode()"<<std::endl;
   }
}
////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetPlayDirection(Direction dir)
{
   _traverseDirection = dir;
   if(_tm){
      switch(_traverseDirection){
         case FORWARD:       
            _tm->setDirection(1);
            break;
         case BACKWARD:
         default:
            _tm->setDirection(-1);
            break;
      };
   }else{
      std::cout<<"Warning!!!!"<<std::endl;
      std::cout<<"Invalid cfdTextureManager!"<<std::endl;
      std::cout<<"Cannot set direction of texture manager in:"<<std::endl;
      std::cout<<"cfdVolumeVisualization::SetPlayDirection()"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::Set3DTextureData(osg::Texture3D* texture)
{
   //not sure if this is needed 
   if(!_texture.valid()){
      _texture = new osg::Texture3D(*texture);
      _texture->setDataVariance(osg::Object::DYNAMIC);
      
   }else{
      if(_texture != texture){
         _texture = texture; 
      }
   }
   if(_stateSet.valid()){
      _attachTextureToStateSet(_stateSet.get());
   }
}
////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetBoundingBox(float* bbox)
{
   float minBBox[3];
   float maxBBox[3];
   //this is because vtk gives mnx,mxx,mny,mxy,mnz,mxz
   minBBox[0] = bbox[0]; 
   minBBox[1] = bbox[2]; 
   minBBox[2] = bbox[4]; 
   maxBBox[0] = bbox[1]; 
   maxBBox[1] = bbox[3]; 
   maxBBox[2] = bbox[5]; 
   _bbox.set(osg::Vec3(minBBox[0],minBBox[1],minBBox[2]), 
                osg::Vec3(maxBBox[0],maxBBox[1],maxBBox[2]));

}
//////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;

   if(!_image.valid()){
      _image = new osg::Image();
   

     _image->allocateImage(_tm->fieldResolution()[0],
                     _tm->fieldResolution()[1],
                     _tm->fieldResolution()[2],
                     GL_RGBA,GL_UNSIGNED_BYTE);

      _image->setImage(_tm->fieldResolution()[0],_tm->fieldResolution()[1],
                     _tm->fieldResolution()[2],GL_RGBA,GL_RGBA, 
                     GL_UNSIGNED_BYTE,
                     _tm->dataField(0),
                     osg::Image::USE_NEW_DELETE,1);
      _image->setDataVariance(osg::Object::DYNAMIC);
   }
   if(!_texture.valid()){
      _texture = new osg::Texture3D();
      _texture->setDataVariance(osg::Object::DYNAMIC);
   

      _texture->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _texture->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _texture->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
      _texture->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
      _texture->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
      _texture->setInternalFormat(GL_RGBA);
      _texture->setImage(_image.get());
   }
   //}
   SetBoundingBox(_tm->getBoundingBox());
   if(_utCbk){
      _utCbk->SetTextureManager(_tm);
   }
}
///////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetNumberofSlices(int nSlices)
{
   _nSlices = nSlices;
}
///////////////////////////////////////////////////////
void cfdVolumeVisualization::SetSliceAlpha(float alpha)
{
   _alpha = alpha;
}
/////////////////////////////////////////////////////////////////   
osg::ref_ptr<osg::StateSet> cfdVolumeVisualization::GetStateSet()
{
   if(_stateSet.valid()){
      return _stateSet;
   }else{
      if(_verbose)
         std::cout<<"Invalid state set in cfdVolumeVisualization::GetStateSet!"<<std::endl;
      return 0;
   }
}
///////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::Group> cfdVolumeVisualization::GetVolumeVisNode()
{
   if(!_volumeVizNode.valid()){
      _buildGraph();
   }
   return _volumeVizNode;
}
/////////////////////////////////////////////////////////////////////   
osg::ref_ptr<osg::Texture3D> cfdVolumeVisualization::GetTextureData()
{
   if(_texture.valid()){
      return _texture;
   }else{
      if(_verbose)
         std::cout<<"Invalid texture data in cfdVolumeVisualization::GetTextureData()!"<<std::endl;
      return 0;
   }
}
/////////////////////////////////////////////////////
void cfdVolumeVisualization::SetVeboseFlag(bool flag)
{
   _verbose = flag;
}
//////////////////////////////////////////////
void cfdVolumeVisualization::_createClipNode()
{
   if(!_clipNode.valid()){
      _clipNode = new osg::ClipNode();
      _clipNode->setDataVariance(osg::Object::DYNAMIC);
   }
}
//////////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::AddClipPlane(ClipPlane direction,double* position)
{
   //biv -- check the logic here if we add more than one plane in
   //the same direction  but w/ different equation. . .this method shouldn't
   //be used but it might mistakenly be called. .. 
   if(_clipNode.valid()){
      _clipNode->addClipPlane(new osg::ClipPlane(direction,
                                            position[0],
                                            position[1],
                                            position[2],
                                            position[3]));
   }else{
      std::cout<<"Error!!!"<<std::endl;
      std::cout<<"Invalid osg::ClipNode in cfdVolumeVisualization::AddClipPlane!!"<<std::endl;
   }
}
/////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::RemoveClipPlane(ClipPlane direction)
{
   if ( _clipNode.valid() )
   {
      osg::ref_ptr< osg::ClipPlane > plane = 0;
      unsigned int planeIndex = 0;
      if ( _clipNode->getNumClipPlanes() )
      {
         for(unsigned int i = 0; i< (unsigned int)_clipNode->getNumClipPlanes();i++)
         {
            plane = _clipNode->getClipPlane(i);
            if ( plane->getClipPlaneNum() == (unsigned int)direction )
            {
               planeIndex = i;
               break;
            }
            else
            {
               plane = 0;
            }
         }
         
         if ( plane.valid() )
         {
            _clipNode->removeClipPlane(planeIndex);
         }
         else
         {
            std::cout<<"Plane not found!"<<std::endl;
            std::cout<<"cfdVolumeVisualization::RemoveClipPlanePosition."<<std::endl;
         }
      }
      else
      {
         std::cout<<"No planes on clip node!"<<std::endl;
         std::cout<<"cfdVolumeVisualization::RemoveClipPlanePosition."<<std::endl;
      }
      plane = 0;
   }  
}
/////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::UpdateClipPlanePosition(ClipPlane direction,
                                               double* newPosition)
{
   osg::ref_ptr<osg::ClipPlane> plane =0;
   if ( _clipNode.valid() )
   {
      if ( _clipNode->getNumClipPlanes() )
      {
         for(unsigned int i = 0; i < (unsigned int)_clipNode->getNumClipPlanes();i++)
         {
            plane = _clipNode->getClipPlane(i);
            if ( plane->getClipPlaneNum() == (unsigned int)direction )
            {
               break;
            }
            else
            {
               plane = 0;
            }
         }
         //plane = _clipNode->getClipPlane(direction);
         if ( plane.valid() )
         {
            if ( newPosition )
            {
               plane->setClipPlane(newPosition[0],newPosition[1],newPosition[2],newPosition[3]);
            }
            else
            {
               std::cout<<"Invalid plane position!"<<std::endl;
               std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
            }
         }
         else
         {
            
            std::cout<<"Invalid plane!"<<std::endl;
            std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
            std::cout<<"Adding new clip plane via::AddClipPlane."<<std::endl;
            AddClipPlane(direction,newPosition);
         }
      }
      else
      {
         std::cout<<"Invalid plane!"<<std::endl;
         std::cout<<"cfdVolumeVisualization::UpdateClipPlanePosition."<<std::endl;
         std::cout<<"Adding new clip plane via::AddClipPlane."<<std::endl;
         AddClipPlane(direction,newPosition);
      
      }
   }
   plane = 0;
}


////////////////////////////////////////////////
void cfdVolumeVisualization::_createStateSet()
{
   if(_texGenParams.valid()){
      if(!_stateSet.valid()){
         _stateSet = _texGenParams->getOrCreateStateSet();;
         _stateSet->setMode(GL_LIGHTING,osg::StateAttribute::OFF);
         _stateSet->setMode(GL_BLEND,osg::StateAttribute::ON);

         osg::ref_ptr<osg::BlendFunc> bf = new osg::BlendFunc;
			 bf->setFunction(osg::BlendFunc::SRC_ALPHA, osg::BlendFunc::ONE_MINUS_SRC_ALPHA);
			 _stateSet->setAttributeAndModes(bf.get());
			 _stateSet->setRenderingHint(osg::StateSet::TRANSPARENT_BIN);
         _attachTextureToStateSet(_stateSet.get());
      }else{
         //state set is already created/set by user
         if(_verbose){
            std::cout<<"Using user defined state set!!!!"<<std::endl;
         }
      }
   }else{
      if(_verbose)
         std::cout<<"Invalid TexGenNode in cfdVolumeVisualization::_createStateSet!"<<std::endl;
   } 
}
////////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::_attachTextureToStateSet(osg::StateSet* ss)
{
   if(ss){
      if(_texture.valid()){
         if(!_utCbk){
            _utCbk =  new cfdUpdateTextureCallback();
         
            _utCbk->SetTextureManager(_tm);
            _utCbk->SetDelayTime(1.0);
         
            int* res = _tm->fieldResolution();
            _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);
            _texture->setSubloadCallback(_utCbk);
         }
         ss->setTextureAttributeAndModes(PLAIN,_texture.get(),
			                        osg::StateAttribute::ON);
         ss->setTextureMode(PLAIN,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
         ss->setTextureMode(PLAIN,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
         ss->setTextureMode(PLAIN,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
         ss->setTextureAttributeAndModes(PLAIN,new osg::TexEnv(osg::TexEnv::REPLACE),
		                             osg::StateAttribute::OVERRIDE | osg::StateAttribute::ON);
      }
   }else{
      if(_verbose)
         std::cout<<"Invalid state set in cfdVolumeVisualization::GetStateSet!"<<std::endl;
   }
}
////////////////////////////////////////////////
//may need to modify this to use the bbox     //
////////////////////////////////////////////////
void cfdVolumeVisualization::_createTexGenNode()
{
   if(_bbox.valid()){
      osg::Vec4 sPlane(0,0,0,0);
      osg::Vec4 tPlane(0,0,0,0);
      osg::Vec4 rPlane(0,0,0,0);

      sPlane[0] = 1.0/(_bbox.xMax() - _bbox.xMin());
      tPlane[1] = 1.0/(_bbox.yMax() - _bbox.yMin());
      rPlane[2] = 1.0/(_bbox.zMax()- _bbox.zMin());
   
      sPlane[3] = - _bbox.xMin()/(_bbox.xMax() - _bbox.xMin());
      tPlane[3] = - _bbox.yMin()/(_bbox.yMax() - _bbox.yMin());
      rPlane[3] = - _bbox.zMin()/(_bbox.zMax()- _bbox.zMin());
      //biv--this may not be right!!!      
      _texGenParams = new osg::TexGenNode();
      _texGenParams->setTextureUnit(_tUnit);
      _texGenParams->getTexGen()->setMode(osg::TexGen::OBJECT_LINEAR);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::S,sPlane); 
      _texGenParams->getTexGen()->setPlane(osg::TexGen::T,tPlane);
      _texGenParams->getTexGen()->setPlane(osg::TexGen::R,rPlane);

   }else{
      if(_verbose)
         std::cout<<"Invalid bbox in cfdVolumeVisualization::_createTexGenNode!"<<std::endl;
   }
}
//////////////////////////////////////////////////////////
void cfdVolumeVisualization::_buildAxisDependentGeometry()
{
   osg::Vec3Array* xcoords = new osg::Vec3Array(4*_nSlices);
   osg::Vec3Array* ycoords = new osg::Vec3Array(4*_nSlices);
   osg::Vec3Array* zcoords = new osg::Vec3Array(4*_nSlices);

   osg::Vec3Array* xncoords = new osg::Vec3Array(4*_nSlices);
   osg::Vec3Array* yncoords = new osg::Vec3Array(4*_nSlices);
   osg::Vec3Array* zncoords = new osg::Vec3Array(4*_nSlices);

   float y = _bbox.yMin();
   float dy = (_bbox.yMax() -_bbox.yMin())/(_nSlices - 1.0);
   for(int i=0;i<_nSlices;++i, y+=dy){
      (*ycoords)[i*4+0].set(_bbox.xMin(),y,_bbox.zMin());
      (*ycoords)[i*4+1].set(_bbox.xMax(),y,_bbox.zMin());
      (*ycoords)[i*4+2].set(_bbox.xMax(),y,_bbox.zMax());
      (*ycoords)[i*4+3].set(_bbox.xMin(),y,_bbox.zMax());
   }
   y = _bbox.yMax();
   for(int i=0;i<_nSlices;++i, y-=dy){
      (*yncoords)[i*4+0].set(_bbox.xMin(),y,_bbox.zMin());
      (*yncoords)[i*4+1].set(_bbox.xMax(),y,_bbox.zMin());
      (*yncoords)[i*4+2].set(_bbox.xMax(),y,_bbox.zMax());
      (*yncoords)[i*4+3].set(_bbox.xMin(),y,_bbox.zMax());
   }

   float x = _bbox.xMin();
   float dx = (_bbox.xMax() -_bbox.xMin())/(_nSlices - 1.0);
   for(int i=0;i<_nSlices;++i, x+=dx){
        (*xcoords)[i*4+0].set(x,_bbox.yMin(),_bbox.zMin());
        (*xcoords)[i*4+1].set(x,_bbox.yMax(),_bbox.zMin());
        (*xcoords)[i*4+2].set(x,_bbox.yMax(),_bbox.zMax());
        (*xcoords)[i*4+3].set(x,_bbox.yMin(),_bbox.zMax());
   }
   x = _bbox.xMax();
   for(int i=0;i<_nSlices;++i, x-=dx){
        (*xncoords)[i*4+0].set(x,_bbox.yMin(),_bbox.zMin());
        (*xncoords)[i*4+1].set(x,_bbox.yMax(),_bbox.zMin());
        (*xncoords)[i*4+2].set(x,_bbox.yMax(),_bbox.zMax());
        (*xncoords)[i*4+3].set(x,_bbox.yMin(),_bbox.zMax());
   }
   float z = _bbox.zMin();
   float dz = (_bbox.zMax() -_bbox.zMin())/(_nSlices - 1.0);
   for(int i=0;i<_nSlices;++i, z+=dz){
      (*zcoords)[i*4+0].set(_bbox.xMax(),_bbox.yMin(),z);
      (*zcoords)[i*4+1].set(_bbox.xMax(),_bbox.yMax(),z);
      (*zcoords)[i*4+2].set(_bbox.xMin(),_bbox.yMax(),z);
      (*zcoords)[i*4+3].set(_bbox.xMin(),_bbox.yMin(),z);
   }
   z = _bbox.zMax();
   for(int i=0;i<_nSlices;++i, z-=dz){
      (*zncoords)[i*4+0].set(_bbox.xMax(),_bbox.yMin(),z);
      (*zncoords)[i*4+1].set(_bbox.xMax(),_bbox.yMax(),z);
      (*zncoords)[i*4+2].set(_bbox.xMin(),_bbox.yMax(),z);
      (*zncoords)[i*4+3].set(_bbox.xMin(),_bbox.yMin(),z);
   }
   osg::Vec4Array* colors = new osg::Vec4Array(1);
   (*colors)[0].set(1.0f,1.0f,1.0f,_alpha);

   _posXSlices = new osg::Geometry();
   _posXSlices->setVertexArray(xcoords);
   _posXSlices->setColorArray(colors);
   _posXSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _posXSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,xcoords->size()));
   
   _negXSlices= new osg::Geometry();
   _negXSlices->setVertexArray(xncoords);
   _negXSlices->setColorArray(colors);
   _negXSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _negXSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,xncoords->size()));

   _posYSlices = new osg::Geometry();
   _posYSlices->setVertexArray(ycoords);
   _posYSlices->setColorArray(colors);
   _posYSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _posYSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,ycoords->size()));
   
   _negYSlices = new osg::Geometry();
   _negYSlices->setVertexArray(yncoords);
   _negYSlices->setColorArray(colors);
   _negYSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _negYSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,yncoords->size()));

   _posZSlices = new osg::Geometry();
   _posZSlices->setVertexArray(zcoords);
   _posZSlices->setColorArray(colors);
   _posZSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _posZSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,zcoords->size()));
   
   _negZSlices= new osg::Geometry();
   _negZSlices->setVertexArray(zncoords);
   _negZSlices->setColorArray(colors);
   _negZSlices->setColorBinding(osg::Geometry::BIND_OVERALL);
   _negZSlices->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::QUADS,0,zncoords->size()));

}
///////////////////////////////////////////////////////////
void cfdVolumeVisualization::_createVolumeSlices()
{
   _buildAxisDependentGeometry();
   _slices = new osg::Geode();
   //this will change w/ callback so default the z slices
   _slices->addDrawable(_posZSlices.get());
   
}
/////////////////////////////////////////
void cfdVolumeVisualization::CreateNode()
{
   if(!_isCreated){
      _buildGraph();
   }
}
//////////////////////////////////////////
void cfdVolumeVisualization::_buildGraph()
{
if(!_tm){
      std::cout<<"Texture Manager not set!!!"<<std::endl;
      return;
   }
   _volumeVizNode = new osg::Group();
   _volumeVizNode->setName("Volume Viz Node");
   _volumeVizNode->setDataVariance(osg::Object::DYNAMIC);
   
   _createTexGenNode();
   _createStateSet();
   _createVolumeSlices();
   _createClipNode();
   _isCreated = true;

   if(!_noShaderGroup.valid()){
      _noShaderGroup = new osg::Group();
      _noShaderGroup->setName("VViz Node:R");
      _volumeVizNode->addChild(_noShaderGroup.get());
   }
   
   if(_stateSet.valid()){
      _noShaderGroup->setStateSet(_stateSet.get());
   }


   if(_texGenParams.valid()){
      _noShaderGroup->addChild(_texGenParams.get());

      if(_slices.valid()){
         _vSSCbk =  new cfdVolumeSliceSwitchCallback(_bbox.center());
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::X_POS,_posXSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Y_POS,_posYSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Z_POS,_posZSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::X_NEG,_negXSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Y_NEG,_negYSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Z_NEG,_negZSlices);
         
         _slices->setCullCallback(_vSSCbk);
         if(_clipNode.valid()){
            _texGenParams->addChild(_clipNode.get());
            _clipNode->addChild(_slices.get());
         }else{
            _texGenParams->addChild(_slices.get());
         }
      }else{
         _isCreated = false;
      }
   }else{
      _isCreated = false;
   }
}
////////////////////////////////////////////////////////////////////
cfdVolumeVisualization&
cfdVolumeVisualization::operator=(const cfdVolumeVisualization& rhs)
{
   if(&rhs != this){
      _vSSCbk = rhs._vSSCbk;
      _utCbk = rhs._utCbk;
      _volumeVizNode = rhs._volumeVizNode;
      _texGenParams = rhs._texGenParams;
     

      _stateSet = rhs._stateSet;
      _material = rhs._material;
      _texture = rhs._texture;
      _slices = rhs._slices;
      _nSlices = rhs._nSlices;
      _alpha = rhs._alpha;
      _tUnit = rhs._tUnit;
      _tm = rhs._tm;
      

      _mode = rhs._mode;
      _traverseDirection = rhs._traverseDirection;
      _stateSet = rhs._stateSet;
   
      _verbose = rhs._verbose;
   
      _image = rhs._image;
      _isCreated = rhs._isCreated;
      if(_shaderDirectory){
         delete [] _shaderDirectory;
         _shaderDirectory = 0;
      }
      _shaderDirectory = new char[strlen(rhs._shaderDirectory)+1];
      strcpy(_shaderDirectory,rhs._shaderDirectory);

      
      _noShaderGroup =  rhs._noShaderGroup;

   }
   return *this;
}
#endif

