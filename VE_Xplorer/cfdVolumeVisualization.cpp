#include "cfdVolumeVisualization.h"
#ifdef _PERFORMER
#elif _OPENSG
#elif _OSG
#include <iostream>
#include <osg/BlendFunc>
////////////////////////////////////////////////
//Constructor                                 //
////////////////////////////////////////////////
cfdVolumeVisualization::cfdVolumeVisualization()
{
   _volumeVizNode  = 0;
   _texGenParams  = 0;
   _vSSCbk = 0;
   _utCbk = 0;
   _mode = PLAY;
   _stateSet  = 0;
   _material  = 0;
   _texture  = 0;
   _slices  = 0;
   _nSlices = 100;
   _alpha = 0.5;
   _tUnit = 0;
   _verbose = 0;
   _tm = 0;
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
}
//////////////////////////////////////////////////
cfdVolumeVisualization::~cfdVolumeVisualization()
{
   //not sure if I should call unref here or not
   if(_tm){
      delete [] _tm;
      _tm = 0;
   }
   if(_utCbk){
      delete _utCbk;
      _utCbk = 0;
   }
}
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
//////////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::Set3DTextureData(osg::Texture3D* texture)
{
   //not sure if this is needed 
   /*if(!_texture.valid()){
      _texture = new osg::Texture3D(*texture);
      _texture->setDataVariance(osg::Object::DYNAMIC);
      
   }else{
      _texture = texture; 
   }*/

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
//////////////////////////////////////////////////////////////////
void cfdVolumeVisualization::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;

   if(!_image.valid()){
      _image = new osg::Image();
   }

   _image->allocateImage(_tm->fieldResolution()[0],
                     _tm->fieldResolution()[1],
                     _tm->fieldResolution()[2],
                     GL_RGBA,GL_UNSIGNED_BYTE);

   _image->setImage(_tm->fieldResolution()[0],                  _tm->fieldResolution()[1],                  _tm->fieldResolution()[2],GL_RGBA,                  GL_RGBA, GL_UNSIGNED_BYTE,                 _tm->dataField(0),                 osg::Image::USE_NEW_DELETE,1);
   _image->setDataVariance(osg::Object::DYNAMIC);
   
   if(!_texture.valid()){
      _texture = new osg::Texture3D();
      _texture->setDataVariance(osg::Object::DYNAMIC);
   }

   _texture->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
   _texture->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
   _texture->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
   _texture->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
   _texture->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
   _texture->setInternalFormat(GL_RGBA);
   _texture->setImage(_image.get());

   SetBoundingBox(_tm->getBoundingBox());

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
////////////////////////////////////////////////////////////////////
cfdVolumeVisualization&
cfdVolumeVisualization::operator=(const cfdVolumeVisualization& rhs)
{
   if(&rhs != this){
      _vSSCbk = rhs._vSSCbk;
      _utCbk = rhs._utCbk;
      _volumeVizNode = rhs._volumeVizNode;
      _texGenParams = rhs._texGenParams;
      _bbox = rhs._bbox;
     
      _stateSet = rhs._stateSet;
      _material = rhs._material;
      _texture = rhs._texture;
      _slices = rhs._slices;
      _nSlices = rhs._nSlices;
      _alpha = rhs._alpha;
      _tUnit = rhs._tUnit;
      _tm = rhs._tm;
   }
   return *this;
}
/////////////////////////////////////////////////////
void cfdVolumeVisualization::SetVeboseFlag(bool flag)
{
   _verbose = flag;
}
//////////////////////////////////////////////
void cfdVolumeVisualization::_createClipNode()
{
   if(_bbox.valid()){
      _visualBoundingBox = new osg::Group();
      
      osg::Geometry* bboxCube = new osg::Geometry;

      osg::Vec3Array* coords = new osg::Vec3Array();
      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(7));

      coords->push_back(_bbox.corner(0));
      coords->push_back(_bbox.corner(4));
      coords->push_back(_bbox.corner(1));
      coords->push_back(_bbox.corner(5));
      coords->push_back(_bbox.corner(2));
      coords->push_back(_bbox.corner(6));
      coords->push_back(_bbox.corner(3));
      coords->push_back(_bbox.corner(7));



      bboxCube->setVertexArray(coords);

      osg::Vec4Array* colors = new osg::Vec4Array(1);
      (*colors)[0].set(1.0f,1.0f,0.0f,1.0f);
      bboxCube->setColorArray(colors);
      bboxCube->setColorBinding(osg::Geometry::BIND_OVERALL);
      bboxCube->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES,
			       0,coords->size()));
      
      osg::Geode* geode = new osg::Geode;
      geode->addDrawable(bboxCube);
      _visualBoundingBox->addChild(geode);
      
   }else{
      if(_verbose)
         std::cout<<"Invalid bbox in cfdVolumeVisualization::_createClipNode!"<<std::endl;
   }
}
//////////////////////////////////////////////////////////////
void cfdVolumeVisualization::UpdateStateSet(osg::StateSet* ss)
{
   if(!_stateSet.valid()){
      _stateSet = new osg::StateSet();
   }
   _stateSet = ss;
   if(_texGenParams.valid()){
      _texGenParams->setStateSet(_stateSet.get());
   }

}
//////////////////////////////////////////////////////
void cfdVolumeVisualization::SetTextureUnit(int tUnit)
{
   _tUnit = tUnit;
   if(_texGenParams.valid()){
      _texGenParams->setTextureUnit(_tUnit);
   }
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
         _attachTextureToStateSet();
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
///////////////////////////////////////////////////////
void cfdVolumeVisualization::_attachTextureToStateSet()
{
   if(_stateSet.valid()){
      if(_texture.valid()){
         if(!_utCbk){
            _utCbk =  new cfdUpdateTextureCallback();
         }
         _utCbk->SetTextureManager(_tm);
         _utCbk->SetDelayTime(1.0);
         
         int* res = _tm->fieldResolution();
         _utCbk->setSubloadTextureSize(res[0],res[1],res[2]);
         _texture->setSubloadCallback(_utCbk);
         _stateSet->setTextureAttributeAndModes(_tUnit,_texture.get(),
			                        osg::StateAttribute::ON);
         _stateSet->setTextureMode(_tUnit,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
         _stateSet->setTextureMode(_tUnit,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
         _stateSet->setTextureMode(_tUnit,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);
         _stateSet->setTextureAttributeAndModes(_tUnit,new osg::TexEnv(osg::TexEnv::REPLACE),
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
void cfdVolumeVisualization::_createVolumeBillboardSlices()
{
   _buildAxisDependentGeometry();
   _slices = new osg::Geode();
   //this will change w/ callback so default the z slices
   _slices->addDrawable(_posZSlices.get());
   
}
/////////////////////////////////////////
void cfdVolumeVisualization::CreateNode()
{
   _buildGraph();
}
//////////////////////////////////////////
void cfdVolumeVisualization::_buildGraph()
{
   _volumeVizNode = new osg::Group();
   _createTexGenNode();
   _createStateSet();
   _createVolumeBillboardSlices();
   _createClipNode();

   if(_texGenParams.valid()){
      if(_stateSet.valid()){
         _texGenParams->setStateSet(_stateSet.get());
      }
      _volumeVizNode->addChild(_texGenParams.get());
      if(_slices.valid()){
         _vSSCbk =  new cfdVolumeSliceSwitchCallback(_bbox.center());
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::X_POS,_posXSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Y_POS,_posYSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Z_POS,_posZSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::X_NEG,_negXSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Y_NEG,_negYSlices);
         _vSSCbk->AddGeometrySlices(cfdVolumeSliceSwitchCallback::Z_NEG,_negZSlices);
         
         _slices->setCullCallback(_vSSCbk);
         _texGenParams->addChild(_slices.get());
      }
      if(_visualBoundingBox.valid()){
         _volumeVizNode->addChild(_visualBoundingBox.get());
        
         
         //_volumeVizNode->setUpdateCallback(_utCbk);
      } 
   }
}
#endif

