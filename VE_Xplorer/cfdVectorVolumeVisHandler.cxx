#ifdef _OSG
#include <osg/TexEnv>
#include <osg/Geode>
#include <osg/TexMat>
#include <osg/StateSet>
#include <osg/Switch>
#ifdef CFD_USE_SHADERS
#include "cfdAdvectionSubGraph.h"
#include "cfdVectorVolumeVisHandler.h"

#include "cfdOSGAdvectionShaderManager.h"
#include "cfdOSGTransferShaderManager.h"
#include "cfdPBufferManager.h"
#include "cfdTextureManager.h"

#include "cfdAdvectPropertyCallback.h"
#include "cfdUpdateTextureCallback.h"
#include "cfd3DTextureCullCallback.h"

#include "cfdOSGPingPongTexture3d.h"
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdVectorVolumeVisHandler::cfdVectorVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
   _aSM = 0;
   _velocityCbk = 0;
   _pbuffer = 0;
   _cullCallback = 0;
   _texturePingPong = 0;
   _transferSM = 0;
}
//////////////////////////////////////////////////////////
cfdVectorVolumeVisHandler::cfdVectorVolumeVisHandler(const cfdVectorVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{

   _aSM = new cfdOSGAdvectionShaderManager(*vvnh._aSM);
   _transferSM = new cfdOSGTransferShaderManager(*vvnh._transferSM);
   _velocityCbk = new cfdUpdateTextureCallback(*vvnh._velocityCbk);
   _pbuffer = vvnh._pbuffer;
   _cullCallback = new cfd3DTextureCullCallback(*vvnh._cullCallback);
   _texturePingPong = new cfdOSGPingPongTexture3D(*vvnh._texturePingPong);
   _advectionSlice = new osg::Group(*vvnh._advectionSlice);
   _property = new osg::Texture3D(*vvnh._property);
   _velocity = new osg::Texture3D(*vvnh._velocity);
}
///////////////////////////////////////////////////////
cfdVectorVolumeVisHandler::~cfdVectorVolumeVisHandler()
{
   if(_tm){
      delete _tm;
      _tm = 0;
   }
   if(_aSM){
      delete _aSM;
      _aSM = 0;
   }
   if(_transferSM){
      delete _transferSM;
      _transferSM = 0;
   }
   if(_velocityCbk){
      delete _velocityCbk;
      _velocityCbk = 0;
   }
   if(_cullCallback){
      delete _cullCallback;
      _cullCallback = 0;
   }
   if(_texturePingPong){
      delete _texturePingPong;
      _texturePingPong = 0;
   }
}
//////////////////////////////////////
void cfdVectorVolumeVisHandler::Init()
{
   cfdVolumeVisNodeHandler::Init();
   //be able to turn the bounding box off/on
   SetBoundingBoxName("Vector VVH BBox");
   SetDecoratorName("Advection VV Fragment PG");
}
///////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_createTransferShader()
{
   if(!_transferSM && _tm){
      int* fieldSize = _tm->fieldResolution();
      _transferSM = new cfdOSGTransferShaderManager();
      _transferSM->SetFieldSize(fieldSize[0],fieldSize[1],fieldSize[2]);
      
      _transferSM->Init();
      if(_decoratorGroup.valid()){
         //_transferSM->GetShaderStateSet()->setTextureAttributeAndModes(0,_texGen.get(),
          //  osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
         _decoratorGroup->setStateSet(_transferSM->GetShaderStateSet());
      }
   }
}
/////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_initPropertyTexture()
{
   
}
///////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_createVelocityFromTextureManager()
{
   if(!_velocity.valid()){
     _velocity = new osg::Texture3D;
     osg::ref_ptr<osg::Image> image = new osg::Image();

     image->allocateImage(_tm->fieldResolution()[0],
                          _tm->fieldResolution()[1],
                          _tm->fieldResolution()[2],
                           GL_RGBA,GL_UNSIGNED_BYTE);

     image->setImage(_tm->fieldResolution()[0],_tm->fieldResolution()[1],
                      _tm->fieldResolution()[2],GL_RGBA,GL_RGBA, 
                       GL_UNSIGNED_BYTE,
                       _tm->dataField(0),
                       osg::Image::USE_NEW_DELETE,1);
     image->setDataVariance(osg::Object::DYNAMIC);

     _velocity->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
     _velocity->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
     _velocity->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP_TO_EDGE);
     _velocity->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP_TO_EDGE);
     _velocity->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP_TO_EDGE);
     _velocity->setTextureSize(_tm->fieldResolution()[0],
                            _tm->fieldResolution()[1],
                            _tm->fieldResolution()[2]);
     _velocity->setInternalFormat(GL_RGBA);
     _velocity->setImage(image.get());
   } 
   if(!_velocityCbk){
      _velocityCbk =  new cfdUpdateTextureCallback();
      int* res = _tm->fieldResolution();
      _velocityCbk->setSubloadTextureSize(res[0],res[1],res[2]);
      _velocity->setSubloadCallback(_velocityCbk);
   }
   _velocityCbk->SetTextureManager(_tm);
   _velocityCbk->SetDelayTime(0.1);
}
////////////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::SetTextureManager(cfdTextureManager* tm)
{
   cfdVolumeVisNodeHandler::SetTextureManager(tm);
   _createVelocityFromTextureManager();
}
/////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_setUpDecorator()
{
   if(!_tm){
      return;
   }
   int* res = _tm->fieldResolution();
   if(!_aSM){
      _aSM = new cfdOSGAdvectionShaderManager();
      _aSM->SetFieldSize(res[0],res[1],res[2]);
      _aSM->SetVelocityTexture(_velocity.get());
   }
   _aSM->Init();  
   _createTransferShader();
   _createTexturePingPong();

   float deltaZ = (_bbox.zMax()-_bbox.zMin())/(float)(_tm->fieldResolution()[2]-1);
   //create the advection subgraph
   /*if(!_advectionSlice.valid()){
      _advectionSlice = CreateAdvectionSubGraph(_tm,_aSM->GetPropertyTexture(),
                                           _pbuffer,
                                           _aSM->GetShaderStateSet(),
                                           deltaZ).get();   
   }
   if(!_cullCallback && _pbuffer ){
      _cullCallback = new cfd3DTextureCullCallback(_advectionSlice.get(),
                                              _tm->fieldResolution()[0],
                                              _tm->fieldResolution()[1]);
      _decoratorGroup->setCullCallback(_cullCallback);
   }*/
   
  
} 
/////////////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::SetPBufferManager(cfdPBufferManager* pbm)
{
   _pbuffer = pbm;
}
////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_createTexturePingPong()
{
   if(!_texturePingPong){
      _texturePingPong = new cfdOSGPingPongTexture3D();
   }
   if(_aSM && _transferSM){
      _texturePingPong->SetPingTexture(_aSM->GetPropertyTexture());
      _texturePingPong->SetPongTexture(_transferSM->GetPropertyTexture());
   }
}
//////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::PingPongTextures()
{
   if(_texturePingPong){
      _texturePingPong->PingPongTextures();
   }
}
//////////////////////////////////////////////////////////////////////////
cfdVectorVolumeVisHandler&
cfdVectorVolumeVisHandler::operator=(const cfdVectorVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);
      _aSM = vvnh._aSM;
      _texturePingPong = vvnh._texturePingPong;
      _velocityCbk = vvnh._velocityCbk;
      _pbuffer = vvnh._pbuffer;
      _cullCallback = vvnh._cullCallback;
      _texturePingPong = vvnh._texturePingPong;
      _advectionSlice = vvnh._advectionSlice;
      _property = vvnh._property;
      _velocity = vvnh._velocity;
      _transferSM =  vvnh._transferSM;
   }
   return *this;
}
#endif //CFD_USE_SHADERS
#endif //_OSG

