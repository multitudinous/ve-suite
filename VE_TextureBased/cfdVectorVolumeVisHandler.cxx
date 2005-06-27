#ifdef VE_PATENTED
#ifdef _OSG
#include <osg/TexEnv>
#include <osg/Geode>
#include <osg/TexMat>
#include <osg/StateSet>
#include <osg/Switch>
#include <osg/Node>

#include "VE_TextureBased/cfdAdvectionSubGraph.h"
#include "VE_TextureBased/cfdVectorVolumeVisHandler.h"
#include "VE_TextureBased/cfdOSGAdvectionShaderManager.h"
#include "VE_TextureBased/cfdOSGTransferShaderManager.h"
#include "VE_TextureBased/cfdPBufferManager.h"
#include "VE_TextureBased/cfdTextureManager.h"
#include "VE_TextureBased/cfdTextureMatrixCallback.h"
#include "VE_TextureBased/cfdUpdateTextureCallback.h"
#include "VE_TextureBased/cfd3DTextureCullCallback.h"
#include "VE_TextureBased/cfdOSGPingPongTexture3d.h"

using namespace VE_TextureBased;
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
   _autoTexGen = false;
   _ssIsSet = false;

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
   _propertyTextureGroup = new osg::Group(*vvnh._propertyTextureGroup);
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
      _velocityCbk->SetIsLuminance(false);
      int* res = _tm->fieldResolution();
      _velocityCbk->setSubloadTextureSize(res[0],res[1],res[2]);
      _velocity->setSubloadCallback(_velocityCbk);
   }
   _velocityCbk->SetTextureManager(_tm);
   _velocityCbk->SetDelayTime(0.1);
}
////////////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::SetTextureManager(VE_TextureBased::cfdTextureManager* tm)
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
   
   if(!_propertyTextureGroup.valid())
   {
      _propertyTextureGroup = new osg::Group();
      _propertyTextureGroup->setName("Property Texture");
      _visualBoundingBox->removeChild(_decoratorGroup.get());
      _bboxSwitch->removeChild(_decoratorGroup.get());
      _visualBoundingBox->addChild(_propertyTextureGroup.get());
      _bboxSwitch->addChild(_propertyTextureGroup.get());
      _propertyTextureGroup->addChild(_decoratorGroup.get());
   }
   int* res = _tm->fieldResolution();
   
   if(!_aSM){
      _aSM = new cfdOSGAdvectionShaderManager();
     
   } 
   _aSM->SetFieldSize(res[0],res[1],res[2]);
   _aSM->SetVelocityTexture(_velocity.get());
   _aSM->SetCenter(_bbox.center());
   _aSM->Init();  
   _createTransferShader();

   float deltaZ = (_bbox.zMax()-_bbox.zMin())/(float)(_tm->fieldResolution()[2]-1);
   //create the advection subgraph
   if(!_advectionSlice.valid()){
      _advectionSlice = new osg::Group();
      _advectionSlice->setName("AdvectionSlice");

      osg::ref_ptr<osg::Group>shaderGroup = CreateAdvectionSubGraph(_tm,
                                           _pbuffer,
                                           deltaZ).get();   
      shaderGroup->setStateSet(_aSM->GetShaderStateSet());
      _advectionSlice->addChild(shaderGroup.get());
   }
   if(!_cullCallback && _pbuffer ){
      _cullCallback = new cfd3DTextureCullCallback(_advectionSlice.get(),
                                              _tm->fieldResolution()[0],
                                              _tm->fieldResolution()[1]);
      _cullCallback->SetPBuffer(_pbuffer);
      _propertyTextureGroup->setCullCallback(_cullCallback);
   }
   _setupAdvectionPropertyStateSet();
   _setupTransferPropertyStateSet();
   _createTexturePingPong();
} 
////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_setupTransferPropertyStateSet()
{
   if(!_ssIsSet){
      unsigned int tunit = _transferSM->GetAutoGenTextureUnit();
      osg::ref_ptr<osg::StateSet> ss = _propertyTextureGroup->getOrCreateStateSet();
      ss->setTextureAttributeAndModes(tunit,
                                  _transferSM->GetPropertyTexture(),
                                  osg::StateAttribute::ON);
      /*ss->setTextureMode(tunit,GL_TEXTURE_3D,osg::StateAttribute::OFF|osg::StateAttribute::OVERRIDE);
      ss->setTextureMode(tunit,GL_TEXTURE_GEN_S,osg::StateAttribute::ON);
      ss->setTextureMode(tunit,GL_TEXTURE_GEN_T,osg::StateAttribute::ON);
      ss->setTextureMode(tunit,GL_TEXTURE_GEN_R,osg::StateAttribute::ON);*/
      ss->setTextureAttributeAndModes(tunit,
                                  new osg::TexEnv(osg::TexEnv::REPLACE),
		                             osg::StateAttribute::OVERRIDE|osg::StateAttribute::ON);
      ss->setDataVariance(osg::Object::DYNAMIC);
      _ssIsSet = true;
   }
}
/////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_setupAdvectionPropertyStateSet()
{
   if(!_ssIsSet){
      unsigned int tunit = _aSM->GetAutoGenTextureUnit();
      osg::ref_ptr<osg::StateSet> ss = _advectionSlice->getOrCreateStateSet();
      ss->setTextureAttributeAndModes(tunit,
                                  _aSM->GetPropertyTexture(),
                                  osg::StateAttribute::ON);
                                 
      ss->setTextureMode(tunit,GL_TEXTURE_3D,osg::StateAttribute::OFF|osg::StateAttribute::OVERRIDE);
      /*ss->setTextureMode(tunit,GL_TEXTURE_GEN_S,
                       osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
      ss->setTextureMode(tunit,GL_TEXTURE_GEN_T,
                       osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
      ss->setTextureMode(tunit,GL_TEXTURE_GEN_R,
                       osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);*/
      ss->setTextureAttributeAndModes(tunit,
                                  new osg::TexEnv(osg::TexEnv::REPLACE),
		                             osg::StateAttribute::OVERRIDE|osg::StateAttribute::ON);
      ss->setDataVariance(osg::Object::DYNAMIC);
   }
}
/////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_applyTextureMatrix()
{
   unsigned int tUnit = 0;/*_transferSM->GetAutoGenTextureUnit();*/
   osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
   tMat->setMatrix(osg::Matrix::identity());
   _decoratorGroup->getStateSet()->setTextureAttributeAndModes(tUnit,
                                                        tMat.get(),
                                                        osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
   float trans[3] = {.5,.5,.5};
   _decoratorGroup->setUpdateCallback(new cfdTextureMatrixCallback(tMat.get(),
                                                             _center,
                                                             _scale,
                                                             trans));
   _updateTexGenUnit(tUnit);
}
/////////////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::SetPBufferManager(cfdPBufferManager* pbm)
{
   _pbuffer = pbm;
}
////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_createTexturePingPong()
{
   if(_cullCallback)
   {
      unsigned int current = _transferSM->GetAutoGenTextureUnit();
      unsigned int previous = _aSM->GetAutoGenTextureUnit();
      _cullCallback->SetPingPongTextures(previous,_advectionSlice.get(),
                                     current,_propertyTextureGroup.get());
   }
}
//////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::PingPongTextures()
{
   if(_cullCallback){
      _cullCallback->GetPingPonger()->PingPongTextures();
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
//#endif //CFD_USE_SHADERS
#endif //_OSG
#endif
