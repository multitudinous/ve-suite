#ifdef _OSG
#include <osg/TexEnv>
#ifdef CFD_USE_SHADERS
#include "cfdAdvectionSubGraph.h"
#include "cfdVectorVolumeVisHandler.h"
#include "cfdAdvectPropertyCallback.h"
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdVectorVolumeVisHandler::cfdVectorVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
   _aSM = 0;
   _tm = 0;
   _shaderSwitch = 0;
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
   _shaderSwitch = new cfdSwitch(*vvnh._shaderSwitch);
   _tm = new cfdTextureManager(*vvnh._tm);
   _transferSM = new cfdOSGTransferShaderManager(*vvnh._transferSM);

   _velocityCbk = new cfdUpdateTextureCallback(*vvnh._velocityCbk);
   _pbuffer = vvnh._pbuffer;
   _cullCallback = new cfd3DTextureCullCallback(*vvnh._cullCallback);
   _texturePingPong = new cfdOSGPingPongTexture3D(*vvnh._texturePingPong);
   _advectionFragGroup = new osg::Group(*vvnh._advectionFragGroup);
   _advectionSlice = new osg::Group(*vvnh._advectionSlice);
   _property = new osg::Texture3D(*vvnh._property);
   _velocity = new osg::Texture3D(*vvnh._velocity);
}
///////////////////////////////////////////////////////
cfdVectorVolumeVisHandler::~cfdVectorVolumeVisHandler()
{

   if(_aSM){
      delete _aSM;
      _aSM = 0;
   }

   if(_shaderSwitch){
     delete _shaderSwitch;
     _shaderSwitch = 0;
   }
   if(_tm){
      delete _tm;
      _tm = 0;
   }
}
//////////////////////////////////////
void cfdVectorVolumeVisHandler::Init()
{
   if(!_bbox.valid()){
      std::cout<<"Invalid bounding box!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_vvN.valid()){
      std::cout<<"Invalid volume viz node!!"<<std::endl;
      std::cout<<"cfdVolumeVizNodeHandler::Init!!"<<std::endl;
      return;
   }
   if(!_tm){
      std::cout<<"Invalid TextureManager!!"<<std::endl;
      std::cout<<"cfdScalarVolumeVisHandler::Init!!"<<std::endl;
      return;
   }
   if(!_topNode.valid()){
      _createVisualBBox();
      _topNode = new osg::Group();
      _topNode->setName("Scalar VolumeVisHandler");

      //be able to turn the bounding box off/on
      _bboxSwitch = new osg::Switch();
      _bboxSwitch->setName("BBox Switch");
      _bboxSwitch->addChild(_visualBoundingBox.get());
      _topNode->addChild(_bboxSwitch.get());
      _initPropertyTexture();
      _attachVolumeVisNodeToGraph();
      _createTexturePingPong();
      _createTransferShader();
   }
}
///////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_createTransferShader()
{
   if(!_transferSM &&_tm&&_property.valid()){
      int* fieldSize = _tm->fieldResolution();
      _transferSM = new cfdOSGTransferShaderManager();
      _transferSM->SetFieldSize(fieldSize[0],fieldSize[1],fieldSize[2]);
      _transferSM->SetPropertyTexture(_property.get());
      _transferSM->Init();
      if(_advectionFragGroup.valid()){
         _advectionFragGroup->setStateSet(_transferSM->GetShaderStateSet()); 
      }
   }
}
/////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_initPropertyTexture()
{
   if(_tm){
      int* fieldSize = _tm->fieldResolution();
      int dataSize = fieldSize[0]*fieldSize[1]*fieldSize[2];
      unsigned char* data = new unsigned char[dataSize*4];
   
      for(int p = 0; p < dataSize; p++){
      
         data[p*4   ] = (unsigned char)0;

         data[p*4 + 1] = (unsigned char)0;
         data[p*4 + 2] = (unsigned char)0;
     
         data[p*4 + 3] = (unsigned char)0;      
      }
      osg::Image* propertyField = new osg::Image();

      propertyField->allocateImage(fieldSize[0],
                                   fieldSize[1],
                                   fieldSize[2],
                                   GL_RGBA,GL_UNSIGNED_BYTE);

      propertyField->setImage(fieldSize[0], fieldSize[1], fieldSize[2],
		              GL_RGBA, GL_RGBA, GL_UNSIGNED_BYTE,
                              data,/*may need a function to init empty data*/
                              osg::Image::USE_NEW_DELETE,1);

      _property = new osg::Texture3D();
      _property->setDataVariance(osg::Object::DYNAMIC);
      
      osg::TexEnv* texEnv = new osg::TexEnv();
      texEnv->setMode(osg::TexEnv::REPLACE);

      _property->setFilter(osg::Texture3D::MIN_FILTER,osg::Texture3D::LINEAR);
      _property->setFilter(osg::Texture3D::MAG_FILTER,osg::Texture3D::LINEAR);
      _property->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
      _property->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
      _property->setInternalFormat(GL_RGBA);
      _property->setTextureSize(fieldSize[0],
                             fieldSize[1],
                             fieldSize[2]);
      _property->setImage(propertyField);
      if(data){
         delete [] data;
         data = 0;
      }
   }else{
      std::cout<<"Invalid field size!!"<<std::endl;
      std::cout<<"cfdOSGTransferShaderManager::_initPropertyTexture"<<std::endl;
   }

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
     _velocity->setWrap(osg::Texture3D::WRAP_R,osg::Texture3D::CLAMP);
     _velocity->setWrap(osg::Texture3D::WRAP_S,osg::Texture3D::CLAMP);
     _velocity->setWrap(osg::Texture3D::WRAP_T,osg::Texture3D::CLAMP);
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
   }
   _velocityCbk->SetTextureManager(_tm);
   _velocityCbk->SetDelayTime(1.0);
   _velocity->setSubloadCallback(_velocityCbk);
  
}
////////////////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
   _createVelocityFromTextureManager();
}
/////////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::_attachVolumeVisNodeToGraph()
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

   if(!_shaderSwitch){
      _shaderSwitch = new cfdSwitch();
      _shaderSwitch->SetName("Advection Volume Switch");
      _visualBoundingBox->addChild(_shaderSwitch->GetRawNode());
      _bboxSwitch->addChild(_shaderSwitch->GetRawNode());
   }
   //0 == plain vis group (doesn't use shaders)
   //1 == volume shader group (uses scalar texture)
   ((osg::Group*)_shaderSwitch->GetRawNode())->addChild(_vvN.get());

   

   //set up the shader nodes
   if(!_advectionFragGroup.valid()){
      _advectionFragGroup = new osg::Group();
      _advectionFragGroup->setName("Advection VV Fragment PG");
   }
   ((osg::Group*)_shaderSwitch->GetRawNode())->addChild(_advectionFragGroup.get());
   
   //default to the "advection" 
   _shaderSwitch->SetVal(1);
   
   //attach this node to the switch so we can turn off/on
   //the fragment shader
   osg::ref_ptr<osg::Group> attachToGroup = dynamic_cast<osg::Group*>
	(((osg::Group*)_vvN->getChild(0))->getChild(0));
        _advectionFragGroup->addChild(attachToGroup.get());

   //create the advection subgraph
   if(!_advectionSlice.valid()){
      _advectionSlice = CreateAdvectionSubGraph(_tm).get();
       
      _advectionSlice->setStateSet(_aSM->GetShaderStateSet());   
   }
   
   if(_pbuffer){
      osg::ref_ptr<osg::TexGenNode> tmpTGN = (osg::TexGenNode*)_advectionFragGroup->getChild(0);
      _advectionFragGroup->setUpdateCallback(new cfdAdvectPropertyCallback(_advectionSlice.get()));
      _cullCallback = new cfd3DTextureCullCallback(_advectionSlice.get(),
                                               _property.get(),
                                               _pbuffer,tmpTGN->getTexGen(),
                                               _tm->fieldResolution()[2]);
      _advectionFragGroup->setCullCallback(_cullCallback);
   }
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
   if(_aSM&&_property.valid()){
      _texturePingPong->SetPingTexture(_aSM->GetPropertyTexture());
      _texturePingPong->SetPongTexture(_property.get());
   }
}
//////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::PingPongTextures()
{
   if(_texturePingPong && _property.valid()){
      _texturePingPong->PingPongTextures();
   }
}

///////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::EnableAdvectionShader()
{
   if(_shaderSwitch){
      _shaderSwitch->SetVal(1);
   }
}
////////////////////////////////////////////////////////
void cfdVectorVolumeVisHandler::DisableAdvectionShader()
{
   if(_shaderSwitch){
      _shaderSwitch->SetVal(0);
   }
}

//////////////////////////////////////////////////////////////////////////
cfdVectorVolumeVisHandler&
cfdVectorVolumeVisHandler::operator=(const cfdVectorVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);
      _aSM = vvnh._aSM;
      _advectionFragGroup = vvnh._advectionFragGroup;
      _texturePingPong = vvnh._texturePingPong;
      _shaderSwitch = vvnh._shaderSwitch;
      _tm = vvnh._tm;

      _velocityCbk = vvnh._velocityCbk;
      _pbuffer = vvnh._pbuffer;
      _cullCallback = vvnh._cullCallback;
      _texturePingPong = vvnh._texturePingPong;
      _advectionFragGroup = vvnh._advectionFragGroup;
      _advectionSlice = vvnh._advectionSlice;
      _property = vvnh._property;
      _velocity = vvnh._velocity;
      _transferSM =  vvnh._transferSM;
   }
   return *this;
}
#endif //CFD_USE_SHADERS
#endif //_OSG

