#ifdef _OSG

#include "cfdScalarVolumeVisHandler.h"
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
#ifdef CFD_USE_SHADERS
   _sSM = 0;
#endif
   _tm = 0;
   _shaderSwitch = 0;
}
//////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{
#ifdef CFD_USE_SHADERS
   _sSM = new cfdOSGScalarShaderManager(*vvnh._sSM);
   _scalarFragGroup = new osg::Group(*vvnh._scalarFragGroup);
#endif
   _shaderSwitch = new cfdSwitch(*vvnh._shaderSwitch);
   _tm = new cfdTextureManager(*vvnh._tm);
}
///////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::~cfdScalarVolumeVisHandler()
{
#ifdef CFD_USE_SHADERS
   if(_sSM){
      delete _sSM;
      _sSM = 0;
   }
#endif
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
void cfdScalarVolumeVisHandler::Init()
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
      _attachVolumeVisNodeToGraph();
   }
}
////////////////////////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::SetTextureManager(cfdTextureManager* tm)
{
   _tm = tm;
}
/////////////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_attachVolumeVisNodeToGraph()
{
   if(!_tm){
      return;
   }
#ifdef CFD_USE_SHADERS
   int* res = _tm->fieldResolution();
   if(!_sSM){
      _sSM = new cfdOSGScalarShaderManager();
      _sSM->InitTextureManager(_tm);
   }else{
      _sSM->UpdateTextureManager(_tm);
   }
   _sSM->Init();
#endif
   if(!_shaderSwitch){
      _shaderSwitch = new cfdSwitch();
      _shaderSwitch->SetName("Scalar Volume Switch");
      _visualBoundingBox->addChild(_shaderSwitch->GetRawNode());
      _bboxSwitch->addChild(_shaderSwitch->GetRawNode());
   }

   //0 == plain vis group (doesn't use shaders)
   //1 == volume shader group (uses scalar texture)
   ((osg::Group*)_shaderSwitch->GetRawNode())->addChild(_vvN.get());

   //default to the "plain style" ie w/o shaders, viewing
   _shaderSwitch->SetVal(0);
#ifdef CFD_USE_SHADERS
   //set up the shader nodes
   if(!_scalarFragGroup.valid()){
      _scalarFragGroup = new osg::Group();
      _scalarFragGroup->setName("Scalar VV Fragment PG");
   }
   ((osg::Group*)_shaderSwitch->GetRawNode())->addChild(_scalarFragGroup.get());

   if(_sSM->GetShaderStateSet()){
     _scalarFragGroup->setStateSet(_sSM->GetShaderStateSet()); 
   }
   //attach this node to the switch so we can turn off/on
   //the fragment shader
   osg::ref_ptr<osg::Group> attachToGroup = dynamic_cast<osg::Group*>
	(((osg::Group*)_vvN->getChild(0))->getChild(0));
   _scalarFragGroup->addChild(attachToGroup.get());
#endif
   
}
#ifdef CFD_USE_SHADERS
////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::EnableVolumeShader()
{
   if(_shaderSwitch){
      _shaderSwitch->SetVal(1);
   }
}
/////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::DisableVolumeShader()
{
   if(_shaderSwitch){
      _shaderSwitch->SetVal(0);
   }
}
#endif
//////////////////////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler&
cfdScalarVolumeVisHandler::operator=(const cfdScalarVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);
#ifdef CFD_USE_SHADERS
      _sSM = vvnh._sSM;
      _scalarFragGroup = vvnh._scalarFragGroup;
#endif
      _shaderSwitch = vvnh._shaderSwitch;
      _tm = vvnh._tm;
   }
   return *this;
}

#endif //_OSG

