#ifdef _OSG

#include "cfdTextureManager.h"
#include "cfdScalarVolumeVisHandler.h"

#ifdef CFD_USE_SHADERS
#include "cfdOSGScalarShaderManager.h"
#endif
#include <osg/TexGen>
#include <osg/Group>
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
#ifdef CFD_USE_SHADERS
   _sSM = 0;
#endif
}
//////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{
#ifdef CFD_USE_SHADERS
   _sSM = new cfdOSGScalarShaderManager(*vvnh._sSM);
#endif
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
}
//////////////////////////////////////
void cfdScalarVolumeVisHandler::Init()
{
   cfdVolumeVisNodeHandler::Init();
   //set our names for debugging purposes
   SetBoundingBoxName("Scalar VVNH BBox");
   SetDecoratorName("Scalar VV Fragment PG");
}
/////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_setUpDecorator()
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

   if(_sSM->GetShaderStateSet() && _decoratorGroup.valid()){
      //_sSM->GetShaderStateSet()->setTextureAttributeAndModes(0,_texGen.get(),
         //osg::StateAttribute::ON|osg::StateAttribute::OVERRIDE);
     _decoratorGroup->setStateSet(_sSM->GetShaderStateSet()); 
   }
#endif
}
//////////////////////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler&
cfdScalarVolumeVisHandler::operator=(const cfdScalarVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);
#ifdef CFD_USE_SHADERS
      _sSM = vvnh._sSM;
#endif
   }
   return *this;
}

#endif //_OSG

