#ifdef _OSG

#include "cfdTextureManager.h"
#include "cfdScalarVolumeVisHandler.h"
#include "cfdTextureMatrixCallback.h"
#ifdef CFD_USE_SHADERS
#include "cfdOSGScalarShaderManager.h"

#endif
#include <osg/TexGen>
#include <osg/TexMat>
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
/////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_applyTextureMatrix()
{
   unsigned int tUnit = 0;
#ifdef CFD_USE_SHADERS
   tUnit = _sSM->GetAutoGenTextureUnit();
#endif
   osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
   tMat->setMatrix(osg::Matrix::identity());
   _decoratorGroup->getStateSet()->setTextureAttributeAndModes(tUnit,tMat.get(),osg::StateAttribute::ON);
   float trans[3] = {.5,.5,.5};
   _decoratorGroup->setUpdateCallback(new cfdTextureMatrixCallback(tMat.get(),
                                                             _center,
                                                             _scale,
                                                             trans));
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

