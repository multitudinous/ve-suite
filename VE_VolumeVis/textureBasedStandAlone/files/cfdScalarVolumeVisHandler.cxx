#ifdef VE_PATENTED
#ifdef _OSG

#include "cfdTextureManager.h"
#include "cfdScalarVolumeVisHandler.h"
#include "cfdTextureMatrixCallback.h"
#ifdef CFD_USE_SHADERS
#include "cfdOSGScalarShaderManager.h"
#include "cfdOSGGammaShaderManager.h"


#include <osg/TexGen>
#include <osg/TexMat>
#include <osg/Group>
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
   _sSM = 0;
   _transferSM = 0;
}
//////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{

   _sSM = new cfdOSGScalarShaderManager(*vvnh._sSM);
   _transferSM = new cfdOSGGammaShaderManager(*vvnh._transferSM);

}
///////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::~cfdScalarVolumeVisHandler()
{

   if(_sSM){
      delete _sSM;
      _sSM = 0;
   }
   if(_transferSM){
      delete _transferSM;
      _transferSM = 0;
   }

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

   /*int* res = _tm->fieldResolution();
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
   }*/
   _createTransferShader();

}
///////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_createTransferShader()
{
   if(!_transferSM && _tm){
      int* fieldSize = _tm->fieldResolution();
      _transferSM = new cfdOSGGammaShaderManager();
      _transferSM->SetUseTextureManagerForProperty(true);
      _transferSM->SetFieldSize(fieldSize[0],fieldSize[1],fieldSize[2]);
      _transferSM->InitTextureManager(_tm);
      _transferSM->Init();
      if(_decoratorGroup.valid()){
         _decoratorGroup->setStateSet(_transferSM->GetShaderStateSet());
      }
   }
}
/////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_applyTextureMatrix()
{
   unsigned int tUnit = 0;

   //tUnit = _sSM->GetAutoGenTextureUnit();
   tUnit = _transferSM->GetAutoGenTextureUnit();

   osg::ref_ptr<osg::TexMat> tMat = new osg::TexMat();
   tMat->setMatrix(osg::Matrix::identity());
   _decoratorGroup->getStateSet()->setTextureAttributeAndModes(tUnit,
                                                        tMat.get(),
                                                        osg::StateAttribute::ON);
   float trans[3] = {.5,.5,.5};
   _decoratorGroup->setUpdateCallback(new cfdTextureMatrixCallback(tMat.get(),
                                                             _center,
                                                             _scale,
                                                             trans));
   _updateTexGenUnit(tUnit);
}
//////////////////////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler&
cfdScalarVolumeVisHandler::operator=(const cfdScalarVolumeVisHandler& vvnh)
{
   if(this != &vvnh){
      cfdVolumeVisNodeHandler::operator=(vvnh);

      _sSM = vvnh._sSM;
      _transferSM = vvnh._transferSM;

   }
   return *this;
}
#endif//CFD_USE_SHADERS
#endif //_OSG
#endif
