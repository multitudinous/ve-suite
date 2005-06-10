#ifdef VE_PATENTED
#ifdef _OSG

#include "cfdTextureManager.h"
#include "cfdScalarVolumeVisHandler.h"
#include "cfdTextureMatrixCallback.h"

#include "cfdScalarShaderManager.h"


#include <osg/TexGen>
#include <osg/TexMat>
#include <osg/Group>
//////////////////////////////////////////////////////
//Constructors                                      //
//////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler()
:cfdVolumeVisNodeHandler()
{
   _transferSM = 0;
}
//////////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh)
:cfdVolumeVisNodeHandler(vvnh)
{

   _transferSM = new cfdScalarShaderManager(*vvnh._transferSM);

}
///////////////////////////////////////////////////////
cfdScalarVolumeVisHandler::~cfdScalarVolumeVisHandler()
{

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
   _createTransferShader();

}
///////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_createTransferShader()
{
   if(!_transferSM && _tm){
      int* fieldSize = _tm->fieldResolution();
      _transferSM = new cfdScalarShaderManager();
#ifndef CFD_USE_SHADERS
      _transferSM->UseCG(true);
#endif
      _transferSM->SetUseTextureManagerForProperty(true);
      _transferSM->SetFieldSize(fieldSize[0],fieldSize[1],fieldSize[2]);
      _transferSM->InitTextureManager(_tm);
      _transferSM->Init();
      if(_decoratorGroup.valid()){
         _decoratorGroup->setStateSet(_transferSM->GetShaderStateSet());
      }
   }
}
////////////////////////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::SetTextureManager(cfdTextureManager* tm)
{
   cfdVolumeVisNodeHandler::SetTextureManager(tm);
   if(_transferSM){
      _transferSM->UpdateTextureManager(_tm);
   }
}
/////////////////////////////////////////////////////
void cfdScalarVolumeVisHandler::_applyTextureMatrix()
{
   unsigned int tUnit = 0;
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
      _transferSM = vvnh._transferSM;
   }
   return *this;
}
#endif //_OSG
#endif
