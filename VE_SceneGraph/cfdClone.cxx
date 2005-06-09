#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"
#include <osg/MatrixTransform>
#include <osg/CopyOp>
//#include "VE_SceneGraph/cfdMaterial.h"

////////////////////
cfdClone::cfdClone()
{
   _cloneTransform = 0;
   _originalNode = 0;
   _instanceNode = 0;
   //implement later
   /*_cloneMaterial = 0*/
}
/////////////////////////////////////
cfdClone::cfdClone(cfdNode* original)
{
   _cloneTransform = new cfdDCS();
   _originalNode = original->GetRawNode();
}
/////////////////////
cfdClone::~cfdClone()
{
   if(_cloneTransform){
      delete _cloneTransform;
      _cloneTransform = 0;
   }
}
///////////////////////////////////////////
void cfdClone::CloneNode(cfdNode* original)
{
   if(!_cloneTransform){
      _cloneTransform = new cfdDCS();
   }
#ifdef _OSG
   _instanceNode = dynamic_cast<osg::Node*>(_originalNode->clone(osg::CopyOp::SHALLOW_COPY));
   if(_instanceNode.valid()){
      unsigned int nChildren = _cloneTransform->GetNumChildren();
      if(nChildren){
         //should only be one child
         _cloneTransform->RemoveChild(_cloneTransform->GetChild(0));
      }
      ((osg::MatrixTransform*)_cloneTransform->GetRawNode())->addChild(_instanceNode.get());
   }
#elif _PERFORMER
#elif _OPENSG
#endif
}
/////////////////////////////////////////////////
void cfdClone::SetTranslation(float* translation)
{
   if(_cloneTransform){
      _cloneTransform->SetTranslationArray(translation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetRotation(float* rotation)
{
   if(_cloneTransform){
      _cloneTransform->SetRotationArray(rotation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetScale(float* scale)
{
   if(_cloneTransform){
      _cloneTransform->SetScaleArray(scale);
   }
}
//////////////////////////////////
cfdDCS* cfdClone::GetClonedGraph()
{
   if(_cloneTransform){
      return _cloneTransform;
   }
   return 0;
}
   /*void SetMaterial(cfdMaterial* mat);
     void SetDiffuse(float* color);    
     void SetAmbient(float* color);    
     void SetEmmision(float* color);    
     void SetSpecular(float spec);    
     void SetOpacity(float op);
    */

   //returns the cloned structure including the
   //transform
