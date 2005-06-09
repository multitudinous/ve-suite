#include "VE_SceneGraph/cfdDCS.h"
#include "VE_SceneGraph/cfdClone.h"
#ifdef _OSG
#include <osg/MatrixTransform>
#include <osg/CopyOp>
//#include "VE_SceneGraph/cfdMaterial.h"
#elif _PERFORMER
#elif _OPENSG
#endif

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
   CloneNode( original );
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
   _originalNode = original->GetRawNode();
   _instanceNode = dynamic_cast<osg::Node*>(_originalNode->clone(osg::CopyOp::SHALLOW_COPY));
   if(_instanceNode.valid())
   {
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
void cfdClone::SetTranslationArray(float* translation)
{
   if(_cloneTransform){
      _cloneTransform->SetTranslationArray(translation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetRotationArray(float* rotation)
{
   if(_cloneTransform){
      _cloneTransform->SetRotationArray(rotation);
   }
}
/////////////////////////////////////////////////
void cfdClone::SetScaleArray(float* scale)
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
