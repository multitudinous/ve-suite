#ifndef CFD_CLONE_H
#define CFD_CLONE_H
#include "VE_SceneGraph/cfdNode.h"
namespace VE_SceneGraph{
   class cfdDCS;
}
//class cfdMaterial;
namespace VE_SceneGraph{
   class VE_SCENEGRAPH_EXPORTS cfdClone: public cfdNode
   {
      public:
         cfdClone();
         cfdClone(cfdNode* original);
         virtual ~cfdClone();

         void CloneNode(cfdNode* original);
         void SetTranslationArray(float* translation);
         void SetRotationArray(float* rotation);
         void SetScaleArray(float* scale);
         /*void SetMaterial(cfdMaterial* mat);
         void SetDiffuse(float* color);    
         void SetAmbient(float* color);    
         void SetEmmision(float* color);    
         void SetSpecular(float spec);    
         void SetOpacity(float op);
         */

         //returns the cloned structure including the
         //transform
         cfdDCS* GetClonedGraph();
      protected:
         cfdDCS* _cloneTransform;
#ifdef _OSG
         osg::ref_ptr<osg::Node> _originalNode;
         osg::ref_ptr<osg::Node> _instanceNode;
#elif _PERFORMER
        pfNode* _originalNode;
         pfNode* _instanceNode;
#elif _OPENSG
#endif
      //implement later
      /*cfdMaterial* _cloneMaterial*/
   };
}
#endif //CFD_CLONE_H
