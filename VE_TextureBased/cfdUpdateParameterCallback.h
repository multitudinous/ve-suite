#ifndef CFD_UPDATE_PARAMETER_CALLBACK_H
#define CFD_UPDATE_PARAMETER_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG

#include <osg/Uniform>
#include "VE_Installer/include/VEConfig.h"
namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdUpdateParameterCallback
      : public osg::Uniform::Callback{

      public:
	      cfdUpdateParameterCallback(); 
  
	/*cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                           const osg::CopyOp &copyop = osg::CopyOp::SHALLOW_COPY);
*/
         enum cfdParameterType{VECTOR,MATRIX,TIME};
         enum cfdParameterSize{ONE=0,TWO,THREE,FOUR};
         virtual void operator () (osg::Uniform* uniVar, osg::NodeVisitor* nv);

         void setTypeAndSize(cfdParameterType type,
                           cfdParameterSize size)
         {
            _type = type;
            _size = size;
         }

         void updateParameter(float* value);
   
      protected:
         float _value[4];
         cfdParameterType _type;
         cfdParameterSize _size;
   };
}
#endif //_OSG
#endif //CFD_UPDATE_PARAMETER_CALLBACK_H
#endif
