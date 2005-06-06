#ifndef CFD_UPDATE_PARAMETER_CALLBACK_H
#define CFD_UPDATE_PARAMETER_CALLBACK_H
#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <osgNV/Version>
#include <osgNV/VectorParameterValue>
#include <osgNV/ParameterValueCallback>
class cfdUpdateParameterCallback: public osgNV::ParameterValueCallback 
{
public:
	cfdUpdateParameterCallback(); 
  
	cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                           const osg::CopyOp &copyop = osg::CopyOp::SHALLOW_COPY);


	META_Object(test, cfdUpdateParameterCallback);

   enum cfdParameterType{VECTOR,MATRIX};
   enum cfdParameterSize{ONE=0,TWO,THREE,FOUR};
	void operator()(osgNV::ParameterValue *param, osg::State &state) const;
	
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
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif //CFD_UPDATE_PARAMETER_CALLBACK_H
#endif
