#ifndef CFD_UPDATE_PARAMETER_CALLBACK_H
#define CFD_UPDATE_PARAMETER_CALLBACK_H
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

	void operator()(osgNV::ParameterValue *param, osg::State &state) const;
	
   void setType(int type){_type = type;}

   void updateParameter(float* value);
   
protected:
   float _value[4];
   int _type;

};
#endif //CFD_USE_SHADERS
#endif //_OSG
#endif //CFD_UPDATE_PARAMETER_CALLBACK_H