#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include "cfdUpdateParameterCallback.h"


////////////////////////////////////////////////////////
//Constructors                                        //
////////////////////////////////////////////////////////
cfdUpdateParameterCallback::cfdUpdateParameterCallback() 
{ 
   _value[0] = 0;
   _value[1] = 0;
   _value[2] = 0;
   _value[3] = 0;
}
////////////////////////////////////////////////////////////////////////////////////
cfdUpdateParameterCallback
::cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                          const osg::CopyOp &copyop )
:osgNV::ParameterValueCallback(copy, copyop) 
{
   _value[0] = 0;
   _value[1] = 0;
   _value[2] = 0;
   _value[3] = 0;
}
/////////////////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::operator()(osgNV::ParameterValue *param, 
                                            osg::State &state) const
{
   osgNV::VectorParameterValue *cgp = 
          dynamic_cast<osgNV::VectorParameterValue *>(param);
   if (cgp){
      switch(_type){
         case 0:
            cgp->set(_value[0]);
            break;
         case 1:
            cgp->set(osg::Vec2(_value[0],_value[1]));
            break;
         case 2:
            cgp->set(osg::Vec3(_value[0],_value[1],_value[2]));
            break;
         case 3:
         default:
            cgp->set(osg::Vec4(_value[0],_value[1],_value[2],_value[3]));
            break;
      };
   }
}
//////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::updateParameter(float* v)
{
   switch(_type){
      case 0:
         _value[0] = v[0];
         break;
      case 1:
         _value[0] = v[0];
         _value[1] = v[1];
         break;
      case 2:
         _value[0] = v[0];
         _value[1] = v[1];
         _value[2] = v[2];
         break;
      case 3:
      default:
         _value[0] = v[0];
         _value[1] = v[1];
         _value[2] = v[2];
         _value[3] = v[3];
         break;
   };
}
#endif //CFD_USE_SHADERS
#endif //_OSG