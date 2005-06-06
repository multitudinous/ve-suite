#ifdef VE_PATENTED
#ifdef _OSG
#ifdef CFD_USE_SHADERS
#include <iostream>
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
   _type = VECTOR;
   _size = ONE;
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
   _type = copy._type;
   _size = copy._size;
}
/////////////////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::operator()(osgNV::ParameterValue *param, 
                                            osg::State &state) const
{
   if(_type == VECTOR){
      osgNV::VectorParameterValue *cgp = 
          dynamic_cast<osgNV::VectorParameterValue *>(param);
      if (cgp){
         switch(_size){
            case ONE:
               cgp->set(_value[0]);
               break;
            case TWO:
               cgp->set(osg::Vec2(_value[0],_value[1]));
               break;
            case THREE:
               cgp->set(osg::Vec3(_value[0],_value[1],_value[2]));
               break;
            case FOUR:
            default:
               cgp->set(osg::Vec4(_value[0],_value[1],_value[2],_value[3]));
               break;
         };
      }

   }else{
      std::cout<<"Unknown parameter type in cfdUpdateParameterCallback."<<std::endl;
   }
}
//////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::updateParameter(float* v)
{
   switch(_size){
      case ONE:
         _value[0] = v[0];
         break;
      case TWO:
         _value[0] = v[0];
         _value[1] = v[1];
         break;
      case THREE:
         _value[0] = v[0];
         _value[1] = v[1];
         _value[2] = v[2];
         break;
      case FOUR:
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
#endif
