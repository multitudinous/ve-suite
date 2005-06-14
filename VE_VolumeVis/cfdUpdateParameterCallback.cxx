#ifdef VE_PATENTED
#ifdef _OSG

#include <iostream>
#include "cfdUpdateParameterCallback.h"
#include <osg/NodeVisitor>

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
/*cfdUpdateParameterCallback
::cfdUpdateParameterCallback(const cfdUpdateParameterCallback &copy,
                          const osg::CopyOp &copyop )
:osg::Uniform::Callback(copy, copyop)
{
   _value[0] = 0;
   _value[1] = 0;
   _value[2] = 0;
   _value[3] = 0;
   _type = copy._type;
   _size = copy._size;
}*/
///////////////////////////////////////////////////////////////////////////////////////
void cfdUpdateParameterCallback::operator()(osg::Uniform* uniVar, osg::NodeVisitor* nv)
{
   if(_type == VECTOR){
      switch(_size){
         case ONE:
            uniVar->set( _value[0]);
            break;
         case TWO:
            uniVar->set(osg::Vec2(_value[0],_value[1]));
            break;
         case THREE:
            uniVar->set(osg::Vec3(_value[0],_value[1],_value[2]));
            break;
         case FOUR:
         default:
            uniVar->set(osg::Vec4(_value[0],_value[1],_value[2],_value[3]));
            break;
      };
   }else if(_type == TIME){
      uniVar->set((float)nv->getFrameStamp()->getReferenceTime());   
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
#endif //_OSG
#endif
