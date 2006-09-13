#include <iostream>

#include <osg/Vec3>
#include <osg/LineSegment>

#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/MouseSelection.h"

using namespace VE_Xplorer;

////////////////////////////////////////////////////////////////////////////////
MouseSelection::MouseSelection()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
MouseSelection::~MouseSelection()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::LineSegment> MouseSelection::CreateLineSegment()
{
   Update();
   return ProjectNormalizedXYIntoObjectCoordinates(ms_projection_matrix,ms_view_matrix,ms_normalized_x,ms_normalized_y);
}
////////////////////////////////////////////////////////////////////////////////
void MouseSelection::Update()
{
   if(cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.empty()){
      return;
   }

   gadget::KeyboardMouse::EventQueue::iterator i;

   for(i=cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.begin();
       i!=cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.end();++i){
       
      const gadget::EventType type=(*i)->type();

      if(type==gadget::KeyPressEvent){
         gadget::KeyEventPtr key_evt=dynamic_pointer_cast<gadget::KeyEvent>(*i);
         
         //std::cout<<key_evt->getKey()<<std::endl;
      }
      
      else if(type==gadget::MouseButtonPressEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         NormalizeXY(mouse_evt->getX(),mouse_evt->getY());
      }

      else if(type==gadget::MouseButtonReleaseEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         NormalizeXY(mouse_evt->getX(),mouse_evt->getY());
      }

      else if(type==gadget::MouseMoveEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         NormalizeXY(mouse_evt->getX(),mouse_evt->getY());
      }
      std::cout<<ms_normalized_x<<std::endl;
      std::cout<<ms_normalized_y<<std::endl;
   }
}
////////////////////////////////////////////////////////////////////////////////
void MouseSelection::Reshape(unsigned int width,unsigned int height)
{
	//assert(tb_button!=-1);
	ms_width=width;
	ms_height=height;
   ms_aspectRatio=(float)width/(float)height;
}
////////////////////////////////////////////////////////////////////////////////
void MouseSelection::NormalizeXY(float x,float y)
{
   ms_normalized_x=-1.0f+((2*x)/ms_width);
   ms_normalized_y=-1.0f+((2*y)/ms_height);
}
////////////////////////////////////////////////////////////////////////////////
osg::ref_ptr<osg::LineSegment> MouseSelection::ProjectNormalizedXYIntoObjectCoordinates
(osg::Matrix &projectionMatrix,osg::Matrix &viewMatrix,float x,float y)
{
   osg::Matrix matrix=viewMatrix*projectionMatrix;
   osg::Matrix inverseVP;
   inverseVP.invert(matrix);
 
   osg::Vec3 near_point=osg::Vec3(x,y,0.0f)*inverseVP;
   osg::Vec3 far_point=osg::Vec3(x,y,1.0f)*inverseVP;

   osg::ref_ptr<osg::LineSegment> lineSegment=new osg::LineSegment(near_point,far_point);

   return lineSegment;
}
////////////////////////////////////////////////////////////////////////////////
