#include <iostream>

#include <osg/Vec3>
#include <osg/Vec4>
#include <osg/Geometry>
#include <osg/Group>
#include <osg/Geode>
#include <osg/LineSegment>

#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdGroup.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/MouseSelection.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

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
   if(cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.empty()){
      return 0;
   }

   Update();
   return ProjectNormalizedXYIntoObjectCoordinates(ms_projection_matrix,ms_view_matrix,ms_normalized_x,ms_normalized_y);
}
////////////////////////////////////////////////////////////////////////////////
void MouseSelection::Update()
{
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

   osg::ref_ptr<osg::Geometry> line=new osg::Geometry;
   osg::ref_ptr<osg::Vec3Array> vertices=new osg::Vec3Array;
   osg::ref_ptr<osg::Vec4Array> colors=new osg::Vec4Array;
   vertices->push_back(near_point);
   vertices->push_back(far_point);
   colors->push_back(osg::Vec4(1.0f,0.0f,0.0f,1.0f));
   line->setVertexArray(vertices.get());
   line->setColorArray(colors.get());
   line->setColorBinding(osg::Geometry::BIND_OVERALL);

   line->addPrimitiveSet(new osg::DrawArrays(osg::PrimitiveSet::LINES,0,vertices->size()));

   osg::ref_ptr<osg::Geode> geode=new osg::Geode;
   geode->addDrawable(line.get());

   
   cfdPfSceneManagement::instance()->GetRootNode()->GetRawNode()->asGroup()->addChild(geode.get());

   return lineSegment;
}
////////////////////////////////////////////////////////////////////////////////
