#include <cmath>
#include <cassert>

#include <osg/Group>
#include <osg/BoundingSphere>
#include <osg/BoundingBox>

#include "VE_Xplorer/XplorerHandlers/cfdNavigate.h"
#include "VE_Xplorer/XplorerHandlers/cfdEnvironmentHandler.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"
#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"
#include "VE_Xplorer/XplorerHandlers/cfdTrackball.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

const double OneEightyDivPI=57.29577951;
const double PIDivOneEighty=.0174532925;
const float offset=0.5f;

////////////////////////////////////////////////////////////////////////////////
cfdTrackball::cfdTrackball()
{
   tb_currPos[0]=0.0f;
	tb_currPos[1]=0.0f;
	tb_prevPos[0]=0.0f;
	tb_prevPos[1]=0.0f;
	tb_moving=false;
	tb_width=1;
	tb_height=1;
	tb_FOVy=1;

   Init();
}
////////////////////////////////////////////////////////////////////////////////
cfdTrackball::~cfdTrackball(){
   ;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Init()
{
	identity(tb_transform);
	identity(tb_accuTransform);
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Update()
{
   gadget::KeyboardMouse::EventQueue::iterator i;

   for(i=cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.begin();
       i!=cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.end();++i){
       
      const gadget::EventType type=(*i)->type();

      if(type==gadget::KeyPressEvent){
         gadget::KeyEventPtr key_evt=dynamic_pointer_cast<gadget::KeyEvent>(*i);
         Keyboard(key_evt->getKey());
         //std::cout<<key_evt->getKey()<<std::endl;
      }
      
      else if(type==gadget::MouseButtonPressEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         Mouse(mouse_evt->getButton(),1,mouse_evt->getX(),mouse_evt->getY());
      }

      else if(type==gadget::MouseButtonReleaseEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         Mouse(mouse_evt->getButton(),0,mouse_evt->getX(),mouse_evt->getY());
      }

      else if(type==gadget::MouseMoveEvent){
         gadget::MouseEventPtr mouse_evt=dynamic_pointer_cast<gadget::MouseEvent>(*i);
         Motion(mouse_evt->getX(),mouse_evt->getY());
      }
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Matrix()
{
	assert(tb_button!=-1);

   if(cfdEnvironmentHandler::instance()->GetKeyboardMouse()->evt_queue.empty()){
      return;
   }

	tb_accuTransform=cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();

   Update();

   Matrix44f mat;
   Matrix44f accRotation;

	identity(mat);
	accRotation=tb_accuTransform;
   float accTranslation[3];
	
	for(int i=0;i<3;i++){
		accRotation[i][3]=0.0;
	}

	for(int i=0;i<3;i++){
		mat[i][3]=tb_accuTransform[i][3];
	}
	mat=mat*tb_transform;
	mat=mat*accRotation;
	tb_accuTransform=mat;

	for(int i=0;i<3;i++){
		accTranslation[i]=tb_accuTransform[i][3];
	}

	cfdPfSceneManagement::instance()->GetWorldDCS()->SetTranslationArray((float *)accTranslation);
	cfdPfSceneManagement::instance()->GetWorldDCS()->SetRotationMatrix(tb_accuTransform);

	identity(tb_transform);
	//Shouldn't need this line, but for some reason the identity call is not
	//completely zeroing out the translation of the tb_transform.
	tb_transform[0][3]=tb_transform[1][3]=tb_transform[2][3]=0.0;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Reshape(unsigned int width,unsigned int height)
{
	assert(tb_button!=-1);
	tb_width=width;
	tb_height=height;
   tb_aspectRatio=(float)width/(float)height;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::SetFOVy(float _top,float _bottom,float _near)
{
	float topAngle=(OneEightyDivPI)*atan(_top/_near);
   float tempDiv=fabs(_bottom)/_near;
	float bottomAngle=(OneEightyDivPI)*atan(tempDiv);
   tb_FOVyRatio=topAngle/bottomAngle;
	tb_FOVy=topAngle+bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Keyboard(int key)
{
   // If "r" is pressed
   if(key==35){
      ResetTransforms();
   }
   // If "f" is pressed
   else if(key==23){
      FitToScreen();
   }
   else{
      return;
   }
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Mouse(int button,int state,int x,int y)
{
	assert(tb_button!=-1);
	tb_button=button;
	if(state==1){
		tb_currPos[0]=(float)x/(float)tb_width;
		tb_currPos[1]=(float)y/(float)tb_height;
		tb_prevPos[0]=(float)x/(float)tb_width;
		tb_prevPos[1]=(float)y/(float)tb_height;
		tb_moving=true;
	}
	else if(state==0)
		tb_moving=false;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Motion(int x,int y)
{
	if(!tb_moving)
		return;
	tb_currPos[0]=(float)x/(float)tb_width;
	tb_currPos[1]=(float)y/(float)tb_height;
	float dx=tb_currPos[0]-tb_prevPos[0];
	float dy=tb_currPos[1]-tb_prevPos[1];
	float mag=sqrtf(dx*dx+dy*dy);
	if(mag<1.0e-6f)
		return;
	if(tb_button==49&&(x>.1*tb_width&&x<.9*tb_width)&&(y>.1*tb_height&&y<.9*tb_height))
   	RotateView(dx,dy);
	else if(tb_button==51)
		Zoom(dy);
	else if(tb_button==50)
      Pan(dx,dy);
	else if(tb_button==49)
		Twist(dx,dy);
	tb_prevPos[0]=tb_currPos[0];
	tb_prevPos[1]=tb_currPos[1];
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::ResetTransforms()
{
   identity(tb_accuTransform);
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::FitToScreen()
{
   osg::BoundingSphere bs;

   bs.expandBy(cfdPfSceneManagement::instance()->GetRootNode()->GetRawNode()->getBound());

	float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
   float z=bs.radius()*tb_FOVyRatio;
   float d=(bs.radius()/tan(Theta))*tb_aspectRatio;  

   tb_accuTransform[0][3]=0.0f;
   tb_accuTransform[1][3]=d;
   tb_accuTransform[2][3]=0.0f;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::RotateView(float dx,float dy)
{
   float mag=sqrtf(dx*dx+dy*dy);
   float angle=mag*400.0f;

   Matrix44f mat;
	identity(mat);
	float tb_axis[3];
	tb_axis[0]=mat[0][0]*dy+mat[2][0]*dx;
	tb_axis[1]=mat[0][1]*dy+mat[2][1]*dx;
	tb_axis[2]=mat[0][2]*dy+mat[2][2]*dx;
	Rotate(tb_axis[0],tb_axis[1],tb_axis[2],angle);
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Twist(float dx,float dy)
{
   Matrix44f mat;
	identity(mat);
	float mag=sqrtf(dx*dx+dy*dy);
	float Theta=atan2f(tb_prevPos[0]-0.5,tb_prevPos[1]-0.5);
	float newTheta=atan2f(tb_currPos[0]-0.5,tb_currPos[1]-0.5);
	float angle=(OneEightyDivPI)*(Theta-newTheta);
	Rotate(mat[1][0],mat[1][1],mat[1][2],angle);
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Zoom(float dy)
{
	float viewlength=fabs(tb_accuTransform[1][3]);
	float d=(viewlength*(1/(1+dy*2)))-viewlength;

   //**********Temporary Fix**********//
	if((tb_accuTransform[1][3]>-offset)&&(tb_accuTransform[1][3]<offset)){
		if(tb_accuTransform[1][3]==0)
			tb_transform[1][3]=offset;
		else if(tb_accuTransform[1][3]>0)
			tb_transform[1][3]=-2*offset;
		else if(tb_accuTransform[1][3]<0)
			tb_transform[1][3]=2*offset;
	}
	else
		tb_transform[1][3]=d;
   //*********************************//

}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Pan(float dx,float dy)
{

   //**********Temporary Fix**********//
   if((tb_accuTransform[1][3]>-offset)&&(tb_accuTransform[1][3]<offset)){
		if(tb_accuTransform[1][3]==0)
			tb_transform[1][3]=offset;
		else if(tb_accuTransform[1][3]>0)
			tb_transform[1][3]=-2*offset;
		else if(tb_accuTransform[1][3]<0)
			tb_transform[1][3]=2*offset;
	}
   //*********************************//
	
   float d=sqrtf(pow(tb_accuTransform[1][3],2));
	float mag=sqrtf(dx*dx+dy*dy);
	float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
	float b=2*d*tan(Theta);
	float dwx=dx*b*tb_aspectRatio;
	float dwy=-dy*b;
	tb_transform[0][3]=dwx;
	tb_transform[2][3]=dwy;
}
////////////////////////////////////////////////////////////////////////////////
void cfdTrackball::Rotate(float x,float y,float z,float angle)
{
	float rad=(float)(angle*PIDivOneEighty);
	float cosAng=(float)(cos(rad));
	float sinAng=(float)(sin(rad));
	float denom=sqrtf(x*x+y*y+z*z);
	if(denom!=0.0f){
		x/=denom;
		y/=denom;
		z/=denom;
	}

	zero(tb_transform);

	tb_transform[0][0]=(x*x)+(cosAng*(1-(x*x)));
	tb_transform[1][0]=(x*y)-(cosAng*(x*y))+(sinAng*z);
	tb_transform[2][0]=(x*z)-(cosAng*(x*z))-(sinAng*y);
	tb_transform[0][1]=(x*y)-(cosAng*(x*y))-(sinAng*z);
	tb_transform[1][1]=(y*y)+(cosAng*(1-(y*y)));
	tb_transform[2][1]=(y*z)-(cosAng*(y*z))+(sinAng*x);
	tb_transform[0][2]=(x*z)-(cosAng*(x*z))+(sinAng*y);
	tb_transform[1][2]=(y*z)-(cosAng*(y*z))-(sinAng*x);
	tb_transform[2][2]=(z*z)+(cosAng*(1-(z*z)));
	tb_transform[3][3]=1.0f;
}
////////////////////////////////////////////////////////////////////////////////
