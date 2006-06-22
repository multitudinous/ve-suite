#include <cmath>
#include <cassert>

#include "VE_Xplorer/trackball.h"
#include "VE_Xplorer/cfdNavigate.h"
#include "VE_Xplorer/cfdEnvironmentHandler.h"
#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/cfdDCS.h"

using namespace VE_Xplorer;
using namespace VE_SceneGraph;

const float offset=0.5f;

Trackball::Trackball(){
	tb_currPos[0]=0.0f;
	tb_currPos[1]=0.0f;
	tb_prevPos[0]=0.0f;
	tb_prevPos[1]=0.0f;
	tb_tracking=false;
	tb_moving=false;
	tb_width=1;
	tb_height=1;
	tb_FOVy=1;
}

Trackball::~Trackball(){;}

void Trackball::Init(){
	identity(tb_transform);
	identity(tb_accuTransform);
}

void Trackball::Matrix(){
	assert(tb_button!=-1);

	tb_accuTransform=cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();

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

void Trackball::Reshape(unsigned int width,unsigned int height){
	assert(tb_button!=-1);
	tb_width=width;
	tb_height=height;
	tb_aspectRatio=(float)width/(float)height;
}

void Trackball::SetFOVy(float _top,float _bottom,float _near){
	float topAngle=(OneEightyDivPI)*atan(_top/_near);
	float bottomAngle=(OneEightyDivPI)*atan(abs(_bottom)/_near);
	tb_FOVy=topAngle+bottomAngle;
}

void Trackball::Keyboard(int key){
	tb_key=key;
	if(tb_key==113)
		ResetTransforms();

	tb_key=NULL;
}

void Trackball::Mouse(int button,int state,int x,int y){
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

void Trackball::Motion(int x,int y){
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

void Trackball::ResetTransforms(){
	float d;
	float w=abs((tb_max[0]-tb_min[0])*0.5f);
	float h=abs((tb_max[1]-tb_min[1])*0.5f);
	float depth=abs((tb_max[2]-tb_min[2])*0.5f);
	float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
	if(w>h&&w>depth)
		d=(w/tan(Theta));
	else if(h>w&&h>depth)
		d=(h/tan(Theta))*tb_aspectRatio;
	else
		d=(depth/tan(Theta))*tb_aspectRatio;
}

void Trackball::RotateView(float dx,float dy){
	Matrix44f mat;
	identity(mat);
	float mag=sqrtf(dx*dx+dy*dy);
	tb_angle=mag*400.0f;
	tb_axis[0]=mat[0][0]*dy+mat[2][0]*dx;
	tb_axis[1]=mat[0][1]*dy+mat[2][1]*dx;
	tb_axis[2]=mat[0][2]*dy+mat[2][2]*dx;
	Rotate(tb_axis[0],tb_axis[1],tb_axis[2],tb_angle);
}

void Trackball::Twist(float dx,float dy){
	Matrix44f mat;
	identity(mat);
	float mag=sqrtf(dx*dx+dy*dy);
	float Theta=atan2f(tb_prevPos[0]-0.5,tb_prevPos[1]-0.5);
	float newTheta=atan2f(tb_currPos[0]-0.5,tb_currPos[1]-0.5);
	tb_angle=(OneEightyDivPI)*(Theta-newTheta);
	Rotate(mat[1][0],mat[1][1],mat[1][2],tb_angle);
}

void Trackball::Zoom(float dy){
	float viewlength=abs(tb_accuTransform[1][3]);
	float d=(viewlength*(1/(1+dy*2)))-viewlength;

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
}

void Trackball::Pan(float dx,float dy){
	float mag=sqrtf(dx*dx+dy*dy);
	float d=sqrtf(pow(tb_accuTransform[1][3],2));
	float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
	float b=2*d*tan(Theta);
	float dwx=dx*b*tb_aspectRatio;
	float dwy=-dy*b;
	tb_transform[0][3]=dwx;
	tb_transform[2][3]=dwy;
}

void Trackball::Rotate(float x,float y,float z,float angle){
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
