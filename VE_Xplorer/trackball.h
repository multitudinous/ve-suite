#ifndef TRACKBALL_H
#define TRACKBALL_H

#include <math.h>
#include <assert.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>
#include <GL/glut.h>

const double PI=3.14159265358979323846;
const float FOVy=50.0f;
const float initZ=-5.0f;

//GLMmodel *pmodel=NULL;

class Trackball{
public:
	Trackball(){
		tb_currPos[0]=0.0f;
		tb_currPos[1]=0.0f;
		tb_prevPos[0]=0.0f;
		tb_prevPos[1]=0.0f;
		tb_tracking=GL_FALSE;
		tb_moving=GL_FALSE;
		tb_width=1;
		tb_height=1;
	};
	~Trackball(){};
	/*void drawmodel(){
		if(!pmodel){
			//pmodel=glmReadOBJ("C:/TSVEG/VE_Suite/VE_TestSuite/Hyper/OBJs/Steel-am-2n.obj");
			//pmodel=glmReadOBJ("OBJs/All.obj");
			pmodel=glmReadOBJ("OBJs/Turbine-postcombustor-am-11-realllll.obj");
			if(!pmodel)
				exit(0);
			glmUnitize(pmodel);
			glmFacetNormals(pmodel);
			glmVertexNormals(pmodel,90.0);
			for(int i=1;i<(int)pmodel->numvertices;i++){
				if (tb_max[0]<pmodel->vertices[3*i+0])
					tb_max[0]=pmodel->vertices[3*i+0];
				if (tb_min[0]>pmodel->vertices[3*i+0])
					tb_min[0]=pmodel->vertices[3*i+0];
				 
				if (tb_max[1]<pmodel->vertices[3*i+1])
					tb_max[1]=pmodel->vertices[3*i+1];
				if (tb_min[1]>pmodel->vertices[3*i+1])
					tb_min[1]=pmodel->vertices[3*i+1];
					        
				if (tb_max[2]<pmodel->vertices[3*i+2])
					tb_max[2]=pmodel->vertices[3*i+2];
				if (tb_min[2]>pmodel->vertices[3*i+2])
					tb_min[2]=pmodel->vertices[3*i+2];
			}
		}
		glmDraw(pmodel,GLM_SMOOTH|GLM_MATERIAL);
	}*/
	void Init(){
		glPushMatrix();
			glLoadIdentity();
			glGetFloatv(GL_MODELVIEW_MATRIX,(GLfloat *)tb_transform);
			glGetFloatv(GL_MODELVIEW_MATRIX,(GLfloat *)tb_accuTransform);
		glPopMatrix();
	}
	void Matrix(){
		assert(tb_button!=-1);
		glPushMatrix();
			glLoadIdentity();
			float accRotation[16];
			for(int i=0;i<16;i++)
				accRotation[i]=tb_accuTransform[i];
			accRotation[12]=0.0;
			accRotation[13]=0.0;
			accRotation[14]=0.0;
			glTranslatef(tb_accuTransform[12],tb_accuTransform[13],tb_accuTransform[14]);
			glMultMatrixf((GLfloat *)tb_transform);
			glMultMatrixf((GLfloat *)accRotation);
			glGetFloatv(GL_MODELVIEW_MATRIX,(GLfloat *)tb_accuTransform);
		glPopMatrix();
		glMultMatrixf(tb_accuTransform);
	}
	void Reshape(int width,int height){
		assert(tb_button!=-1);
		tb_width=width;
		tb_height=height;
		tb_aspectRatio=(GLfloat)width/(GLfloat)height;
	}
	void Keyboard(unsigned char key,int x,int y){
		float d;
		tb_key=key;
		if(tb_key==' '){
			float w=abs((tb_max[0]-tb_min[0])/2);
			float h=abs((tb_max[1]-tb_min[1])/2);
			float depth=abs((tb_max[2]-tb_min[2])/2);
		 	float Theta=(FOVy/2)*(PI/180);
			if(w>h&&w>depth)
				d=(w/tan(Theta));
			else if(h>w&&h>depth)
				d=(h/tan(Theta))*tb_aspectRatio;
			else
				d=(depth/tan(Theta))*tb_aspectRatio;
			for(int i=0;i<16;i++)
				tb_transform[i]=0.0f;
			tb_transform[0]=tb_transform[5]=tb_transform[10]=tb_transform[15]=1.0f;
			tb_accuTransform[12]=0.0f;
			tb_accuTransform[13]=0.0f;
			tb_accuTransform[14]=-d-initZ-0.1;
		}
		tb_key=NULL;
	}
	void Mouse(int button,int state,int x,int y){
		assert(tb_button!=-1);
		tb_button=button;
		if(state==GLUT_DOWN){
			tb_currPos[0]=(float)x/(float)tb_width;
			tb_currPos[1]=(float)y/(float)tb_height;
			tb_prevPos[0]=(float)x/(float)tb_width;
			tb_prevPos[1]=(float)y/(float)tb_height;
			tb_moving=GL_TRUE;
		}
		else if(state==GLUT_UP)
			tb_moving=GL_FALSE;
	}
	void Motion(int x,int y){
		if (!tb_moving)
			return;
		tb_currPos[0]=(float)x/(float)tb_width;
		tb_currPos[1]=(float)y/(float)tb_height;
		float dx=tb_currPos[0]-tb_prevPos[0];
		float dy=tb_currPos[1]-tb_prevPos[1];
		float mag=sqrtf(dx*dx+dy*dy);
		if(mag<1.0e-6f)
			return;
		if(tb_button==GLUT_LEFT_BUTTON&&(x>.1*tb_width&&x<.9*tb_width)&&(y>.1*tb_height&&y<.9*tb_height)){
   			_rotateview(dx,dy);
		}
		else if(tb_button==GLUT_RIGHT_BUTTON)
			_zoom(dy);
		else if(tb_button==GLUT_MIDDLE_BUTTON)
       		_pan(dx,dy);
		else if(tb_button==GLUT_LEFT_BUTTON)
			_twist(dx,dy);
		tb_prevPos[0]=tb_currPos[0];
		tb_prevPos[1]=tb_currPos[1];
	}
protected:
   GLint tb_button;
	unsigned char tb_key;
   GLboolean tb_tracking;
   GLboolean tb_moving;
   GLfloat tb_currPos[2];
   GLfloat tb_prevPos[2];
   GLfloat tb_transform[16];
   GLfloat tb_accuTransform[16];
	GLfloat tb_aspectRatio;
   GLuint tb_width;
   GLuint tb_height;
   GLfloat tb_angle;
   GLfloat tb_axis[3];
	GLfloat tb_max[3],tb_min[3];
	void _rotateview(float dx,float dy){
		float mat[16];
		float mag=sqrtf(dx*dx+dy*dy);
		tb_angle=mag*300.0f;
		glGetFloatv(GL_MODELVIEW_MATRIX,(GLfloat *)mat);
		tb_axis[0]=mat[0]*dy+mat[1]*dx;
		tb_axis[1]=mat[4]*dy+mat[5]*dx;
		tb_axis[2]=mat[8]*dy+mat[9]*dx;
		_atRotate(tb_axis[0],tb_axis[1],tb_axis[2],tb_angle);
	}
	void _twist(float dx,float dy){
		float mat[16];
		float mag=sqrtf(dx*dx+dy*dy);
		float Theta=atan2f(tb_prevPos[1]-0.5,tb_prevPos[0]-0.5);
		float newTheta=atan2f(tb_currPos[1]-0.5,tb_currPos[0]-0.5);
		tb_angle=(180/PI)*(Theta-newTheta);
		glGetFloatv(GL_MODELVIEW_MATRIX,(GLfloat *)mat);
		_atRotate(mat[2],mat[6],mat[10],tb_angle);
	}
	void _zoom(float dy){
		float viewlength=sqrtf(pow(tb_accuTransform[14]+initZ,2));
		float d=(viewlength*(1/(1+dy*2)))-viewlength;
		for(int i=0;i<16;i++)
			tb_transform[i]=0.0f;
		tb_transform[0]=tb_transform[5]=tb_transform[10]=tb_transform[15]=1.0f;
		tb_transform[14]=d;
	}
	void _pan(float dx,float dy){
		float mag=sqrtf(dx*dx+dy*dy);
		float d=sqrtf(pow(tb_accuTransform[14]+initZ,2));
		float Theta=(FOVy/2)*(PI/180);
		float b=2*d*tan(Theta);
		float dwx=dx*b*tb_aspectRatio;
		float dwy=-dy*b;
		for(int i=0;i<16;i++)
			tb_transform[i]=0.0f;
		tb_transform[0]=tb_transform[5]=tb_transform[10]=tb_transform[15]=1.0f;
		tb_transform[12]=dwx;
		tb_transform[13]=dwy;
	}
	void _atRotate(float x,float y,float z,float angle){
		float rad=(float)(angle*PI/180);
		float cosAng=(float)(cos(rad));
		float sinAng=(float)(sin(rad));
		float denom=sqrtf(x*x+y*y+z*z);
		if(denom!=0.0f){
			x/=denom;
			y/=denom;
			z/=denom;
		}
		for(int i=0;i<16;i++)
			tb_transform[i]=0.0f;
		tb_transform[0]=(x*x)+(cosAng*(1-(x*x)));
		tb_transform[1]=(x*y)-(cosAng*(x*y))+(sinAng*z);
		tb_transform[2]=(x*z)-(cosAng*(x*z))-(sinAng*y);
		tb_transform[4]=(x*y)-(cosAng*(x*y))-(sinAng*z);
		tb_transform[5]=(y*y)+(cosAng*(1-(y*y)));
		tb_transform[6]=(y*z)-(cosAng*(y*z))+(sinAng*x);
		tb_transform[8]=(x*z)-(cosAng*(x*z))+(sinAng*y);
		tb_transform[9]=(y*z)-(cosAng*(y*z))-(sinAng*x);
		tb_transform[10]=(z*z)+(cosAng*(1-(z*z)));
		tb_transform[15]=1.0f;
	}
};

#endif