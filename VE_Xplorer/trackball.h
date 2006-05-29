#ifndef TRACKBALL_H
#define TRACKBALL_H

#include <gmtl/Matrix.h>

using namespace gmtl;

const double PI=3.14159265358979323846;
const float FOVy=52.0f;

class Trackball{
public:
	Trackball();
	~Trackball();
	//void Print(Matrix44f mat);
	void Init();
	void Matrix();
	void Reshape(unsigned int width,unsigned int height);
	void Keyboard(int key);
	void Mouse(int button,int state,int x,int y);
	void Motion(int x,int y);

protected:
   bool tb_tracking;
   bool tb_moving;
   float tb_currPos[2];
   float tb_prevPos[2];
	float tb_aspectRatio;
   unsigned int tb_width;
   unsigned int tb_height;
	int tb_key;
	int tb_button;
   float tb_angle;
   float tb_axis[3];
	float tb_max[3];
	float tb_min[3];
	Matrix44f tb_transform;
	Matrix44f tb_accuTransform;
	Matrix44f WorldMatrix;

	void ResetTransforms();
	//void Scale(int key);
	void RotateView(float dx,float dy);
	void Twist(float dx,float dy);
	void Zoom(float dy);
	void Pan(float dx,float dy);
	void Rotate(float x,float y,float z,float angle);
};

#endif