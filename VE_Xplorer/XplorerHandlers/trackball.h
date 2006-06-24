#ifndef TRACKBALL_H
#define TRACKBALL_H

#include <gmtl/Matrix.h>

using namespace gmtl;

const double OneEightyDivPI=57.29577951;
const double PIDivOneEighty=.0174532925;

class Trackball{
public:
	Trackball();
	~Trackball();
	void Init();
	void Matrix();
	void Reshape(unsigned int width,unsigned int height);
	void SetFOVy(float _top,float _bottom,float _near);
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
	float tb_FOVy;
	int tb_key;
	int tb_button;
   float tb_angle;
   float tb_axis[3];
	float tb_max[3];
	float tb_min[3];
	Matrix44f tb_transform;
	Matrix44f tb_accuTransform;

	void ResetTransforms();
	void RotateView(float dx,float dy);
	void Twist(float dx,float dy);
	void Zoom(float dy);
	void Pan(float dx,float dy);
	void Rotate(float x,float y,float z,float angle);
};
#endif
