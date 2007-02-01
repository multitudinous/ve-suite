//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
//Don't implement this class; it is handled through DeviceHandler
//!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

#ifndef TRACKBALL_H
#define TRACKBALL_H
/*!\file Trackball.h
Trackball API
*/
/*!\class VE_Xplorer::Trackball
* 
*/
#include <gmtl/Matrix.h>

#include "VE_Installer/include/VEConfig.h"

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Trackball
{
public:
   Trackball();
	~Trackball();

   void Animate(bool animate);
	void ProcessTrackballEvents();
	void Reshape(unsigned int width,unsigned int height);
	void SetFOVy(float _top,float _bottom,float _near);
   void ResetTransforms();
   void FrameAll();

private:
   void UpdateTransform();
   void Keyboard(int key);
	void Mouse(int button,int state,int x,int y);
	void Motion(int x,int y);

   bool tb_moving;
   bool tb_animate;

   unsigned int tb_width;
   unsigned int tb_height;

   int tb_key;
	int tb_button;

   float tb_currPos[2];
   float tb_prevPos[2];
   float tb_magnitude;
   float tb_sensitivity;
	float tb_aspectRatio;
   float tb_FOVyRatio;
	float tb_FOVy;

   //Is of form [row][column]
   gmtl::Matrix44f tb_transform;
   gmtl::Matrix44f tb_accuTransform;

	void RotateView(float dx,float dy);
	void Twist(float dx,float dy);
	void Zoom(float dy);
	void Pan(float dx,float dy);
	void Rotate(float x,float y,float z,float angle);
};
}

#endif
