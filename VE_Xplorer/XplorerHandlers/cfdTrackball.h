#ifndef CFD_TRACKBALL_H
#define CFD_TRACKBALL_H

#include <gmtl/Matrix.h>

#include "VE_Installer/include/VEConfig.h"

using namespace gmtl;

namespace VE_Xplorer
{
	class VE_XPLORER_EXPORTS cfdTrackball
   {
      public:
	      cfdTrackball();
	      ~cfdTrackball();
	      void Init();
         void Update();
	      void Matrix();
	      void Reshape(unsigned int width,unsigned int height);
	      void SetFOVy(float _top,float _bottom,float _near);
	      void Mouse(int button,int state,int x,int y);
	      void Motion(int x,int y);

      protected:
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
	      float tb_max[3];
	      float tb_min[3];
         Matrix44f tb_transform;
         Matrix44f tb_accuTransform;

	      void RotateView(float dx,float dy);
	      void Twist(float dx,float dy);
	      void Zoom(float dy);
	      void Pan(float dx,float dy);
	      void Rotate(float x,float y,float z,float angle);
   };
}

#endif
