#include "VE_Xplorer/XplorerHandlers/KeyboardMouse.h"

#include "VE_Xplorer/SceneGraph/cfdPfSceneManagement.h"
#include "VE_Xplorer/SceneGraph/PhysicsSimulator.h"

#include <gadget/Type/KeyboardMouse/KeyEvent.h>
#include <gadget/Type/KeyboardMouse/MouseEvent.h>

#include <LinearMath/btVector3.h>

// --- OSG Stuff --- //
#include <osg/Group>
#include <osg/BoundingSphere>
#include <osg/BoundingBox>
#include <osg/CameraNode>
#include <osg/LineSegment>

//C/C++ Libraries
#include <iostream>
#include <cmath>

using namespace VE_Xplorer;

const double OneEightyDivPI = 57.29577951;
const double PIDivOneEighty = .0174532925;
const float offset = 0.5f;

////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::KeyboardMouse()
:
tb_moving( false ),
tb_animate( false ),
tb_width( 1 ),
tb_height( 1 ),
tb_key( -1 ),
tb_button( -1 ),
tb_magnitude( 0.0f ),
tb_sensitivity( 1.0e-06 ),
tb_aspectRatio( 0.0f ),
tb_FOVyRatio( 0.0f ),
tb_FOVy( 0.0f )
{
	mKeyboard.init( "VJKeyboard" );

	tb_currPos[0] = 0.0f;
	tb_currPos[1] = 0.0f;
	tb_prevPos[0] = 0.0f;
	tb_prevPos[1] = 0.0f;

	gmtl::identity( tb_transform );
	gmtl::identity( tb_accuTransform );
}
////////////////////////////////////////////////////////////////////////////////
KeyboardMouse::~KeyboardMouse()
{
   ;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateNavigation()
{
	gadget::KeyboardMouse::EventQueue evt_queue = mKeyboard->getEventQueue();
   gadget::KeyboardMouse::EventQueue::iterator i;

   if( evt_queue.empty() )
	{
      return;
   }

	//Get the current matrix
   tb_accuTransform = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();

   for( i = evt_queue.begin(); i != evt_queue.end(); i++ )
	{
		const gadget::EventType type = (*i)->type();

		if( type == gadget::KeyPressEvent )
		{
         gadget::KeyEventPtr key_evt = boost::dynamic_pointer_cast< gadget::KeyEvent >(*i);
			
			this->TBKeyboard( key_evt->getKey() );
		}

      /*
      //Use this call if you want to hold a key for it to be active
      else if( type == gadget::KeyReleaseEvent )
		{
         _key = -1;
      }
      */

	   else if( type == gadget::MouseButtonPressEvent )
		{
         gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast<gadget::MouseEvent>(*i);

			this->TBMouse( mouse_evt->getButton(), 1, mouse_evt->getX(), mouse_evt->getY() );

			//If in animation mode, stop the animation with mouse press event
			if( tb_animate )
			{
				gmtl::identity( tb_transform );
				tb_transform[0][3] = tb_transform[1][3] = tb_transform[2][3] = 0.0f;
			}
		}

		else if( type == gadget::MouseButtonReleaseEvent )
		{
         gadget::MouseEventPtr mouse_evt=boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
			
			this->TBMouse( mouse_evt->getButton(), 0, mouse_evt->getX(), mouse_evt->getY() );
		}

		else if( type == gadget::MouseMoveEvent )
		{
         gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
         
			this->TBMotion( mouse_evt->getX(), mouse_evt->getY() );
		}
	}

   //Split apart the current matrix into rotation and translation parts
   gmtl::Matrix44f accuRotation;
   gmtl::Matrix44f matrix;

   for( int i = 0; i < 3; i++ )
	{
      //Get the current rotation matrix
      accuRotation[i][0] = tb_accuTransform[i][0];
      accuRotation[i][1] = tb_accuTransform[i][1];
      accuRotation[i][2] = tb_accuTransform[i][2];

      //Get the current translation matrix
      matrix[i][3] = tb_accuTransform[i][3];
   }

   //Multiply by the transform and then by the rotation
   matrix *= tb_transform;
   matrix *= accuRotation;

   //Set the current matrix
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetMat( matrix );

   //If not in animation mode, reset the transform
   if( !tb_animate )
	{
      gmtl::identity( tb_transform );
      tb_transform[0][3] = tb_transform[1][3] = tb_transform[2][3] = 0.0f;
   }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelection()
{
   osg::ref_ptr< osg::LineSegment > ls = new osg::LineSegment;

   //Virtual function defined in Device.cxx
	this->Traverse();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Animate( bool animate )
{
   tb_animate = animate;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Reshape( unsigned int width, unsigned int height )
{
	tb_width = width;
	tb_height = height;

   tb_aspectRatio = (float)width/(float)height;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetFOVy( float _top, float _bottom, float _near )
{
	float topAngle=(OneEightyDivPI)*atan(_top/_near);
   float tempDiv=fabs(_bottom)/_near;
	float bottomAngle=(OneEightyDivPI)*atan(tempDiv);

   tb_FOVyRatio=topAngle/bottomAngle;
	tb_FOVy=topAngle+bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBKeyboard( int key )
{
   //If "r" is pressed
   if(key==35)
	{
      ResetTransforms();
   }

   //If "f" is pressed
   else if(key==23)
	{
      FrameAll();
   }
   
   key=-1;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBMouse( int button, int state, int x, int y )
{
	tb_button=button;

	if(state==1)
	{
		tb_currPos[0]=(float)x/(float)tb_width;
		tb_currPos[1]=(float)y/(float)tb_height;
		tb_prevPos[0]=(float)x/(float)tb_width;
		tb_prevPos[1]=(float)y/(float)tb_height;
		tb_moving=true;

      /*
      //If physics is enabled, shoot a box on right-mouse click
      if(button==51 && VE_SceneGraph::PhysicsSimulator::instance()->GetPhysicsState())
		{
			btVector3 destination( 0, 0, 0 );
			VE_SceneGraph::PhysicsSimulator::instance()->ShootBox( destination );
      }
      */
   }

   else if(state==0)
	{
		tb_moving=false;
   }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBMotion( int x, int y )
{
   if(!tb_moving)
	{
		return;
   }

	tb_currPos[0]=(float)x/(float)tb_width;
	tb_currPos[1]=(float)y/(float)tb_height;
	float dx=tb_currPos[0]-tb_prevPos[0];
	float dy=tb_currPos[1]-tb_prevPos[1];

	tb_magnitude=sqrtf(dx*dx+dy*dy);
   if(tb_magnitude<tb_sensitivity)
	{
      return;
   }

   if(tb_button==49&&(x>.1*tb_width&&x<.9*tb_width)&&(y>.1*tb_height&&y<.9*tb_height))
	{
   	RotateView(dx,dy);
   }

   else if(tb_button==51)
	{
		Zoom(dy);
   }

   else if(tb_button==50)
	{
      Pan(dx,dy);
   }

   else if(tb_button==49)
	{
		Twist(dx,dy);
   }

	tb_prevPos[0]=tb_currPos[0];
	tb_prevPos[1]=tb_currPos[1];
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetMat(identity(tb_accuTransform));
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
   osg::ref_ptr<osg::Group> root=new osg::Group;
   root->addChild(VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS());

   tb_accuTransform=VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();

   //Get the selected objects and expand by their bounding box
   osg::BoundingSphere bs;
   for(unsigned int i=0;i<root->getNumChildren();++i)
	{
      bs.expandBy(root->getChild(i)->getBound());
   }

   float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
   float x=bs.center().x();

   float y;
   if(tb_aspectRatio<=1.0f)
	{
      y=(bs.radius()/tan(Theta))*tb_aspectRatio;
   }

   else
	{
      y=(bs.radius()/tan(Theta));
   }

   float z=bs.center().z();

   if(bs.center().x()!=0)
	{
      tb_accuTransform[0][3]-=x;
   }
   
   tb_accuTransform[1][3]=y;
    
   if(bs.center().z()!=0)
	{
      tb_accuTransform[2][3]-=z;
   }

   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetMat(tb_accuTransform);
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RotateView( float dx, float dy )
{
   float angle=tb_magnitude*400.0f;

   gmtl::Matrix44f mat;
	identity(mat);
	float tb_axis[3];
	tb_axis[0]=mat[0][0]*dy+mat[2][0]*dx;
	tb_axis[1]=mat[0][1]*dy+mat[2][1]*dx;
	tb_axis[2]=mat[0][2]*dy+mat[2][2]*dx;
	Rotate(tb_axis[0],tb_axis[1],tb_axis[2],angle);
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist( float dx, float dy )
{
   gmtl::Matrix44f mat;
	identity(mat);
	float Theta=atan2f(tb_prevPos[0]-0.5,tb_prevPos[1]-0.5);
	float newTheta=atan2f(tb_currPos[0]-0.5,tb_currPos[1]-0.5);
	float angle=(OneEightyDivPI)*(Theta-newTheta);

	Rotate(mat[1][0],mat[1][1],mat[1][2],angle);
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( float dy )
{
	float viewlength=fabs(tb_accuTransform[1][3]);
	float d=(viewlength*(1/(1+dy*2)))-viewlength;

   //**********Temporary Fix**********//
	if((tb_accuTransform[1][3]>-offset)&&(tb_accuTransform[1][3]<offset))
	{
		if(tb_accuTransform[1][3]==0)
			tb_transform[1][3]=offset;
		else if(tb_accuTransform[1][3]>0)
			tb_transform[1][3]=-2*offset;
		else if(tb_accuTransform[1][3]<0)
			tb_transform[1][3]=2*offset;
	}

	else
	{
		tb_transform[1][3]=d;
	}
   //*********************************//

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( float dx, float dy )
{
   //**********Temporary Fix**********//
   if((tb_accuTransform[1][3]>-offset)&&(tb_accuTransform[1][3]<offset))
	{
		if(tb_accuTransform[1][3]==0)
			tb_transform[1][3]=offset;
		else if(tb_accuTransform[1][3]>0)
			tb_transform[1][3]=-2*offset;
		else if(tb_accuTransform[1][3]<0)
			tb_transform[1][3]=2*offset;
	}
   //*********************************//
	
   float d=tb_accuTransform[1][3];
	float Theta=(tb_FOVy*0.5f)*(PIDivOneEighty);
	float b=2*d*tan(Theta);
	float dwx=dx*b*tb_aspectRatio;
	float dwy=-dy*b;
	tb_transform[0][3]=dwx;
	tb_transform[2][3]=dwy;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( float x, float y, float z, float angle )
{
	float rad=(float)(angle*PIDivOneEighty);
	float cosAng=(float)(cos(rad));
	float sinAng=(float)(sin(rad));
	float denom=sqrtf(x*x+y*y+z*z);
	if(denom!=0.0f)
	{
		x/=denom;
		y/=denom;
		z/=denom;
	}

	zero(tb_transform);

	tb_transform[0][0]=(x*x)+(cosAng*(1-(x*x)));
	tb_transform[1][0]=(y*x)-(cosAng*   (y*x))+(sinAng*z);
	tb_transform[2][0]=(z*x)-(cosAng*   (z*x))-(sinAng*y);
	tb_transform[0][1]=(x*y)-(cosAng*   (x*y))-(sinAng*z);
	tb_transform[1][1]=(y*y)+(cosAng*(1-(y*y)));
	tb_transform[2][1]=(z*y)-(cosAng*   (z*y))+(sinAng*x);
	tb_transform[0][2]=(x*z)-(cosAng*   (x*z))+(sinAng*y);
	tb_transform[1][2]=(y*z)-(cosAng*   (y*z))-(sinAng*x);
	tb_transform[2][2]=(z*z)+(cosAng*(1-(z*z)));
	tb_transform[3][3]=1.0f;
}
////////////////////////////////////////////////////////////////////////////////
