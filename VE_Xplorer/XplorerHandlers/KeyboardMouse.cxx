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

key( -1 ),
button( -1 ),
state( -1 ),
x( 0 ),
y( 0 ),

width( 1 ),
height( 1 ),

aspect_ratio( 0.0f ),
fovy_ratio( 0.0f ),
fovy( 0.0f ),

tb_magnitude( 0.0f ),
tb_sensitivity( 1.0e-06 ),
tb_animate( false )

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
   this->ProcessKBEvents( 0 );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::UpdateSelection()
{
   this->ProcessKBEvents( 1 );

   osg::ref_ptr< osg::LineSegment > ls = new osg::LineSegment;

   //Virtual function defined in Device.cxx
	this->Traverse();
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ProcessKBEvents( int mode )
{
   gadget::KeyboardMouse::EventQueue evt_queue = mKeyboard->getEventQueue();
   gadget::KeyboardMouse::EventQueue::iterator i;

   if( evt_queue.empty() )
	{
      return;
   }

   //Navigation mode
   if( mode == 0 )
   {
	   //Get the current matrix
      tb_accuTransform = VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();
   }

   for( i = evt_queue.begin(); i != evt_queue.end(); i++ )
	{
		const gadget::EventType type = (*i)->type();

		if( type == gadget::KeyPressEvent )
		{
         gadget::KeyEventPtr key_evt = boost::dynamic_pointer_cast< gadget::KeyEvent >(*i);
         key = key_evt->getKey();
			
         //Navigation mode
         if( mode == 0 )
         {
			   this->TBKeyboard();
         }
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
         button = mouse_evt->getButton();
         state = 1;
         x = mouse_evt->getX();
         y = mouse_evt->getY();

         //Navigation mode
         if( mode == 0 )
         {
			   this->TBMouse();

			   //If in animation mode, stop the animation with mouse press event
			   if( tb_animate )
			   {
				   gmtl::identity( tb_transform );
				   tb_transform[0][3] = tb_transform[1][3] = tb_transform[2][3] = 0.0f;
			   }
         }
		}

		else if( type == gadget::MouseButtonReleaseEvent )
		{
         gadget::MouseEventPtr mouse_evt=boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
         button = mouse_evt->getButton();
         state = 0;
         x = mouse_evt->getX();
         y = mouse_evt->getY();
			
         //Navigation mode
         if( mode == 0 )
         {
			   this->TBMouse();
         }
		}

		else if( type == gadget::MouseMoveEvent )
		{
         gadget::MouseEventPtr mouse_evt = boost::dynamic_pointer_cast< gadget::MouseEvent >(*i);
         x = mouse_evt->getX();
         y = mouse_evt->getY();
         
         //Navigation mode
         if( mode == 0 )
         {
			   this->TBMotion();
         }
		}
	}

   //Navigation mode
   if( mode == 0 )
   {
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
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Animate( bool animate )
{
   tb_animate = animate;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Reshape( unsigned int w, unsigned int h )
{
	width = w;
	height = h;

   aspect_ratio = (float)width / (float)height;

//   std::cout<< aspect_ratio <<std::endl;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::SetFOVy( float t, float b, float n )
{
	float topAngle = OneEightyDivPI * atan( t / n );
   float tempDiv = fabs( b ) / n;
	float bottomAngle = OneEightyDivPI * atan( tempDiv );

   fovy_ratio = topAngle / bottomAngle;
	fovy = topAngle + bottomAngle;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBKeyboard()
{
   //If "r" is pressed
   if( key == 35 )
	{
      ResetTransforms();
   }

   //If "f" is pressed
   else if( key == 23 )
	{
      FrameAll();
   }
   
   //Reset key
   key = -1;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBMouse()
{

	if( state == 1 )
	{
		tb_currPos[0] = (float)x / (float)width;
		tb_currPos[1] = (float)y / (float)height;
		tb_prevPos[0] = (float)x / (float)width;
		tb_prevPos[1] = (float)y / (float)height;
   }

   else if( state == 0 )
	{
		return;
   }
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::TBMotion()
{
   if( !state )
	{
		return;
   }

	tb_currPos[0] = (float)x / (float)width;
	tb_currPos[1] = (float)y / (float)height;
	float dx = tb_currPos[0] - tb_prevPos[0];
	float dy = tb_currPos[1] - tb_prevPos[1];

	tb_magnitude = sqrtf( dx*dx + dy*dy );
   if( tb_magnitude < tb_sensitivity )
	{
      return;
   }

   if( button == 49 && ( x > 0.1*width && x < 0.9*width ) && ( y > 0.1*height && y < 0.9*height ) )
	{
   	RotateView( dx, dy );
   }

   else if( button == 51 )
	{
		Zoom( dy );
   }

   else if( button == 50 )
	{
      Pan( dx, dy );
   }

   else if( button == 49 )
	{
		Twist( dx, dy );
   }

	tb_prevPos[0] = tb_currPos[0];
	tb_prevPos[1] = tb_currPos[1];
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::ResetTransforms()
{
   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetMat( identity( tb_accuTransform ) );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::FrameAll()
{
   osg::ref_ptr< osg::Group > root = new osg::Group;
   root->addChild( VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS() );

   tb_accuTransform=VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->GetMat();

   //Get the selected objects and expand by their bounding box
   osg::BoundingSphere bs;
   for( unsigned int i = 0; i < root->getNumChildren(); i++ )
	{
      bs.expandBy( root->getChild(i)->getBound() );
   }

   float Theta = (fovy*0.5f)*(PIDivOneEighty);
   float x_val = bs.center().x();

   float y_val;
   if( aspect_ratio <= 1.0f )
	{
      y_val = (bs.radius()/tan(Theta))*aspect_ratio;
   }

   else
	{
      y_val = (bs.radius()/tan(Theta));
   }

   float z_val = bs.center().z();

   if( bs.center().x() != 0 )
	{
      tb_accuTransform[0][3] -= x_val;
   }
   
   tb_accuTransform[1][3] = y_val;
    
   if( bs.center().z() != 0 )
	{
      tb_accuTransform[2][3] -= z_val;
   }

   VE_SceneGraph::cfdPfSceneManagement::instance()->GetWorldDCS()->SetMat( tb_accuTransform );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::RotateView( float dx, float dy )
{
   float angle = tb_magnitude*400.0f;

   gmtl::Matrix44f mat;
	identity( mat );
	float tb_axis[3];
	tb_axis[0] = mat[0][0]*dy + mat[2][0]*dx;
	tb_axis[1] = mat[0][1]*dy + mat[2][1]*dx;
	tb_axis[2] = mat[0][2]*dy + mat[2][2]*dx;
	Rotate( tb_axis[0], tb_axis[1], tb_axis[2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Twist( float dx, float dy )
{
   gmtl::Matrix44f mat;
	identity( mat );
	float Theta = atan2f( tb_prevPos[0]-0.5, tb_prevPos[1]-0.5 );
	float newTheta = atan2f( tb_currPos[0]-0.5, tb_currPos[1]-0.5 );
	float angle = (OneEightyDivPI)*(Theta-newTheta);

	Rotate( mat[1][0], mat[1][1], mat[1][2], angle );
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Zoom( float dy )
{
	float viewlength=fabs( tb_accuTransform[1][3] );
	float d = (viewlength*(1/(1+dy*2)))-viewlength;

   //**********Temporary Fix**********//
	if( ( tb_accuTransform[1][3] > -offset ) && ( tb_accuTransform[1][3] < offset ) )
	{
		if( tb_accuTransform[1][3] == 0 )
      {
			tb_transform[1][3] = offset;
      }

		else if( tb_accuTransform[1][3] > 0 )
      {
			tb_transform[1][3] = -2*offset;
      }

		else if( tb_accuTransform[1][3] < 0 )
      {
			tb_transform[1][3] = 2*offset;
      }
	}

	else
	{
		tb_transform[1][3] = d;
	}
   //*********************************//

}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Pan( float dx, float dy )
{
   //**********Temporary Fix**********//
   if( ( tb_accuTransform[1][3] > -offset ) && ( tb_accuTransform[1][3] < offset ) )
	{
		if( tb_accuTransform[1][3] == 0 )
      {
			tb_transform[1][3] = offset;
      }

		else if( tb_accuTransform[1][3] > 0 )
      {
			tb_transform[1][3] = -2*offset;
      }

		else if( tb_accuTransform[1][3] < 0 )
      {
			tb_transform[1][3] = 2*offset;
      }
	}
   //*********************************//
	
   float d = tb_accuTransform[1][3];
	float Theta = (fovy*0.5f)*(PIDivOneEighty);
	float b = 2*d*tan(Theta);
	float dwx = dx*b*aspect_ratio;
	float dwy = -dy*b;
	tb_transform[0][3] = dwx;
	tb_transform[2][3] = dwy;
}
////////////////////////////////////////////////////////////////////////////////
void KeyboardMouse::Rotate( float x_val, float y_val, float z_val, float angle )
{
	float rad = (float)( angle*PIDivOneEighty );
	float cosAng = (float)( cos( rad ) );
	float sinAng = (float)( sin( rad ) );
	float denom = sqrtf( x_val*x_val + y_val*y_val + z_val*z_val );
	if( denom != 0.0f )
	{
		x_val /= denom;
		y_val /= denom;
		z_val /= denom;
	}

	zero( tb_transform );

	tb_transform[0][0] = (x_val*x_val) + (cosAng*(1-(x_val*x_val)));
	tb_transform[1][0] = (y_val*x_val) - (cosAng*   (y_val*x_val)) + (sinAng*z_val);
	tb_transform[2][0] = (z_val*x_val) - (cosAng*   (z_val*x_val)) - (sinAng*y_val);
	tb_transform[0][1] = (x_val*y_val) - (cosAng*   (x_val*y_val)) - (sinAng*z_val);
	tb_transform[1][1] = (y_val*y_val) + (cosAng*(1-(y_val*y_val)));
	tb_transform[2][1] = (z_val*y_val) - (cosAng*   (z_val*y_val)) + (sinAng*x_val);
	tb_transform[0][2] = (x_val*z_val) - (cosAng*   (x_val*z_val)) + (sinAng*y_val);
	tb_transform[1][2] = (y_val*z_val) - (cosAng*   (y_val*z_val)) - (sinAng*x_val);
	tb_transform[2][2] = (z_val*z_val) + (cosAng*(1-(z_val*z_val)));
	tb_transform[3][3] = 1.0f;
}
////////////////////////////////////////////////////////////////////////////////
