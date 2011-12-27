/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VR Juggler is (C) Copyright 1998-2010 by Iowa State University
 *
 * Original Authors:
 *   Allen Bierbaum, Christopher Just,
 *   Patrick Hartling, Kevin Meinert,
 *   Carolina Cruz-Neira, Albert Baker
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/

#include <vrj/vrjConfig.h>

#include <math.h>

#if defined(__APPLE__)
#  include <OpenGL/gl.h>
//#  include <OpenGL/glu.h>
#else
#  include <GL/gl.h>
//#  include <GL/glu.h>
#endif

#include <gmtl/Matrix.h>
#include <gmtl/Generate.h>
#include <gmtl/Vec.h>
#include <gmtl/Point.h>
#include <gmtl/Misc/MatrixConvert.h>

#include "simpleApp.h"

using namespace gmtl;
using namespace vrj;


// Clears the viewport.  Put the call to glClear() in this
// method so that this application will work with configurations
// using two or more viewports per display window.
void simpleApp::bufferPreDraw()
{
   glClearColor(0.0f, 0.0f, 0.0f, 0.0f);
   glClear(GL_COLOR_BUFFER_BIT);
}

//----------------------------------------------
//  Draw the scene.
//
// - Draws a box and a coordinate axis set on the box
// - Offset and rotate the box by a matrix that we create
//----------------------------------------------
void simpleApp::draw()
{
   glClear(GL_DEPTH_BUFFER_BIT);

   // --- Setup for drawing --- //
   glMatrixMode(GL_MODELVIEW);

   // -- Get Wand matrix --- //
   Matrix44f wand_matrix;
   wand_matrix = mWand->getData();

   // -- Create box offset matrix -- //
   Matrix44f box_offset;
   const EulerAngleXYZf euler_ang(Math::deg2Rad(-90.0f), Math::deg2Rad(0.0f),
                                  Math::deg2Rad(0.0f));
   box_offset = makeRot<Matrix44f>(euler_ang);
   setTrans(box_offset, Vec3f(0.0f, 1.0f, 0.0f));

   // --- Create a color for the wand --- //
   float wand_color[3];
   wand_color[0] = wand_color[1] = wand_color[2] = 0.7f;

   glPushMatrix();

      // --- Draw the box --- //
      glPushMatrix();
         glMultMatrixf(box_offset.mData);    // Push the wand offset matrix on the stack
         glColor3fv(wand_color);
         glScalef(0.5f, 0.5f, 0.5f);
         drawCube();
      glPopMatrix();

      // Draw a coordinate axis "on" the box
      glLineWidth(5.0f);
      glDisable(GL_LIGHTING);
      glPushMatrix();
         glMultMatrixf(box_offset.mData);    // Goto wand position

         Vec3f x_axis(7.0f,0.0f,0.0f);
         Vec3f y_axis(0.0f, 7.0f, 0.0f);
         Vec3f z_axis(0.0f, 0.0f, 7.0f);
         Vec3f origin(0.0f, 0.0f, 0.0f);

         glBegin(GL_LINES);
            glColor3f(1.0f, 0.0f, 0.0f);
            glVertex3fv(origin.mData);
            glVertex3fv(x_axis.mData);

            glColor3f(0.0f, 1.0f, 0.0f);
            glVertex3fv(origin.mData);
            glVertex3fv(y_axis.mData);

            glColor3f(0.0f, 0.0f, 1.0f);
            glVertex3fv(origin.mData);
            glVertex3fv(z_axis.mData);
         glEnd();
      glPopMatrix();
      glEnable(GL_LIGHTING);
   glPopMatrix();
}



void simpleApp::initGLState()
{
   GLfloat light0_ambient[] = { 0.1f,  0.1f,  0.1f,  1.0f};
   GLfloat light0_diffuse[] = { 0.8f,  0.8f,  0.8f,  1.0f};
   GLfloat light0_specular[] = { 1.0f,  1.0f,  1.0f,  1.0f};
   GLfloat light0_position[] = {0.0f, 0.75f, 0.75f, 0.0f};

   GLfloat mat_ambient[] = { 0.7f, 0.7f,  0.7f, 1.0f };
   GLfloat mat_diffuse[] = { 1.0f,  0.5f,  0.8f, 1.0f };
   GLfloat mat_specular[] = { 1.0,  1.0,  1.0,  1.0};
   GLfloat mat_shininess[] = { 50.0};
//   GLfloat mat_emission[] = { 1.0,  1.0,  1.0,  1.0};
   GLfloat no_mat[] = { 0.0,  0.0,  0.0,  1.0};

   glLightfv(GL_LIGHT0, GL_AMBIENT,  light0_ambient);
   glLightfv(GL_LIGHT0, GL_DIFFUSE,  light0_diffuse);
   glLightfv(GL_LIGHT0, GL_SPECULAR,  light0_specular);
   glLightfv(GL_LIGHT0, GL_POSITION,  light0_position);

   glMaterialfv( GL_FRONT, GL_AMBIENT, mat_ambient );
   glMaterialfv( GL_FRONT,  GL_DIFFUSE, mat_diffuse );
   glMaterialfv( GL_FRONT, GL_SPECULAR, mat_specular );
   glMaterialfv( GL_FRONT,  GL_SHININESS, mat_shininess );
   glMaterialfv( GL_FRONT,  GL_EMISSION, no_mat);

   glEnable(GL_DEPTH_TEST);
   glEnable(GL_NORMALIZE);
   glEnable(GL_LIGHTING);
   glEnable(GL_LIGHT0);
   glEnable(GL_COLOR_MATERIAL);
   glShadeModel(GL_SMOOTH);
}

/// Utility function for drawing a cube
void drawbox(GLdouble x0, GLdouble x1, GLdouble y0, GLdouble y1,
             GLdouble z0, GLdouble z1, GLenum type)
{
   static GLdouble n[6][3] = {
      {-1.0, 0.0, 0.0}, {0.0, 1.0, 0.0}, {1.0, 0.0, 0.0},
      {0.0, -1.0, 0.0}, {0.0, 0.0, 1.0}, {0.0, 0.0, -1.0}
   };
   static GLint faces[6][4] = {
      { 0, 1, 2, 3}, { 3, 2, 6, 7}, { 7, 6, 5, 4},
      { 4, 5, 1, 0}, { 5, 6, 2, 1}, { 7, 4, 0, 3}
   };
   GLdouble v[8][3], tmp;
   GLint i;

   if (x0 > x1)
   {
      tmp = x0; x0 = x1; x1 = tmp;
   }
   if (y0 > y1)
   {
      tmp = y0; y0 = y1; y1 = tmp;
   }
   if (z0 > z1)
   {
      tmp = z0; z0 = z1; z1 = tmp;
   }
   v[0][0] = v[1][0] = v[2][0] = v[3][0] = x0;
   v[4][0] = v[5][0] = v[6][0] = v[7][0] = x1;
   v[0][1] = v[1][1] = v[4][1] = v[5][1] = y0;
   v[2][1] = v[3][1] = v[6][1] = v[7][1] = y1;
   v[0][2] = v[3][2] = v[4][2] = v[7][2] = z0;
   v[1][2] = v[2][2] = v[5][2] = v[6][2] = z1;

   for (i = 0; i < 6; i++)
   {
      glBegin(type);
         glNormal3dv(&n[i][0]);
         glVertex3dv(&v[faces[i][0]][0]);
         glNormal3dv(&n[i][0]);
         glVertex3dv(&v[faces[i][1]][0]);
         glNormal3dv(&n[i][0]);
         glVertex3dv(&v[faces[i][2]][0]);
         glNormal3dv(&n[i][0]);
         glVertex3dv(&v[faces[i][3]][0]);
      glEnd();
   }
}

void simpleApp::CalculateRegistrationVariables()
{
    gadget::PositionInterface  headposDevice;
    headposDevice.init( "VJHead" );
    gadget::PositionInterface  wandposDevice;
    wandposDevice.init( "VJWand" );
    gadget::PositionInterface  pointerposDevice;
    pointerposDevice.init( "VESPointer" );
    
    //Get the bird data from
    //VJHead
    gmtl::Matrix44d headMat = gmtl::convertTo< double >( headposDevice->getData() );
    gmtl::Point4d headPoint = gmtl::makeTrans< gmtl::Point4d >( headMat );
    //VJWand
    gmtl::Matrix44d wandMat = gmtl::convertTo< double >( wandposDevice->getData() );
    gmtl::Point4d wandPoint = gmtl::makeTrans< gmtl::Point4d >( wandMat );
    //VJPointer
    gmtl::Matrix44d pointerMat = gmtl::convertTo< double >( pointerposDevice->getData() );
    gmtl::Point4d pointerPoint = gmtl::makeTrans< gmtl::Point4d >( pointerMat );

    //Create the centroid for the triangle
    gmtl::Point3d centroid;
    centroid.set( 
        (headPoint[ 0 ] + wandPoint[ 0 ] + pointerPoint[ 0 ])/3.0, 
        (headPoint[ 1 ] + wandPoint[ 1 ] + pointerPoint[ 1 ])/3.0,
        (headPoint[ 2 ] + wandPoint[ 2 ] + pointerPoint[ 2 ])/3.0 );
        
    //Create vector from the origin of the triangle to the fron bird
    gmtl::Vec3d forwardVec;
    forwardVec.set( headPoint[ 0 ] - centroid[ 0 ], 
                   -(headPoint[ 2 ] - centroid[ 2 ]), 
                   headPoint[ 1 ] - centroid[ 1 ] );
    gmtl::normalize( forwardVec );

    //Cross the front vector with the vector from one of the rear bird corners
    gmtl::Vec3d rearVec;
    rearVec.set( wandPoint[ 0 ] - centroid[ 0 ], 
                -(wandPoint[ 2 ] - centroid[ 2 ]), 
                wandPoint[ 1 ] - centroid[ 1 ] );
    gmtl::normalize( rearVec );

    //Create the up vector
    gmtl::Vec3d upVec;
    upVec.set( (rearVec[1]*forwardVec[2]) - (rearVec[2]*forwardVec[1]),
             (rearVec[2]*forwardVec[0]) - (rearVec[0]*forwardVec[2]),
             (rearVec[0]*forwardVec[1]) - (rearVec[1]*forwardVec[0]) );
    gmtl::normalize( upVec );

    gmtl::Vec3d rightVec;
    rightVec.set( (forwardVec[1]*upVec[2]) - (forwardVec[2]*upVec[1]),
                 (forwardVec[2]*upVec[0]) - (forwardVec[0]*upVec[2]),
                 (forwardVec[0]*upVec[1]) - (forwardVec[1]*upVec[0]) );
    gmtl::normalize( rightVec );
    
    //GMTL is columan major order so this is why the data is laid out in columns
    //http://www.fastgraph.com/makegames/3drotation/
    gmtl::Matrix44d transMat;
    transMat.set( rightVec[ 0 ], forwardVec[ 0 ], upVec[ 0 ],  centroid[ 0 ],
                  rightVec[ 1 ], forwardVec[ 1 ], upVec[ 1 ], -centroid[ 2 ],
                  rightVec[ 2 ], forwardVec[ 2 ], upVec[ 2 ],  centroid[ 1 ],
                             0.,         0.,              0.,             1. );
    
    //Get the SIP offsets from the birds to the centroid
    //X is to the rear, y is up, z is to the left
    //fbirdd = (sip[0]*u.mm-1048.1*u.mm,sip[1]*u.mm+686.8*u.mm,sip[2]*u.mm+13.3*u.mm)
	//lrbirdd = (sip[0]*u.mm+597.8*u.mm,sip[1]*u.mm+792.5*u.mm,sip[2]*u.mm+421.4*u.mm)
	//rrbirdd = (sip[0]*u.mm+600.9*u.mm,sip[1]*u.mm+792.4*u.mm,sip[2]*u.mm-421.4*u.mm)
    double mm2ft = 0.0032808;

    double frontBirdX = -1048.1 * mm2ft;
    double frontBirdY = 686.8 * mm2ft;
    double frontBirdZ = 13.3 * mm2ft;

    double leftRearBirdX = 597.8 * mm2ft;
    double leftRearBirdY = 792.5 * mm2ft;
    double leftRearBirdZ = 421.4 * mm2ft;

    double rightRearBirdX = 600.9 * mm2ft;
    double rightRearBirdY = 792.4 * mm2ft;
    double rightRearBirdZ = -421.4 * mm2ft;
    //These coords are transformed into ves coord space
    //x = -z
    //y = -x
    //z = y
    gmtl::Point3d sipOffSetFrontBird;
    sipOffSetFrontBird.set( -frontBirdZ, -frontBirdX, frontBirdY );

    gmtl::Point3d sipOffSetLeftRearBird;
    sipOffSetLeftRearBird.set( -leftRearBirdZ, -leftRearBirdX, leftRearBirdY );
    
    gmtl::Point3d sipOffSetRightRearBird;
    sipOffSetRightRearBird.set( -rightRearBirdZ, -rightRearBirdX, rightRearBirdY );
    
    gmtl::Point3d measuredSIPCentroid;
    measuredSIPCentroid.set( 
        (sipOffSetFrontBird[ 0 ] + sipOffSetLeftRearBird[ 0 ] + sipOffSetRightRearBird[ 0 ])/3.0, 
        (sipOffSetFrontBird[ 1 ] + sipOffSetLeftRearBird[ 1 ] + sipOffSetRightRearBird[ 1 ])/3.0,
        (sipOffSetFrontBird[ 2 ] + sipOffSetLeftRearBird[ 2 ] + sipOffSetRightRearBird[ 2 ])/3.0 );
    
    gmtl::Matrix44d measuredSIPCentroidMat = 
        gmtl::makeTrans< gmtl::Matrix44d >( measuredSIPCentroid );
    
    //Now we convert the sip matrix back through the transform mat to move it 
    //to the VR Juggler coord
    gmtl::Matrix44d registerMat = transMat * measuredSIPCentroidMat;
    gmtl::invert( registerMat );
}