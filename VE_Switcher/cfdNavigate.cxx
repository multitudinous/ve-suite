/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2005 by Iowa State University
 *
 * Original Development Team:
 *   - ISU's Thermal Systems Virtual Engineering Group,
 *     Headed by Kenneth Mark Bryden, Ph.D., www.vrac.iastate.edu/~kmbryden
 *   - Reaction Engineering International, www.reaction-eng.com
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
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 *
 * -----------------------------------------------------------------
 * File:          $RCSfile: cfdNavigate.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdNavigate.h"

#include <gmtl/Xforms.h>
#include <gmtl/Generate.h> // for setTrans

using namespace gmtl;
using namespace gadget;

cfdNavigate::cfdNavigate( )
{
   wand.init("VJWand");
}

cfdNavigate::~cfdNavigate( )
{
}

void cfdNavigate::ResetCoordsToZero( )
{
   this->worldLoc[0] = this->worldLoc[1] = this->worldLoc[2] = 0.0f;
}

void cfdNavigate::Initialize( float delta )
{
   this->cursorLen = 2.0f;
   this->dObj = delta;// Default should be 0.05f
   this->UpdateDir( );
   this->UpdateLoc( );

   this->worldLoc[0] = this->worldLoc[1] = this->worldLoc[2] = 0.0f;

   for ( int i=0; i<3; i++ )
   {
      this->cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}

void cfdNavigate::GetDirection( float xyzV[3] )
{
   this->GetDirection( xyzV[0], xyzV[1], xyzV[2] );
}

void cfdNavigate::GetDirection( float &xV, float &yV, float &zV )
{
   this->UpdateDir( );

   xV = this->dir[0];
   yV = this->dir[1];
   zV = this->dir[2];
}

float * cfdNavigate::GetDirection( )
{
   this->UpdateDir( );

   return this->dir;
}

void cfdNavigate::GetLocation( float xyzL[3] )
{
   this->GetLocation( xyzL[0], xyzL[1], xyzL[2] );
}

void cfdNavigate::GetLocation( float &xL, float &yL, float &zL )
{
   this->UpdateLoc( );

   xL = this->loc[0];
   yL = this->loc[1];
   zL = this->loc[2];
}

float * cfdNavigate::GetLocation( )
{
   this->UpdateLoc( );

   return this->loc;
}

void cfdNavigate::GetObjLocation( float xyzO[3] )
{
   this->GetObjLocation( xyzO[0], xyzO[1], xyzO[2] );
}

void cfdNavigate::GetObjLocation( float &xO, float &yO, float &zO )
{
   xO = this->objLoc[0];
   yO = this->objLoc[1];
   zO = this->objLoc[2];
}

float * cfdNavigate::GetObjLocation( )
{
   return this->objLoc;
}

float * cfdNavigate::GetCurObjLocation( )
{
   this->CursorTranslate(); 
   return this->objLoc;
}

void cfdNavigate::GetCursorLocation( float xyzC[3] )
{
   this->GetCursorLocation( xyzC[0], xyzC[1], xyzC[2] );
}

void cfdNavigate::GetCursorLocation( float &xC, float &yC, float &zC )
{
   this->CursorTranslate( );
   xC = this->cursorLoc[0];
   yC = this->cursorLoc[1];
   zC = this->cursorLoc[2];
}

float * cfdNavigate::GetCursorLocation( )
{
   this->CursorTranslate( );
   return this->cursorLoc;
}

void cfdNavigate::GetWorldLocation( float xyzS[3] )
{
   this->GetWorldLocation( xyzS[0], xyzS[1], xyzS[2] );
}

void cfdNavigate::GetWorldLocation( float &xS, float &yS, float &zS )
{
   xS = this->worldLoc[0];
   yS = this->worldLoc[1];
   zS = this->worldLoc[2];
}

float * cfdNavigate::GetWorldLocation( )
{
   return this->worldLoc;
}

void cfdNavigate::UpdateDir( )      //Changed by Gengxun
{ 
   // get the normalized direction relative to the juggler frame
   vjVec.set( 0.0f, 0.0f, -1.0f );
   vjMat = wand->getData( );
   vjVec = gmtl::xform( vjVec,vjMat,vjVec);
   gmtl::normalize(vjVec);

   // transform from juggler to performer...
   dir[0] =  vjVec[0];
   dir[1] = -vjVec[2];
   dir[2] =  vjVec[1];
}

void cfdNavigate::UpdateLoc( )      //Changed by Gengxun
{
   // get the location relative to the juggler frame
   gmtl::Vec3f loc_temp;
   gmtl::setTrans(loc_temp,vjMat);

   // transform from juggler to performer...
   loc[0] =  loc_temp[0];
   loc[1] = -loc_temp[2];
   loc[2] =  loc_temp[1];
}  

/*
cfdNavigate::TransformJugglerToPerformer()
{
}
*/

void cfdNavigate::FwdTranslate( )
{
   this->UpdateDir( );

   for ( int i=0; i<3; i++ )
   {
      // Update the translation movement for the objects
      // How much object should move
      this->worldLoc[i] += this->dir[i]*this->dObj;

      // How much the cursor movement are needed to trace back
      // to the object after each movement of the object
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}

void cfdNavigate::AftTranslate( )
{
   this->UpdateDir( );

   for ( int i=0; i<3; i++ )
   {
      // Update the translation movement for the objects
      // How much object should move
      this->worldLoc[i] -= this->dir[i]*this->dObj;

      // How much the cursor movement are needed to trace back
      // to the object after each movement of the object
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}

void cfdNavigate::CursorTranslate( )
{
   this->UpdateDir( );
   this->UpdateLoc( );

   for ( int i=0; i<3; i++ )
   {
      this->cursorLoc[i] = this->loc[i] + this->dir[i]*this->cursorLen;
      this->objLoc[i] = this->cursorLoc[i] + this->worldLoc[i];
   }
}
