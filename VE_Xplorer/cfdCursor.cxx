/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2004 by Iowa State University
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
 * File:          $RCSfile: cfdCursor.cxx,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "cfdCursor.h"
#include "cfdEnum.h"
#include "cfdDCS.h"
#include "cfdGeode.h"
#include "cfdCommandArray.h"
#include "cfdGroup.h"
#include "cfdDataSet.h"

#include <vtkPolyData.h>
#include <vtkPolyDataSource.h>
#include <vtkCubeSource.h>
#include <vtkGlyph3D.h>
#include <vtkLineSource.h>
#include <vtkPlaneSource.h>
#include <vtkPointSource.h>
#include <vtkPolyDataNormals.h>
#include <vtkSphereSource.h>
#include <vtkActor.h>
#include <vtkPolyDataMapper.h>
#include <vtkProperty.h>

#define CURSOR_DIST 2.0f
#define BOX_LENGTH 2.0f

#include <vpr/Util/Debug.h>
#include <gmtl/Matrix.h>
#include <gmtl/Xforms.h>
#include <gmtl/Vec.h>
#include <gmtl/VecOps.h>
#include <vrj/Draw/Pf/PfUtil.h>
using namespace gmtl; //added by Gengxun
//using namespace vrj;

cfdCursor::cfdCursor( vtkPolyData * arrow, cfdDCS *worldDCS, cfdGroup* rootNode )
{
   this->arrow = arrow;
   this->worldDCS = worldDCS;
   this->activeDataSetDCS = NULL;
   _rootNode = rootNode;

   // get scale factors of the worldDCS...
   Matrix44f mat;
   mat = this->worldDCS->GetMat();

   Vec3f r;
   
   for ( unsigned int i = 0; i < 3; i++ )
      r[ i ] = mat[ 0 ][ i ];
   float xscale = length( r );

   for ( unsigned int i = 0; i < 3; i++ )
      r[ i ] = mat[ 1 ][ i ];
   float yscale = length( r );

   for ( unsigned int i = 0; i < 3; i++ )
      r[ i ] = mat[ 2 ][ i ];
   float zscale = length( r );

   
   vprDEBUG(vprDBG_ALL,1) << "cfdCursor: scale = "
                          << xscale << " : " << yscale << " : "  << zscale
                          << std::endl << vprDEBUG_FLUSH;

   this->sphereSrc = vtkSphereSource::New();
   this->sphereNorm = vtkPolyDataNormals::New();
   this->sphereMapper = vtkPolyDataMapper::New();
   this->sphereActor = vtkActor::New();

   this->cubeSrc = vtkCubeSource::New();
   this->cubeMapper = vtkPolyDataMapper::New();
   this->cubeActor = vtkActor::New();

   this->planeSrc       = vtkPlaneSource::New();
   this->planeActorS    = vtkActor::New();
   this->planeMapperS   = vtkPolyDataMapper::New();
   this->sphereGlyph    = vtkGlyph3D::New();
   this->planeSphereS   = vtkSphereSource::New();

   this->lineSrc     = vtkLineSource::New();
   this->lineMapper  = vtkPolyDataMapper::New();
   this->lineActor   = vtkActor::New();
   this->lineSphere  = vtkSphereSource::New();
   this->lineGlyph   = vtkGlyph3D::New();

   this->arrowPlaneS    = vtkPlaneSource::New();
   this->arrowMapperS   = vtkPolyDataMapper::New();
   this->arrowActorS    = vtkActor::New();
   this->arrowGlyphS    = vtkGlyph3D::New();

   this->cursorGeode = new cfdGeode;
   this->cursorDCS = new cfdDCS;
   float tempArray[ 3 ];
   tempArray[ 0 ] = xscale;
   tempArray[ 1 ] = yscale;
   tempArray[ 2 ] = zscale;

   this->cursorDCS->SetScaleArray( tempArray );

   this->pReso = 2;  // Set the number of x-y subdivisions in the plane
   this->last_pReso = this->pReso;

   this->pSize = 0.1f;
   this->last_pSize = this->pSize;

   this->last_direction = XPLANE;
   this->last_cursor_type = XPLANE;
}

cfdCursor::~cfdCursor()
{
   this->sphereSrc->Delete();
   this->sphereNorm->Delete();
   this->sphereMapper->Delete();
   this->sphereActor->Delete();

   this->cubeSrc->Delete();
   this->cubeMapper->Delete();
   this->cubeActor->Delete();

   this->planeSrc->Delete();
   this->planeActorS->Delete();
   this->planeMapperS->Delete();
   this->sphereGlyph->Delete();
   this->planeSphereS->Delete();

   this->lineSrc->Delete();
   this->lineMapper->Delete();
   this->lineActor->Delete();
   this->lineSphere->Delete();
   this->lineGlyph->Delete();

   this->arrowPlaneS->Delete();
   this->arrowMapperS->Delete();
   this->arrowActorS->Delete();
   this->arrowGlyphS->Delete();

   delete this->cursorGeode;
   delete this->cursorDCS;
}

void cfdCursor::Initialize( double x[3], double v[3] )
{
   for ( int i=0; i<3; i++ )
   {
      this->loc[i] = x[i];
      this->dir[i] = v[i];
   }

   this->BuildSphere();

   this->BuildArrowSource();

   this->BuildCube();

   this->BuildLineSource();

   this->BuildPlaneSource();

   this->cursorGeode->TranslateTocfdGeode( this->sphereActor );
   this->cursorDCS->AddChild( (cfdSceneNode*) this->cursorGeode );
}


void cfdCursor::BuildSphere()
{
   // Case 0 -- single point with sphere polygon.

   //   Building the sphere source.
   this->sphereSrc->SetRadius( 0.05f );
   this->sphereSrc->SetCenter( 0.0f, 0.0f, 0.0f );
   this->sphereSrc->Update();

   this->sphereNorm->SetInput( this->sphereSrc->GetOutput() );
   this->sphereNorm->Update();

   this->sphereMapper->SetInput( this->sphereNorm->GetOutput() );
   this->sphereMapper->Update();

   this->sphereActor->SetMapper( this->sphereMapper );
   this->sphereActor->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
}

void cfdCursor::BuildPlaneSource()
{
   // Create x, y, z plane source
   this->planeSrc->SetOrigin( 0.0f, -this->pSize, -this->pSize);
   this->planeSrc->SetPoint1( 0.0f, this->pSize, -this->pSize );
   this->planeSrc->SetPoint2( 0.0f, -this->pSize, this->pSize );
   this->planeSrc->SetResolution( this->pReso, this->pReso );

   this->planeSphereS->SetRadius( 0.05f );
   this->planeSphereS->Update();

   this->sphereGlyph->SetSource( this->planeSphereS->GetOutput() );
   this->sphereGlyph->SetInput( this->planeSrc->GetOutput() );
   this->sphereGlyph->Update();

   this->planeMapperS->SetInput( this->sphereGlyph->GetOutput() );
   this->planeMapperS->Update();

   this->planeActorS->SetMapper( this->planeMapperS );
   this->planeActorS->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
}

void cfdCursor::BuildArrowSource()
{
   // Create x, y, z plane source
   this->arrowPlaneS->SetOrigin( 0.0f, -this->pSize, -this->pSize);
   this->arrowPlaneS->SetPoint1( 0.0f, this->pSize, -this->pSize );
   this->arrowPlaneS->SetPoint2( 0.0f, -this->pSize, this->pSize );
   this->arrowPlaneS->SetResolution( this->pReso, this->pReso );
   this->arrowPlaneS->Update();

   this->arrowGlyphS->SetSource( this->arrow );
   this->arrowGlyphS->SetInput( this->arrowPlaneS->GetOutput() );
   this->arrowGlyphS->SetScaleFactor( 0.8 );
   this->arrowGlyphS->SetVectorModeToUseNormal();
   this->arrowGlyphS->SetScaleModeToDataScalingOff();
   this->arrowGlyphS->Update();

   this->arrowMapperS->SetInput( this->arrowGlyphS->GetOutput() );
   this->arrowMapperS->Update();

   this->arrowActorS->SetMapper( this->arrowMapperS );
   this->arrowActorS->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
}

//add for box cursor
void cfdCursor::BuildCube( void )
{
   // build the Cube.
   this->cubeSrc->SetXLength( BOX_LENGTH );
   this->cubeSrc->SetYLength( BOX_LENGTH );
   this->cubeSrc->SetZLength( BOX_LENGTH );
   //this->cubeSrc->SetCenter(pos[0],pos[1],pos[2]);

   //this->sphereNorm->SetInput( this->cubeSrc->GetOutput() );
   this->cubeMapper->SetInput( this->cubeSrc->GetOutput() );
   this->cubeActor->SetMapper( this->cubeMapper );
   this->cubeActor->GetProperty()->SetColor( 0.0f, 0.0f, 1.0f );
   this->cubeActor->GetProperty()->SetOpacity( 0.5f );
}
//add end

void cfdCursor::BuildLineSource( void )
{
   // build the Line Source
   //this->lineSrc->SetPoint1( this->pos[0], this->pos[1], this->pos[2] );
   //this->lineSrc->SetPoint2( this->pos[0], (this->pos[1] + 2), this->pos[2] );
   this->lineSrc->SetPoint1( -this->pSize, 0.0f, 0.0f );
   this->lineSrc->SetPoint2( +this->pSize, 0.0f, 0.0f );
   this->lineSrc->SetResolution( this->pReso );

   this->lineSphere->SetRadius( 0.05f );
   this->lineSphere->Update();

   this->lineGlyph->SetSource( this->lineSphere->GetOutput() );
   this->lineGlyph->SetInput( this->lineSrc->GetOutput() );
   this->lineGlyph->Update();

   this->lineMapper->SetInput( this->lineGlyph->GetOutput() );
   this->lineMapper->Update();

   this->lineActor->SetMapper( this->lineMapper );
   this->lineActor->GetProperty()->SetColor( 1.0f, 0.5f, 0.15f );
}

void cfdCursor::UpdateSphere( void )
{
   this->sphereSrc->SetCenter( 0.0f, 0.0f, 0.0f );
   this->sphereSrc->Update();
   this->cursorGeode->TranslateTocfdGeode( this->sphereActor );
}

void cfdCursor::UpdateArrowSource( void )
{
   //cout << " updating arrow source " << endl;
   //if ( this->last_pSize != this->pSize )
   {
      this->arrowPlaneS->SetOrigin( 0.0f, -this->pSize, -this->pSize);
      this->arrowPlaneS->SetPoint1( 0.0f, this->pSize, -this->pSize );
      this->arrowPlaneS->SetPoint2( 0.0f, -this->pSize, this->pSize );
   }

   //if ( this->last_pReso != this->pReso )
   {
      this->arrowPlaneS->SetResolution( this->pReso, this->pReso );
   }

   this->arrowPlaneS->SetNormal( this->dir );
   this->arrowPlaneS->Update();

   this->cursorGeode->TranslateTocfdGeode( this->arrowActorS );
}

//add for box cursor
void cfdCursor::UpdateCube( void )
{
   vprDEBUG(vprDBG_ALL, 1) << " updating cube source "
                           << std::endl << vprDEBUG_FLUSH;

   this->cursorGeode->TranslateTocfdGeode( this->cubeActor );
}
//add end

void cfdCursor::UpdateLineSource( int direction )
{
   vprDEBUG(vprDBG_ALL, 1) << " updating line source "
                           << std::endl << vprDEBUG_FLUSH;

   if      ( direction == XLINE )
   {
      this->lineSrc->SetPoint1( -this->pSize, 0.0f, 0.0f );
      this->lineSrc->SetPoint2( +this->pSize, 0.0f, 0.0f );
   }
   else if ( direction == YLINE )
   {
      this->lineSrc->SetPoint1( 0.0f, -this->pSize, 0.0f );
      this->lineSrc->SetPoint2( 0.0f, +this->pSize, 0.0f );
   }
   else if ( direction == ZLINE )
   {
      this->lineSrc->SetPoint1( 0.0f, 0.0f, -this->pSize );
      this->lineSrc->SetPoint2( 0.0f, 0.0f, +this->pSize );
   }

   //if ( this->last_pReso != this->pReso )
   {
      this->lineSrc->SetResolution( this->pReso );
   }

   this->lineSrc->Update();

   this->cursorGeode->TranslateTocfdGeode( this->lineActor );
}

void cfdCursor::UpdatePlaneSource( int i )
{
   vprDEBUG(vprDBG_ALL, 1) << " updating plane source " << i
                           << std::endl << vprDEBUG_FLUSH;

   //if ( this->last_pSize != this->pSize )
   {
      this->planeSrc->SetOrigin( 0.0f, -this->pSize, -this->pSize);
      this->planeSrc->SetPoint1( 0.0f, this->pSize, -this->pSize );
      this->planeSrc->SetPoint2( 0.0f, -this->pSize, this->pSize );
   }

   //if ( this->last_pReso != this->pReso )
   {
      this->planeSrc->SetResolution( this->pReso, this->pReso );
   }

   //if ( this->last_direction != i )
   {
      if ( i == XPLANE )
      {
         this->planeSrc->SetNormal( 1.0f, 0.0f, 0.0f );
      }
      else if ( i == YPLANE )
      {
         this->planeSrc->SetNormal( 0.0f, 1.0f, 0.0f );
      }
      else if ( i == ZPLANE )
      {
         this->planeSrc->SetNormal( 0.0f, .0f, 1.0f );
      }
      this->last_direction = i;
   }

   vprDEBUG(vprDBG_ALL, 1) << " updating plane source " << last_direction
                           << std::endl << vprDEBUG_FLUSH;
   this->planeSrc->Update();
   this->cursorGeode->TranslateTocfdGeode( this->planeActorS );
}

void cfdCursor::Update( int t, double x[3], double v[3], double wx[3] )
{
   int i;

   for( i=0; i<3; i++ )
   {
      this->loc[i] = x[i];
      this->dir[i] = v[i];
      this->pos[i] = this->loc[i] + CURSOR_DIST*this->dir[i];
      this->pos_c[i] = this->pos[i] + wx[i]; //compensate for world translation
   }
   vprDEBUG(vprDBG_ALL, 3) <<"wx:"<<wx[0]<<","<<wx[1]<<","<<wx[2]
                           << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 3) <<"pos:"<<pos[0]<<","<<pos[1]<<","<<pos[2]
                           << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 3) <<"pos_c:"<<pos_c[0]<<","<<pos_c[1]<<","<<pos_c[2]
                           << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 3) << this->last_cursor_type << " : " << t
                           << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 3) << this->last_pReso << " : " << this->pReso
                           << std::endl << vprDEBUG_FLUSH;
   vprDEBUG(vprDBG_ALL, 3) << this->last_pSize << " : " << this->pSize
                           << std::endl << vprDEBUG_FLUSH;

   if ( this->last_cursor_type != t ||
         this->last_pReso != this->pReso ||
         this->last_pSize != this->pSize )
   {
      switch( t )
      {
         case XPLANE:
            this->UpdatePlaneSource( XPLANE );
            break;

         case YPLANE:
            this->UpdatePlaneSource( YPLANE );
            break;

         case ZPLANE:
            this->UpdatePlaneSource( ZPLANE );
            break;

         case SPHERE:
            this->UpdateSphere();
            break;

         case ARROW:
            this->UpdateArrowSource();
            break;

         case CUBE:
            this->UpdateCube();
            break;

         case XLINE:
            this->UpdateLineSource( XLINE );
            break;

         case YLINE:
            this->UpdateLineSource( YLINE );
            break;

         case ZLINE:
            this->UpdateLineSource( ZLINE );
            break;

         default:
            break;
      }
      this->last_cursor_type = t;
      this->last_pReso = this->pReso;
      this->last_pSize = this->pSize;
   }

   this->SetTranslation();
}


cfdDCS* cfdCursor::GetcfdDCS()
{
   return this->cursorDCS;
}

//add for box cursor
void cfdCursor::getExtent( double boxExtent[6] )
{
   boxExtent[0] = pos_c[0] - BOX_LENGTH/2;
   boxExtent[1] = pos_c[0] + BOX_LENGTH/2;
   boxExtent[2] = pos_c[1] - BOX_LENGTH/2;
   boxExtent[3] = pos_c[1] + BOX_LENGTH/2;
   boxExtent[4] = pos_c[2] - BOX_LENGTH/2;
   boxExtent[5] = pos_c[2] + BOX_LENGTH/2;
}

vtkCubeSource * cfdCursor::getBox()
{
   return this->cubeSrc;
}
//add end


void cfdCursor::SetPlaneSize( float size )
{
   vprDEBUG(vprDBG_ALL, 1) << "Setting plane size : " << size
                           << std::endl << vprDEBUG_FLUSH;

   this->last_pSize = this->pSize;
   this->pSize = size;
}


void cfdCursor::SetPlaneReso( int size )
{
   this->last_pReso = this->pReso;
   this->pReso = size;
}


void cfdCursor::GetPlaneSize( float &size )
{
   size = this->pSize;
}


float cfdCursor::GetPlaneSize()
{
   return this->pSize;
}


void cfdCursor::GetPlaneReso( int &size )
{
   size = this->pReso;
}


int cfdCursor::GetPlaneReso()
{
   return this->pReso;
}

vtkPolyDataSource * cfdCursor::GetSourcePoints( int cursorId )
{
   this->GetLocalLocationVector();

   switch( cursorId )
   {
      case XPLANE:
         this->planeSrc->SetCenter( this->localLocation );
         this->planeSrc->Update();
         return (vtkPolyDataSource *)this->planeSrc->GetOutput();
         break;

      case YPLANE:
         this->planeSrc->SetCenter( this->localLocation );
         this->planeSrc->Update();
         return (vtkPolyDataSource *)this->planeSrc->GetOutput();
         break;

      case ZPLANE:
         this->planeSrc->SetCenter( this->localLocation );
         this->planeSrc->Update();
         return (vtkPolyDataSource *)this->planeSrc->GetOutput();
         break;

      case SPHERE:
         this->sphereSrc->SetCenter( this->localLocation );
         this->sphereSrc->Update();
         return (vtkPolyDataSource *)this->sphereSrc->GetOutput();
         break;

      case ARROW:
         this->arrowPlaneS->SetCenter( this->localLocation );
         this->arrowPlaneS->Update();
         return (vtkPolyDataSource *)this->arrowPlaneS->GetOutput();
         break;

      case CUBE:
         this->cubeSrc->SetCenter( this->localLocation );
         this->cubeSrc->Update();
         return (vtkPolyDataSource *)this->cubeSrc->GetOutput();
         break;

      case XLINE:
         this->lineSrc->SetPoint1( this->localLocation[0]-this->pSize,
                                   this->localLocation[1],
                                   this->localLocation[2] );
         this->lineSrc->SetPoint2( this->localLocation[0]+this->pSize,
                                   this->localLocation[1],
                                   this->localLocation[2] );
         this->lineSrc->Update();
         return (vtkPolyDataSource *)this->lineSrc->GetOutput();
         break;

      case YLINE:
         this->lineSrc->SetPoint1( this->localLocation[0],
                                   this->localLocation[1]-this->pSize,
                                   this->localLocation[2] );
         this->lineSrc->SetPoint2( this->localLocation[0],
                                   this->localLocation[1]+this->pSize,
                                   this->localLocation[2] );
         this->lineSrc->Update();
         return (vtkPolyDataSource *)this->lineSrc->GetOutput();
         break;

      case ZLINE:
         this->lineSrc->SetPoint1( this->localLocation[0],
                                   this->localLocation[1],
                                   this->localLocation[2]-this->pSize );
         this->lineSrc->SetPoint2( this->localLocation[0],
                                   this->localLocation[1],
                                   this->localLocation[2]+this->pSize );
         this->lineSrc->Update();
         return (vtkPolyDataSource *)this->lineSrc->GetOutput();
         break;

      default:
         return NULL;
         break;
   }
}

void cfdCursor::SetTranslation( void )
{
   // Called constantly to place the active cursor in the virtual world.
   // Need to orient the cursor properly for the current active dataset.
   // Extract rotations from totalMat, where
   // totalMat = activeDataSetMat * pfWorldMat;

   float loc_f[ 3 ];
   loc_f[ 0 ] = this->loc[ 0 ];
   loc_f[ 1 ] = this->loc[ 1 ];
   loc_f[ 2 ] = this->loc[ 2 ];
   //*loc_f = *loc;
   this->cursorDCS->SetTranslationArray( loc_f );

   // get pfMatrix of worldDCS
   Matrix44f pfWorldMat;
   pfWorldMat = this->worldDCS->GetMat();

   Matrix44f totalMat;
   if ( this->activeDataSetDCS )
   {
      // get pfMatrix of activeDataSetDCS
      Matrix44f activeDataSetMat;
      activeDataSetMat = this->activeDataSetDCS->GetMat();

      totalMat = activeDataSetMat * pfWorldMat;
   }
   else
      totalMat = pfWorldMat;

   //totalMat.getOrthoCoord( this->coord );
   //this->cursorDCS->SetRotationArray( this->coord->hpr );
   this->cursorDCS->SetRotationMatrix( totalMat );
}

void cfdCursor::GetLocalLocationVector( void )
{
   // Called when the local (dataset) cursor coordinates are needed.
   // Such as in the need for seedpoint coordinates.
   // Transform from the cursor's global (root) description to local.
   // local_vec = [activeDataSet mat]^(-1) * [world mat]^(-1) * global_vec

   // verify that there is a dataset DCS ...
   if ( this->activeDataSetDCS  == NULL )
   {
      std::cerr << "ERROR: don't have an activeDataSetDCS" << std::endl;
      return;
   }

   vprDEBUG(vprDBG_ALL,1) << "global position rel to performer: "
                          << this->loc[ 0 ] << " : "
                          << this->loc[ 1 ] << " : "
                          << this->loc[ 2 ] << std::endl << vprDEBUG_FLUSH;

   // store the global location in a performer vector...
   gmtl::Vec4f jugglerVec;
   jugglerVec[ 0 ] = this->loc[ 0 ];
   jugglerVec[ 1 ] = this->loc[ 1 ];
   jugglerVec[ 2 ] = this->loc[ 2 ];
   jugglerVec[ 3 ] = 1.0f;

   // get juggler Matrix of worldDCS
   Matrix44f pfWorldMat;
   pfWorldMat = this->worldDCS->GetMat();

   // invert the worldDCS matrix...
   Matrix44f pfWorldMatInv;
   gmtl::invert( pfWorldMatInv, pfWorldMat );

   // compute local_vec = [world matrix]^(-1) * global_vec
   Vec4f localVector;
   localVector = pfWorldMatInv * jugglerVec;

   // get juggler Matrix of activeDataSetDCS
   Matrix44f activeDataSetMat;
   activeDataSetMat = this->activeDataSetDCS->GetMat();

   // invert activeDataSetDCS
   Matrix44f activeDataSetMatInv;
   gmtl::invert( activeDataSetMatInv, activeDataSetMat );

   // compute new_local_vec = [activeDataSet Matrix]^(-1) * local_vec
   Vec4f pfLocXX;
   pfLocXX = activeDataSetMatInv * localVector;

   // Set class member location
   this->localLocation[ 0 ] =  (double)pfLocXX[ 0 ];
   this->localLocation[ 1 ] =  (double)pfLocXX[ 1 ];
   this->localLocation[ 2 ] =  (double)pfLocXX[ 2 ];

   vprDEBUG(vprDBG_ALL,1) << " NOIE : local position rel to Juggler: "
                          << this->localLocation[ 0 ] << " : "
                          << this->localLocation[ 1 ] << " : "
                          << this->localLocation[ 2 ]
                          << std::endl << vprDEBUG_FLUSH;
}

void cfdCursor::SetActiveDataSetDCS( cfdDCS* myDCS )
{
   this->activeDataSetDCS = myDCS;
}

void cfdCursor::SetActiveDataSet( cfdDataSet* input )
{
   _activeDataSet = input;
}

// compare VjObs_i commandArray with its child's value
bool cfdCursor::CheckCommandId( cfdCommandArray* commandArray )
{
   if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) == CHANGE_STREAMLINE_CURSOR )
   {
      vprDEBUG(vprDBG_ALL,1) << "this->id = " << commandArray->GetCommandValue( cfdCommandArray::CFD_ID ) 
                << ", this->min = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ) 
                << ", this->max = " << commandArray->GetCommandValue( cfdCommandArray::CFD_MAX )
                << std::endl << vprDEBUG_FLUSH;

      if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == NO_CURSOR )
      {
         vprDEBUG(vprDBG_ALL,1) 
           << "removing cursor with cursor->GetpfDCS() = "
           << this->GetcfdDCS() << std::endl << vprDEBUG_FLUSH;

         this->cursorId = NONE;
         if ( this->_rootNode->SearchChild( this->GetcfdDCS() ) >= 0 )
            this->_rootNode->RemoveChild( this->GetcfdDCS() );
      }
      else
      {
         if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == POINT_CURSOR )
            this->cursorId = SPHERE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == X_PLANE_CURSOR )
            this->cursorId = XPLANE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == Y_PLANE_CURSOR )
            this->cursorId = YPLANE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == Z_PLANE_CURSOR )
            this->cursorId = ZPLANE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == X_LINE_CURSOR )
            this->cursorId = XLINE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == Y_LINE_CURSOR )
            this->cursorId = YLINE;
         else if ( commandArray->GetCommandValue( cfdCommandArray::CFD_ISO_VALUE ) == Z_LINE_CURSOR )
            this->cursorId = ZLINE;
         else
         {
            vprDEBUG(vprDBG_ALL,0) 
              << "ERROR: Unknown cursorId -- Setting cursor to XPLANE"
              << std::endl << vprDEBUG_FLUSH;

            this->cursorId = XPLANE;
         }
         
         // fix this don't know what it used for
         // look in old in cfdApp.cxx
         // this->chgMod = true;

         vprDEBUG(vprDBG_ALL,1) 
           << "adding cursor with cursor->GetpfDCS() = "
           << this->GetcfdDCS() << std::endl << vprDEBUG_FLUSH;

         // if disconnected from scene graph, add
         if ( this->_rootNode->SearchChild( this->GetcfdDCS() ) < 0 )
         {
            this->_rootNode->AddChild( this->GetcfdDCS() );
         }
      }

      if ( _activeDataSet != NULL )
         this->SetActiveDataSetDCS( _activeDataSet->GetDCS() );

      //if ( this->cursorId != NONE && this->cursorId != SPHERE && this->cursorId != CUBE )
      if ( this->cursorId != NONE && this->cursorId != SPHERE )
      {
         this->SetPlaneReso( commandArray->GetCommandValue( cfdCommandArray::CFD_MIN ) ); 

         // convert size percentage (0-100) request to plane size
         // fix this
         //this->SetPlaneSize( commandArray->GetCommandValue( cfdCommandArray::CFD_MAX ) * 0.5 * 0.01 * 
         //                cfdObjects::GetActiveMeshedVolume()->GetLength() );
      }
      return true;
   }

/*
      else  // changed from wand
      {
         if ( this->menuB == true )
         { 
            // If menu is up, remove menu and laser wand and cycle through cursor modes
            if ( this->cursorId == NONE )
            {
               this->cursorId = XPLANE;
               this->rootNode->removeChild( this->menu->GetpfDCS() );
               this->rootNode->removeChild( this->laser->GetpfDCS() );
               this->rootNode->addChild( this->cursor->GetpfDCS() );
            }
            else if ( this->cursorId == XPLANE )
               this->cursorId = YPLANE;
            else if ( this->cursorId == YPLANE )
               this->cursorId = ZPLANE;
            else if ( this->cursorId == ZPLANE )
               this->cursorId = SPHERE;
            else if ( this->cursorId == SPHERE )
               this->cursorId = ARROW;
            else if ( this->cursorId == ARROW )
               this->cursorId = XLINE;
            else if ( this->cursorId == XLINE )
               this->cursorId = YLINE;
            else if ( this->cursorId == YLINE )
               this->cursorId = ZLINE;
            else if ( this->cursorId == ZLINE )
               this->cursorId = CUBE;
            else if ( this->cursorId == CUBE )
            {
               // If on last cursor type, remove menu and reset cursor to off
               this->cursorId = NONE;
               this->menuB = false;
            }
         }
         else
         { 
            // If menu is down, add menu and laser wand, and keep cursor off
            this->rootNode->addChild( this->menu->GetpfDCS() );
            this->rootNode->addChild( this->laser->GetpfDCS() );
            this->menuB = true;
            this->rootNode->removeChild( this->cursor->GetpfDCS() );
         }
      }
*/

   return true;
}

// in future, multi-threaded apps will make a copy of VjObs_i commandArray
void cfdCursor::UpdateCommand()
{
   cerr << "doing nothing in cfdCursor::UpdateCommand()" << endl;

}
