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

#include <Performer/pr/pfGeoSet.h>
#include <Performer/pf/pfGeode.h>
#include <Performer/pf/pfDCS.h>
#include <Performer/pr/pfLinMath.h>
#include "vtkActorToPF.h"

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
#include <vrj/Draw/Pf/PfUtil.h>
//using namespace gmtl; //added by Gengxun
//using namespace vrj;

cfdCursor::cfdCursor( vtkPolyData * arrow, pfDCS *worldDCS )
{
   this->arrow = arrow;
   this->worldDCS = worldDCS;
   this->activeDataSetDCS = NULL;

   // get scale factors of the worldDCS...
   pfMatrix mat;
   this->worldDCS->getMat( mat );

   pfVec3 r;
   mat.getRow(0, r);
   float xscale = r.length();
   mat.getRow(1, r);
   float yscale = r.length();
   mat.getRow(2, r);
   float zscale = r.length();
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

   this->cursorGeode = new pfGeode;
   this->cursorDCS = new pfDCS;
   this->cursorDCS->setScale( xscale,  yscale, zscale );

   this->pReso = 2;  // Set the number of x-y subdivisions in the plane
   this->last_pReso = this->pReso;

   this->pSize = 0.1f;
   this->last_pSize = this->pSize;

   this->last_direction = XPLANE;
   this->last_cursor_type = XPLANE;

   this->coord = new pfCoord();
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

   pfDelete( this->cursorGeode );
   pfDelete( this->cursorDCS );

   pfDelete( this->coord );
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

   vtkActorToPF( this->sphereActor, this->cursorGeode );
   this->cursorDCS->addChild( this->cursorGeode );
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
   vtkActorToPF( this->sphereActor, this->cursorGeode );
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

   vtkActorToPF( this->arrowActorS, this->cursorGeode );
}

//add for box cursor
void cfdCursor::UpdateCube( void )
{
   vprDEBUG(vprDBG_ALL, 1) << " updating cube source "
                           << std::endl << vprDEBUG_FLUSH;

   vtkActorToPF( this->cubeActor, this->cursorGeode );
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

   vtkActorToPF( this->lineActor, this->cursorGeode );
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
   vtkActorToPF( this->planeActorS, this->cursorGeode );
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
      int numGSets = this->cursorGeode->getNumGSets();
      vprDEBUG(vprDBG_ALL, 1) << "cfdCursor numGSets: " << numGSets
                              << std::endl << vprDEBUG_FLUSH;

      for ( i=0; i<numGSets; i++ )
      {
         pfGeoSet *g  = this->cursorGeode->getGSet( 0 );
         this->cursorGeode->removeGSet( g );
         pfDelete( g );
      }
      vprDEBUG(vprDBG_ALL, 1) << " Cursor : " << t
                              << std::endl << vprDEBUG_FLUSH;
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


pfDCS * cfdCursor::GetpfDCS()
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

void writePfMatrix( char * text, pfMatrix pfMat )
{
   std::cout << text << std::endl;
   for (int i=0; i<4; i++)
   {
      for (int j=0; j<4; j++)
      {
         std::cout << pfMat[i][j] << "\t";
      }
      std::cout << std::endl;
   }
   std::cout << std::endl;
}

void cfdCursor::SetTranslation( void )
{
   // Called constantly to place the active cursor in the virtual world.
   // Need to orient the cursor properly for the current active dataset.
   // Extract rotations from totalMat, where
   // totalMat = activeDataSetMat * pfWorldMat;

   this->cursorDCS->setTrans( this->loc[ 0 ], this->loc[ 1 ], this->loc[ 2 ] );

   // get pfMatrix of worldDCS
   pfMatrix pfWorldMat;
   this->worldDCS->getMat( pfWorldMat );

   pfMatrix totalMat;
   if ( this->activeDataSetDCS )
   {
      // get pfMatrix of activeDataSetDCS
      pfMatrix activeDataSetMat;
      this->activeDataSetDCS->getMat( activeDataSetMat );

      totalMat = activeDataSetMat * pfWorldMat;
   }
   else
      totalMat = pfWorldMat;

   totalMat.getOrthoCoord( this->coord );
   this->cursorDCS->setRot( this->coord->hpr[ 0 ],
                            this->coord->hpr[ 1 ],
                            this->coord->hpr[ 2 ] );
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
   pfVec3 pfLoc = pfVec3( this->loc[ 0 ], this->loc[ 1 ], this->loc[ 2 ] );

   // get pfMatrix of worldDCS
   pfMatrix pfWorldMat;
   this->worldDCS->getMat( pfWorldMat );

   //writePfMatrix( "pfWorldMat =", pfWorldMat );

   // invert the worldDCS matrix...
   pfMatrix pfWorldMatInv;
   pfWorldMatInv.invertFull( pfWorldMat );

   // compute local_vec = [world matrix]^(-1) * global_vec
   pfVec3 pfLocX;
   pfLocX.fullXformPt( pfLoc, pfWorldMatInv );

   // get pfMatrix of activeDataSetDCS
   pfMatrix activeDataSetMat;
   this->activeDataSetDCS->getMat( activeDataSetMat );

   //writePfMatrix( "activeDataSetMat =", activeDataSetMat );

   // invert activeDataSetDCS
   pfMatrix activeDataSetMatInv;
   activeDataSetMatInv.invertFull( activeDataSetMat );

   // compute new_local_vec = [activeDataSet Matrix]^(-1) * local_vec
   pfVec3 pfLocXX;
   pfLocXX.fullXformPt( pfLocX, activeDataSetMatInv );

   this->localLocation[ 0 ] = (double)pfLocXX[ 0 ];
   this->localLocation[ 1 ] = (double)pfLocXX[ 1 ];
   this->localLocation[ 2 ] = (double)pfLocXX[ 2 ];

   vprDEBUG(vprDBG_ALL,1) << " local position rel to performer: "
                          << this->localLocation[ 0 ] << " : "
                          << this->localLocation[ 1 ] << " : "
                          << this->localLocation[ 2 ]
                          << std::endl << vprDEBUG_FLUSH;
}

void cfdCursor::SetActiveDataSetDCS( pfDCS * myDCS )
{
   this->activeDataSetDCS = myDCS;
}

