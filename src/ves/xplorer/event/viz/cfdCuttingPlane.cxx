/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2007 by Iowa State University
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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#include "VE_Xplorer/XplorerHandlers/cfdCuttingPlane.h"
#include "VE_Xplorer/XplorerHandlers/cfdDataSet.h"
#include <vtkPlane.h>
#include "VE_Xplorer/XplorerHandlers/cfdDebug.h"

using namespace VE_Xplorer;

cfdCuttingPlane::cfdCuttingPlane( const double bounds[6], const int xyz,
                                  const int numSteps )
{
   vprDEBUG(vesDBG,2) << "in cfdCuttingPlane constructor"
                          << std::endl << vprDEBUG_FLUSH;

   // set the boundary of current data set.
   this->SetBounds( bounds );

   this->type = xyz;

   vprDEBUG(vesDBG,1) << "this->type = " << this->type 
                          << std::endl << vprDEBUG_FLUSH;

   // specify the normal to sweep, the step-size, and the origin...
   if ( this->type == 0 )
   {
      this->normal[0] = 1.0f;
      this->normal[1] = 0.0f;
      this->normal[2] = 0.0f;
      this->dx = ( this->bd[1] - this->bd[0] ) / (float)numSteps;
      //this->origin[0] = this->bd[0];
      this->origin[0] = ( this->bd[1] + this->bd[0] ) / 2.0;
      this->origin[1] = ( this->bd[3] + this->bd[2] ) / 2.0;
      this->origin[2] = ( this->bd[5] + this->bd[4] ) / 2.0;
   }
   else if ( this->type == 1 )
   {
      this->normal[0] = 0.0f;
      this->normal[1] = 1.0f;
      this->normal[2] = 0.0f;
      this->dx = ( this->bd[3] - this->bd[2] ) / (float)numSteps;
      this->origin[0] = ( this->bd[1] + this->bd[0] ) / 2.0;
      //this->origin[1] = this->bd[2];
      this->origin[1] = ( this->bd[3] + this->bd[2] ) / 2.0;
      this->origin[2] = ( this->bd[5] + this->bd[4] ) / 2.0;
   }
   else if ( this->type == 2 )
   {
      this->normal[0] = 0.0f;
      this->normal[1] = 0.0f;
      this->normal[2] = 1.0f;
      this->dx = ( this->bd[5] - this->bd[4] ) / (float)numSteps;
      this->origin[0] = ( this->bd[1] + this->bd[0] ) / 2.0;
      this->origin[1] = ( this->bd[3] + this->bd[2] ) / 2.0;
      //this->origin[2] = this->bd[4];
      this->origin[2] = ( this->bd[5] + this->bd[4] ) / 2.0;
   }
   else
   {
      std::cerr << "in cfdCuttingPlane type WAS NOT 0, 1, or 2!" << std::endl;
      exit (1);
   }

   vprDEBUG(vesDBG, 1) << "this->origin = " << this->origin[0] << " : "
                           << this->origin[1] << " : " << this->origin[2]
                           << std::endl << vprDEBUG_FLUSH; 
   this->plane = vtkPlane::New( );
   this->plane->SetOrigin( this->origin );
   this->plane->SetNormal( this->normal );

   // reset the origin
   this->ResetOriginToLow();
}

cfdCuttingPlane::~cfdCuttingPlane( )
{
   //vprDEBUG(vesDBG,2) << "in cfdCuttingPlane destructor" 
    //                      << std::endl << vprDEBUG_FLUSH;
// Fix this mccdo
   this->plane->Delete();
}

void cfdCuttingPlane::SetBounds( const double bounds[6] )
{   
   for ( int i=0; i<6; i++ )
   {
      this->bd[i] = (double)bounds[i];
   }
}

vtkPlane * cfdCuttingPlane::GetPlane( )
{
   return this->plane;
}

void cfdCuttingPlane::Advance( int requestedValue )
{
   // with tablet, requestedValue will be an integer from 0-100
   // with old menu system, requestedValue will be 999
   if( requestedValue == 999 )
   {
      // advance the origin
      this->IncrementOrigin();
   } 
   else
   {
      // The jave slider bar returns integers 0-100 representing percentile.  
      this->ComputeOrigin( requestedValue );

      // if too low error will occur, so reset close to bottom of range
      if ( this->isAtStart() ) 
      {
         this->ResetOriginToLow();
      }
   }
   // if over the limit, reset close to bottom of range
   // if at the limit, reset close to end of range
   if      ( this->isPastEnd() ) this->ResetOriginToLow();
   else if ( this->isAtEnd()   ) this->ResetOriginToHigh();

   vprDEBUG(vesDBG, 0) << "this->origin[" << this->type << "] = " 
      << this->origin[this->type] << std::endl << vprDEBUG_FLUSH;

   this->plane->SetOrigin( this->origin );
   this->plane->Modified();
   //this->plane->Update();
}

void cfdCuttingPlane::GetOrigin( double Origin[ 3 ]  )
{
   Origin[0] = this->origin[0];
   Origin[1] = this->origin[1];
   Origin[2] = this->origin[2];
}

void cfdCuttingPlane::ComputeOrigin( int requestedValue )
{
   if ( requestedValue < 0 )   requestedValue = 0;
   if ( requestedValue > 100 ) requestedValue = 100;

   // bd is an array of 6 values xmin, xmax, ymin, ymax, zmin, zmax
   // bd is calculated from the raw cfdDataSet NOT from the precalc values
   // type is either 0,1,2 representing x,y,z
   this->origin[this->type] = this->bd[2*this->type] + 
                              ( requestedValue * 0.010f ) * 
                              (this->bd[2*this->type+1] - this->bd[2*this->type]);
}

int cfdCuttingPlane::isPastEnd()
{
   if (this->origin[this->type] > (this->bd[2*this->type+1] + 0.5*this->dx)) 
   {
      return 1;
   }
   else                                                                  
   {
      return 0;
   }
}

int cfdCuttingPlane::isAtEnd()
{
   if (this->origin[this->type] > (this->bd[2*this->type+1] - (this->bd[2*this->type+1]-this->bd[2*this->type]) / 100.0) )
   {
      return 1;
   }
   else
   {
      return 0;
   }
}

int cfdCuttingPlane::isAtStart()
{
   if (this->origin[this->type] < (this->bd[2*this->type] + (this->bd[2*this->type+1]-this->bd[2*this->type]) / 100.0) )
   {
      return 1;
   }
   else
   {
      return 0;
   }
}

void cfdCuttingPlane::ResetOriginToLow()
{
   this->origin[this->type] = this->bd[2*this->type] + 
                              (this->bd[2*this->type+1]-this->bd[2*this->type]) / 100.0;
}

void cfdCuttingPlane::ResetOriginToHigh()
{
   this->origin[this->type] = this->bd[2*this->type+1] - 
                              (this->bd[2*this->type+1]-this->bd[2*this->type]) / 100.0;
}

void cfdCuttingPlane::IncrementOrigin()
{
   this->origin[this->type] += this->dx;
}

