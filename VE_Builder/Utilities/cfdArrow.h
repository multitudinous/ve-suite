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
 * File:          $RCSfile: cfdArrow.h,v $
 * Date modified: $Date: 2004/03/23 16:36:51 $
 * Version:       $Revision: 1.2 $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_ARROW
#define CFD_ARROW

class vtkPolyData;

class cfdArrow
{
  public:
   cfdArrow( );
   ~cfdArrow( );
   void  SetShaftResolution( int shaftRes );    
   int   GetShaftResolution( );
   void  SetShaftRadius( float shaftRad );
   float GetShaftRadius( );
   void  SetTipResolution( int tipRes );
   int   GetTipResolution( );
   void  SetTipRadius( float tipRad );
   float GetTipRadius( );
   void  SetTipLength( float tipLen );
   float GetTipLength( );
   void  TurnNormalsOn( );
   void  TurnNormalsOff( );
   vtkPolyData * GetPolyData( );

  private:
   float PI;
   int   shaftResolution;
   float shaftRadius;
   float shaftAngleIncrement;
   int   tipResolution;
   float tipAngleIncrement;
   float tipRadius;
   float tipLength;
   int normalsActivated;
};

#endif
