/*************** <auto-copyright.pl BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2006 by Iowa State University
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
 * File:          $RCSfile: cfdNavigate.h,v $
 * Date modified: $Date$
 * Version:       $Rev$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_NAVIGATE_H
#define CFD_NAVIGATE_H

#include <gadget/Type/PositionInterface.h>
#include <gmtl/Matrix.h>
#include <gmtl/Vec.h>

//! Navigation tracker
/*!
  A class to track the wand location, object translation,
  and virtual cursor location in virtual environment.
*/

class cfdNavigate
{
 public:
  //! Constructor
  /*!
    Constructs VR Juggler objects.
  */
  cfdNavigate( );
  //! Destructor
  /*!
    
  */
  ~cfdNavigate( );
  //! Wand object
  /*!
    Initialization of navigation objects: VR Juggler, wand, cursor, data set
  */
  void ResetCoordsToZero( );
  void Initialize( float );
  void GetDirection( float xyzV[3] );
  void GetDirection( float &xV, float &yV, float &zV );
  //! Wand object
  /*!
    Get wand direction.
  */
  float * GetDirection( );
  void GetLocation( float xyzL[3] );
  void GetLocation( float &xL, float &yL, float &zL );
  //! Wand object
  /*!
    Get wand location.
  */
  float * GetLocation( );
  void GetObjLocation( float xyzO[3] );
  void GetObjLocation( float &xO, float &yO, float &zO );
  //! Data set object(s)
  /*!
    Get actual cursor location with respect to the data set.
  */
  float * GetObjLocation( );
  float * GetCurObjLocation( );  //added
  void GetCursorLocation( float xyzC[3] );
  void GetCursorLocation( float &xC, float &yC, float &zC );
  //!  Cursor object(s)
  /*!
    Get cursor location with respect to the virtual space.
  */
  float * GetCursorLocation( );
  void GetWorldLocation( float xyzW[3] );
  void GetWorldLocation( float &xW, float &yW, float &zW );
  //! Virtual environment object(s)
  /*!
    Get location of objects with respect to virtual space.
  */
  float * GetWorldLocation( );
  //! Wand object
  /*!
    Forward translation.
  */
  void FwdTranslate( );
  //! Wand object
  /*!
    Aft translation.
  */
  void AftTranslate( );
  //! Cursor object
  /*!
    Cursor tracker.
  */
  void CursorTranslate( );

 private:
  //! Wand object
  /*!
    Update wand direction.
  */
  void UpdateDir( );
  //! Wand object
  /*!
    Update wand location.
  */
  void UpdateLoc( );
  //! VR Juggler
  /*!
    VR Juggler's wand positional interface.
  */
  //vjPosInterface wand;
    gadget::PositionInterface  wand;
  //! VR Juggler
  /*!
    VR Juggler's vector math function.
  */
  //vjVec3 *vjVec;
  gmtl::Vec3f  vjVec;
  //! VR Juggler
  /*!
    VR Juggler's matrix math function.
  */
  gmtl::Matrix44f vjMat;
  //vjMatrix *vjMat;
  //! Wand object
  /*!
    Location of the wand with respect to the virtual space.
  */
  float loc[3];
  //! Wand object
  /*!
    Direction of the wand.
  */
  float dir[3];
  //! Virtual environment object(s)
  /*!
    Location of the objects with respect to the virtual space.
  */
  float worldLoc[3];
  //! Cursor object(s)
  /*!
    Location of the cursor with respect to the virtual space.
  */
  float cursorLoc[3];
  //! Data set object(s)
  /*!
    Location with respect to data set (the actual location to interact with data).
  */
  float objLoc[3];
  //! Cursor object(s)
  /*!
    Cursor length.
  */
  float cursorLen;
  //! Wand object
  /*!
    Displacement of the objects in virtual space. (AKA step size)
  */
  float dObj;
};

#endif
