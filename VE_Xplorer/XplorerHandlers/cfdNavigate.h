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
 * Date modified: $Date$
 * Version:       $Rev$
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_NAVIGATE_H
#define CFD_NAVIGATE_H
/*!\file cfdNavigate.h
cfdNavigate API
*/
/*!\class VE_Xplorer::cfdNavigate
*   A class to track the wand location, object translation,
and virtual cursor location in virtual environment.
*/
#include "VE_Xplorer/SceneGraph/DCS.h"

#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

namespace VE_SceneGraph
{
   class DCS;
}

namespace VE_XML
{
   class Command;
}

#include "VE_Installer/include/VEConfig.h"

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS cfdNavigate
{
public:
   //! Constructor
   /*!
   Constructs VR Juggler objects.
   */
   cfdNavigate( );
   //! Destructor
   ~cfdNavigate( );
   //! Wand object
   /*!
   Initialization of navigation objects: VR Juggler, wand, cursor, data set
   */
   void Initialize( VE_SceneGraph::DCS* );
   void SetDataValues( int, int );
   void updateNavigationFromGUI( void );
   //! Wand object
   /*!
   Get wand direction.
   */
   double * GetDirection( );
   //! Wand object
   /*!
   Get wand location.
   */
   double * GetLocation( );
   void GetObjLocation( float xyzO[3] );
   void GetObjLocation( float &xO, float &yO, float &zO );
   //! Data set object(s)
   /*!
   Get actual cursor location with respect to the data set.
   */
   float * GetObjLocation( );
   float * GetCurObjLocation( );  //added
   //!  Cursor object(s)
   /*!
   Get cursor location with respect to the virtual space.
   */
   double * GetCursorLocation( );
   //! Virtual environment object(s)
   /*!
   Get location of objects with respect to virtual space.
   */
   double * GetWorldLocation( );
   void GetWorldLocation( double xyzW[3] );
   void GetWorldLocation( double &xW, double &yW, double &zW );
   void SetWorldLocation( double xyzW[ 3 ] );
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

   void UpdateLoc( double* );

   void UpdateDir( );

   double* GetWorldTranslation();
   float* GetWorldRotation();

   void SetHeadRotationFlag( int );
   void SetSubZeroFlag( int );

   // New function for testing the new VECommand structure
   void SetVECommand( VE_XML::Command* veCommand );
   // accessor to set initial world position
   void SetInitialWorldPosition( float* translate, float* rotate, float* scale );
private:
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
   gadget::PositionInterface  head;

   // VR Juggler's wand digital interface.
public:
   gadget::DigitalInterface digital[6];
   gadget::DigitalInterface IHdigital[10];
   gadget::DigitalInterface flyThrough[4];
   int buttonData[ 6 ];
   // x, y, and z translation of objects in world coordinates.
   // Variables only used in preFrame
   double * currentWandDirection;
   int cfdId;
   int cfdIso_value;
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;

   double worldTrans[ 3 ];
   float worldRot[ 3 ];

private:
   //! VR Juggler
   /*!
   VR Juggler's vector math function.
   */
   gmtl::Vec3f  vjVec;
   gmtl::Vec3f  LastVec;
   //! VR Juggler
   /*!
   VR Juggler's matrix math function.
   */
   gmtl::Matrix44f vjMat;
   gmtl::Matrix44f vjHeadMat;
   //! Wand object
   /*!
   Location of the wand with respect to the virtual space.
   */
   double loc[3];
   //! Wand object
   /*!
   Direction of the wand.
   */
   double dir[3];
   //! Virtual environment object(s)
   /*!
   Location of the objects with respect to the virtual space.
   */
   double worldLoc[3];
   //! Cursor object(s)
   /*!
   Location of the cursor with respect to the virtual space.
   */
   double cursorLoc[3];
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
   Displacement of the objects in virtual space.
   */
   float dObj;

   float translationStepSize;
   float rotationStepSize;

   int rotationFlag;
   int subzeroFlag;

   // class used to store xml command
   VE_XML::Command* command;
   // data storage for initial world dcs location
   float initialTranslate[ 3 ];
   float initialRotate[ 3 ];
};
}
#endif
