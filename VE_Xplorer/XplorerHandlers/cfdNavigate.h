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
   /// Constructor
   cfdNavigate( );

   /// Destructor
   ~cfdNavigate( );

   ///Initialization of navigation objects
   ///\param worldDCS The world coordinate system
   void Initialize( VE_SceneGraph::DCS* worldDCS);

   ///Update nagvigation commands
   void updateNavigationFromGUI( void );

   ///Get wand direction
   double* GetDirection( );

   ///Get wand location
   double* GetLocation( );

   ///Get the object location
   ///\param xyzO An array of doubles containing the object position
   void GetObjLocation( double xyzO[3] );

   ///Get the object location
   ///\param &x0 The x position
   ///\param &y0 The y position
   ///\param &z0 The z position
   void GetObjLocation( double &xO, double &yO, double &zO );

   ///Get the object location
   double* GetObjLocation( );

   ///Get the cursor location with respect to the data set
   double* GetCurObjLocation( ); 

   ///Get cursor location with respect to the virtual space
   double* GetCursorLocation( );

   ///Get the world location
   double* GetWorldLocation( );

   ///Get the world location
   ///\param xyzW An array of doubles containing the world coordinates
   void GetWorldLocation( double xyzW[3] );

   ///Get the world location
   ///\param &xW The x world coordinate
   ///\param &yW The y world coordinate
   ///\param &zW The z world coordinate
   void GetWorldLocation( double &xW, double &yW, double &zW );

   ///Set the world coordinates
   ///\param xyzW An array of doubles containing the world coordinates
   void SetWorldLocation( double xyzW[ 3 ] );

   ///Forward translation
   void FwdTranslate( );

   ///Aft translation
   void AftTranslate( );

   ///Cursor tracker
   void CursorTranslate( );

   ///Update the world coordinates
   ///\param tempTrans An array of doubles with new coordinate position
   void UpdateLoc( double* tempTrans );

   ///Transfer VRJuggler coordinates to OSG
   void UpdateDir( );

   ///Get the current world coordinate translation
   double* GetWorldTranslation();

   ///Get the current world coordinate rotation
   double* GetWorldRotation();

   ///Set the rotation method
   ///\param input Indicates which rotation method is needed
   void SetHeadRotationFlag( int input );

   ///Does not let the user go below the ground plane at 0,0,0 
   ///\param zero Flag to insure translation does not go below zero plane
   void SetSubZeroFlag( int input );

   ///New function for testing the new VECommand structure
   ///\param veCommand Sets the Command used for navigation
   void SetVECommand( VE_XML::Command* veCommand );
private:
   ///Update wand location
   void UpdateLoc( );

   gadget::PositionInterface wand; ///<VRJuggler's wand positional interface
   gadget::PositionInterface head; ///<VRJuggler's head positional interface

   // VR Juggler's wand digital interface.
public:
   gadget::DigitalInterface digital[6]; ///Array handling button controls on wand
   gadget::DigitalInterface IHdigital[10]; ///<do not know what this does
   gadget::DigitalInterface flyThrough[4]; ///<do not know what this does
   int buttonData[ 6 ]; ///<do not know what this does

   double* currentWandDirection; ///<Current wand direction
   int cfdIso_value; ///<Variable used for keeping track of type of movement
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS; ///<The world coordinate system

   double worldTrans[ 3 ]; ///<World coordinate translation
   double worldRot[ 3 ]; ///<World coordinate rotation

private:
   gmtl::Vec3d  vjVec; ///<VRJuggler's vector position
   gmtl::Vec3d  LastVec; ///<VRJuggler's last vector position

   gmtl::Matrix44d vjMat; ///<Contains current translation matrix
   gmtl::Matrix44d vjHeadMat; ///<Contains current head position matrix

   double loc[3]; ///<Location of the wand with respect to the virtual space
   double dir[3]; ///<Direction of the wand
   double worldLoc[3]; ///<Location of the objects with respect to the virtual space
   double cursorLoc[3]; ///<Location of the cursor with respect to the virtual space
   double objLoc[3]; ///<Location with respect to data set (the actual location to interact with data)
   double cursorLen; ///<Cursor length
   double dObj; ///<Displacement of the objects in virtual space

   double translationStepSize; ///<Size of translation step
   double rotationStepSize; ///<Size of rotation step

   int rotationFlag; ///<Rotation flag
   int subzeroFlag; ///<Zero plane flag
 
   VE_XML::Command* command; ///<Store xml command

   double initialTranslate[ 3 ]; ///<Initial world coordinate translation
   double initialRotate[ 3 ]; ///<Initial world coordinate rotation
};
}
#endif
