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
 * Date modified: $Date: 2007-03-18 11:01:44 -0500 (Sun, 18 Mar 2007) $
 * Version:       $Rev: 7159 $
 * Author:        $Author$
 * Id:            $Id$
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef TABLET_DEVICE_H
#define TABLET_DEVICE_H
/*!\file Tablet.h
Tablet API
*/
/*!\class VE_Xplorer::Tablet
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

#include "VE_Xplorer/XplorerHandlers/Device.h"

#ifdef _OSG
#include <osg/ref_ptr>
#elif _PERFORMER
#endif

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Tablet : public Device
{
public:
   //! Constructor
   /*!
   Constructs VR Juggler objects.
   */
   Tablet( void );
   //! Destructor
   virtual ~Tablet();
   //! Wand object
   /*!
   Initialization of navigation objects: VR Juggler, wand, cursor, data set
   */
   void Initialize( void );
   void UpdateNavigation( void );

   void SetHeadRotationFlag( int );
   void SetSubZeroFlag( int );
   // New function for testing the new VECommand structure
   void SetVECommand( VE_XML::Command* veCommand );
private:
   //! VR Juggler
   /*!
   VR Juggler's wand positional interface.
   */
   //vjPosInterface wand;
   gadget::PositionInterface  head;

   int cfdId;
   int cfdIso_value;
   osg::ref_ptr< VE_SceneGraph::DCS > worldDCS;

private:
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
