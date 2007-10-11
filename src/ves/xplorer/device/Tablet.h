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
#ifndef TABLET_H
#define TABLET_H

/*!\file Tablet.h
  Tablet API
*/

/*!\class VE_Xplorer::Tablet
* A class to track the wand location, object translation,
  and virtual cursor location in virtual environment.
*/

// --- VE-Suite Includes --- //
#include "VE_Installer/include/VEConfig.h"

#include "VE_Xplorer/SceneGraph/DCS.h"

#include "VE_Xplorer/XplorerHandlers/Device.h"

// --- VRJuggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

// --- OSG Includes --- //
#ifdef _OSG
#include <osg/ref_ptr>
#endif

namespace VE_SceneGraph
{
    class DCS;
}

namespace VE_XML
{
    class Command;
}

namespace VE_Xplorer
{
class VE_XPLORER_EXPORTS Tablet : public Device
{
public:
    ///Constructor
    Tablet();
    ///Destructor
    virtual ~Tablet();

    ///Initialize some variables in the class
    void Initialize();

    ///Update the position in scene
    void UpdateNavigation();

    ///Set the rotation method
    ///\param input Indicates which rotation method is needed
    void SetHeadRotationFlag( int input );

    ///Does not let the user go below the ground plane at 0,0,0 
    ///\param input Flag to insure translation does not go below zero plane
    void SetSubZeroFlag( int zero );

    ///New function for new VECommand structure
    ///\param veCommand Sets the Command used for navigation
    void SetVECommand( VE_XML::Command* veCommand );

private:
    gadget::PositionInterface head; ///<VRJuggler's head positional interface

    int cfdIso_value; ///<Variable used for keeping track of type of movement

private:
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

    VE_XML::Command* command; ///<Stores xml command

};
}
#endif //TABLET_H
