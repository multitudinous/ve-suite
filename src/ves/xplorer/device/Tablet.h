/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2011 by Iowa State University
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
 *************** <auto-copyright.rb END do not edit this line> ***************/

#ifndef TABLET_H
#define TABLET_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/scenegraph/DCS.h>

#include <ves/xplorer/device/Device.h>

#include <ves/open/xml/CommandPtr.h>

// --- VRJuggler Includes --- //
#include <gmtl/Vec.h>
#include <gmtl/Matrix.h>
#include <gadget/Type/PositionInterface.h>
#include <gadget/Type/DigitalInterface.h>

// --- OSG Includes --- //
#include <osg/ref_ptr>

namespace ves
{
namespace xplorer
{

namespace scenegraph
{
class DCS;
}

namespace device
{
/*!\file Tablet.h
 * Tablet API
 */

/*!\class ves::xplorer::Tablet
 * A class to track the wand location, object translation,
   and virtual cursor location in virtual environment.
 */
class VE_XPLORER_EXPORTS Tablet : public Device
{
public:
    ///Constructor
    Tablet();

    ///Destructor
    virtual ~Tablet();

    ///
    ///\return
    virtual Tablet* AsTablet();

    ///Initialize some variables in the class
    virtual void Initialize();

    ///Processes tablet events
    virtual void ProcessEvents( ves::open::xml::CommandPtr command );

    ///Set the rotation method
    ///\param input Indicates which rotation method is needed
    void SetHeadRotationFlag( int input );

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

};
} //end device
} //end xplorer
} //end ves

#endif //TABLET_H
