/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2009 by Iowa State University
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

#ifndef UNIT_IN_RESAMPLE_OUT_H
#define UNIT_IN_RESAMPLE_OUT_H

// --- VE-Suite Includes --- //
#include "UnitInOut.h"

namespace ves
{
namespace xplorer
{
namespace rtt
{

//! Same as UnitInOut but do resampling inbetween
/*
 * Resample the input. This PPU will 
 * render the input data resampled to the output. Next PPU will work 
 * on the resampled one. NOTE: You loose information in your data after 
 * appling this PPU.
 */

class UnitInResampleOut : public UnitInOut
{
public:
    ///Constructor
    //! Create default ppfx
    UnitInResampleOut();

    ///
    UnitInResampleOut(
        const UnitInResampleOut& unitInResampleOut,
        const osg::CopyOp& copyop = osg::CopyOp::SHALLOW_COPY );

    ///
    META_Node( rtt, UnitInResampleOut );

    ///
    virtual void Initialize();

    ///Get x resampling factor
    float GetFactorX() const;

    ///Get y resampling factor
    float GetFactorY() const;

    ///Set x resampling factor
    void SetFactorX( float xFactor );

    ///Set y resampling factor
    void SetFactorY( float yFactor );

protected:
    ///Destructor
    //! Release it and used memory
    virtual ~UnitInResampleOut();

private:
    ///
    bool mDirtyFactor;

    ///
    float mWidthFactor;

    ///
    float mHeightFactor;

};
} //end rtt
} //end xplorer
} //end ves

#endif //UNIT_IN_RESAMPLE_OUT_H
