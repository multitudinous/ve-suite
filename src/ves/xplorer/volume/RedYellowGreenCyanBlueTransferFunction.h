/*************** <auto-copyright.rb BEGIN do not edit this line> **************
 *
 * VE-Suite is (C) Copyright 1998-2010 by Iowa State University
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

#ifndef RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H
#define RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H

// --- VE-Suite Includes --- //
#include <ves/VEConfig.h>

#include <ves/xplorer/volume/TransferFunction.h>

namespace ves
{
namespace xplorer
{
namespace volume
{
/*!\file RedYellowGreenCyanBlueTransferFunction.h
 * Red-to-Blue transfer function API
 */

/*!\class ves::xplorer::volume::RYGCBLinearTF
 * Class defining defining Red-to-Blue transfer function.
 */
class VE_TEXTURE_BASED_EXPORTS RYGCBLinearTF : public TransferFunction
{
public:
    ///Constructor
    ///\param s Power of 2 (ie, 2^n) resolution
    RYGCBLinearTF( unsigned int s = 256 );

    ///Copy Constructor
    RYGCBLinearTF( const RYGCBLinearTF& rhs );

    ///Destructor
    virtual ~RYGCBLinearTF();

    ///Initialize the data in the function
    virtual void InitializeData();

    ///Update the transfer function
    ///\param component The ComponentType\n 0 == "RED"\n1 == "GREEN"\n2 == "BLUE"\n3 == "ALPHA"
    ///\param data A pointer for passing in user data
    ///\param rangeMin The starting value to update
    ///\param rangeMax The ending value to update
    /*virtual void Update(unsigned int component,
                        void* data,
                        float rangeMin=0.0,
                        float rangeMax=1.0);
    */
    ///Equal operator
    ///\param rhs The TransferFunction to set this one to.
    RYGCBLinearTF& operator=( const RYGCBLinearTF& rhs );

protected:
    ///Update the transfer function
    virtual void _update();

};
} //end volume
} //end xplorer
} //end ves

#endif //RED_YELLOW_GREE_CYAN_BLUE_LINEAR_TRANSFER_FUNCTION_H
