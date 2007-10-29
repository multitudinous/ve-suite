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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef LUMINANCE_TRANSFER_FUNCTION_H
#define LUMINANCE_TRANSFER_FUNCTION_H
/*!\file LuminanceTransferFunction.h
  Grey scale transfer function API
  */
/*!\class ves::xplorer::volume::LuminanceTF
 * Class defining grey scale transfer function.
 */
#include <vector>
#include <ves/xplorer/volume/TransferFunction.h>
#include <ves/VEConfig.h>
namespace ves
{
namespace xplorer
{
namespace volume
{
class VE_TEXTURE_BASED_EXPORTS LuminanceTF: public TransferFunction
{
public:
   ///Constructor
   ///\param s Power of 2 (ie, 2^n) resolution
   LuminanceTF(unsigned int s=256);
   
   ///Copy Constructor
   LuminanceTF(const LuminanceTF& rhs);

   ///Destructor
   virtual ~LuminanceTF();

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
                       float rangeMax=1.0);*/
   
   ///Equal operator
   ///\param rhs The TransferFunction to set this one to. 
   LuminanceTF& operator=(const LuminanceTF& rhs);
protected:
	///Update the transfer function
   virtual void _update();
};
}
}
}
#endif// LUMINANCE_TRANSFER_FUNCTION_H
