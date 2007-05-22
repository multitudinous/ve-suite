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
 * Date modified: $Date: 2006-07-28 20:47:10 -0500 (Fri, 28 Jul 2006) $
 * Version:       $Rev: 5067 $
 * Author:        $Author: mccdo $
 * Id:            $Id: SmokeShaderManager.h 5067 2006-07-29 01:47:10Z mccdo $
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef GREY_SCALE_SHADER_MANAGER_H
#define GREY_SCALE_SHADER_MANAGER_H
/*!\file GreyScaleShaderManager.h
* GreyScaleShaderManager API
*/

/*!\class VE_TextureBased::GreyScaleShaderManager
*
*/
#ifdef _OSG
#include "VE_Xplorer/TextureBased/cfdScalarShaderManager.h"
namespace VE_TextureBased
{
   class cfdTextureManager;
}
namespace VE_TextureBased
{
class GreyScaleShaderManager: public cfdScalarShaderManager{
public:
   ///Constructor
   GreyScaleShaderManager(){};
   ///Destructor
   virtual ~GreyScaleShaderManager(){}
         
protected:
   ///Update the transfer function
   ///\param fastUpdate Determines if we are updating the preintegrated table or just diagonal
   //void _updateTransferFunction(bool preIntegrated=true);
   ///Initialize the transfer functions
   virtual void _initTransferFunctions();
};
}
#endif//_OSG
#endif// GREY_SCALE_SHADER_MANAGER_H
