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
#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H
/*!\file cfdScalarVolumeVisHandler.h
* cfdScalarVolumeVisHandler API
*/

/*!\class ves::xplorer::volume::cfdScalarVolumeVisHandler
*
*/
#ifdef _OSG
namespace osg { class Group; }

#include <ves/xplorer/volume/cfdVolumeVisNodeHandler.h> 
namespace ves
{
namespace xplorer
{
namespace volume
{
class cfdScalarShaderManager;
class cfdTextureManager;
class VE_TEXTURE_BASED_EXPORTS cfdScalarVolumeVisHandler 
      : public cfdVolumeVisNodeHandler
   {
      public:
         ///Constructor
         cfdScalarVolumeVisHandler();
         ///Copy Constructor
         ///\param vvnh cfdScalarVolumeVisHandler to copy
         cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
         ///Destructor
         virtual ~cfdScalarVolumeVisHandler();
         ///Initialize parameters
         virtual void Init();
         ///Set the current cfdTextureManager
         ///\param tm cfdTextureManager pointer
         virtual void SetTextureManager(cfdTextureManager* tm);
         ///Equal operator
         ///\param vvnh cfdScalarVolumeVisHandler to set this equal to.
         cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);

      protected:
         ///Set up the decorator node 
         virtual void _setUpDecorator();
         ///Apply the texture matrix
         virtual void _applyTextureMatrix();
         ///Create the default set of shaders
         void _createDefaultShaders();
         cfdScalarShaderManager* _transferSM;///<cfdScalarShaderManager
   };
}
}
}
#endif //_OSG
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
