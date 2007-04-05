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
 * -----------------------------------------------------------------
 *
 *************** <auto-copyright.pl END do not edit this line> ***************/
#ifndef CFD_SCALAR_VOLUME_VIS_HANDLER_H
#define CFD_SCALAR_VOLUME_VIS_HANDLER_H
/*!\file cfdScalarVolumeVisHandler.h
* cfdScalarVolumeVisHandler API
*/

/*!\class VE_TextureBased::cfdScalarVolumeVisHandler
*
*/
#ifdef VE_PATENTED
#ifdef _OSG
namespace osg { class Group; }

namespace VE_TextureBased
{
   class cfdScalarShaderManager;
   class cfdTextureManager;
}
#include "VE_Xplorer/TextureBased/cfdVolumeVisNodeHandler.h" 

namespace VE_TextureBased
{
   class VE_TEXTURE_BASED_EXPORTS cfdScalarVolumeVisHandler 
      : public cfdVolumeVisNodeHandler{
      public:
         cfdScalarVolumeVisHandler();
         cfdScalarVolumeVisHandler(const cfdScalarVolumeVisHandler& vvnh);
         virtual ~cfdScalarVolumeVisHandler();
         virtual void Init();
         virtual void SetTextureManager(cfdTextureManager* tm);
         //cfdScalarShaderManager* GetScalarShaderManager();
         cfdScalarVolumeVisHandler& operator=(const cfdScalarVolumeVisHandler& vvnh);

      protected:
         virtual void _setUpDecorator();
         virtual void _applyTextureMatrix();
         void _createDefaultShaders();
         cfdScalarShaderManager* _transferSM;
   };
}
#endif //_OSG
#endif
#endif// CFD_SCALAR_VOLUME_VIS_HANDLER_H
