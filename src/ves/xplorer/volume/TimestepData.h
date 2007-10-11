/*************** <auto-copyright.pl BEGIN do not edit this line> *************
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
 *************** <auto-copyright.pl END do not edit this line> **************/
#ifndef JPG_TIMESTEP_DATA_H_
#define JPG_TIMESTEP_DATA_H_

#include <ves/xplorer/volume/ScalarData.h>
#include <ves/xplorer/volume/VectorData.h>

#include <gmtl/Vec.h>

#include <limits>

#include <ves/VEConfig.h>

namespace VE_TextureBased
{
   struct VE_TEXTURE_BASED_EXPORTS TimestepData
   {
      /**
       * Ctor
       * Takes the vector data and scalar data.
       *
       * @param   vd       the vector data map.
       * @param   sdm      the scalar data map.
       */
      TimestepData(const VectorMap& vdm, const ScalarDataMap& sdm)
         : mVectorMap(vdm), mScalarData(sdm)
      {}

      /// The vector data.
      VectorMap                                       mVectorMap;

      /// The scalar data.
      ScalarDataMap                                   mScalarData;

   };
}
#endif
