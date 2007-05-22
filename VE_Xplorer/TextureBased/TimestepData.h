#ifndef JPG_TIMESTEP_DATA_H_
#define JPG_TIMESTEP_DATA_H_

#include "VE_Xplorer/TextureBased/ScalarData.h"
#include "VE_Xplorer/TextureBased/VectorData.h"

#include <gmtl/Vec.h>

#include <limits>

#include "VE_Installer/include/VEConfig.h"

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
