#ifndef JPG_TIMESTEP_DATA_H_
#define JPG_TIMESTEP_DATA_H_

#ifdef VE_PATENTED

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
       * @param   vd       the vector data.
       * @param   sdm      the scalar data map.
       */
      TimestepData(const VectorData& vd, const ScalarDataMap& sdm)
         : mVectorData(vd), mScalarData(sdm), 
           mNaN(std::numeric_limits<float>::quiet_NaN())
      {}

      /**
       * Implicit conversion operator to convert to a gmtl::Vec3f.
       */
      operator gmtl::Vec3f()
      {
         return mVectorData.mData;
      }

      /**
       * Array Operator overload to return a value of the VectorData at 
       * index idx.
       *
       * @param   idx      the array index of the vector data to return.
       *
       * @return     the value of mVectorData at idx.
       *
       * @note    No bounds checking on idx is performed.
       */
      float& operator[](const size_t idx)
      {
         return mVectorData.mData[idx];
      }

      const float& operator[](const size_t idx) const
      {
         return mVectorData.mData[idx];
      }

      /**
       * Array Operator overload to return a value of the specified scalar in 
       * the Scalar Data.
       *
       * @param   name     the name of the scalar to get the value of.
       *
       * @return     the value of the scalar for name, or NaN if no such value
       *             exists.
       */
      float& operator[](const std::string& name)
      {
         ScalarDataMap::iterator itr = mScalarData.find(name);
         if (itr == mScalarData.end())
         {
            return mNaN;
         }
         return itr->second;
      }

      const float& operator[](const std::string& name) const
      {
         ScalarDataMap::const_iterator itr = mScalarData.find(name);
         if (itr == mScalarData.end())
         {
            return mNaN;
         }
         return itr->second;
      }

      /// The vector data.
      VectorData                                      mVectorData;

      /// The scalar data.
      ScalarDataMap                                   mScalarData;

      /// The NaN constant.
      float                                           mNaN;
   };
}

#endif
#endif
