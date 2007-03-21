#ifndef JPG_VECTOR_DATA_H_
#define JPG_VECTOR_DATA_H_

#include <gmtl/Vec.h>
#include <string>
#include <vector>

#ifdef VE_PATENTED

namespace VE_TextureBased
{
   /**
    * Represents a data Vector of a texture data set.
    */
   struct VE_TEXTURE_BASED_EXPORTS VectorData
   {
      /**
       * Default Ctor
       */
      VectorData()
      {}

      /**
       * Ctor that just takes a vector; this is so gmtl::Vec3f objects can
       * be used to initialize a VectorData object.
       */
      VectorData(const gmtl::Vec3f& vec)
         : mData(vec)
      {}

      /**
       * Ctor that takes a vector and a string.
       *
       * @param   vec      the vector data.
       * @param   name     the name of the vector.
       */
      VectorData(const std::string& name, const gmtl::Vec3f& vec)
         : mName(name), mData(vec)
      {}

      /**
       * Implicit conversion operator provided to allow the use of VectorData
       * objects with APIs that only accept gmtl::Vec3f.
       */
      operator gmtl::Vec3f()
      {
         return mData;
      }

      /**
       * Array operator overload for convenience.
       *
       * @param   idx      the array index.
       *
       * @return     the value of mData[idx].
       */
      float& operator[](const size_t idx)
      {
         return mData[idx];
      }

      /**
       * const array operator overload so that const VectorData objects will
       * compile without error.
       *
       * @param   idx      the array index.
       *
       * @return     the value of mData[idx].
       */
      const float& operator[](const size_t idx) const
      {
         return mData[idx];
      }

      /// The name of the vector.
      std::string                                  mName;

      /// The vector data.
      gmtl::Vec3f                                  mData;
   };

   // Convenience typedef
   typedef std::vector<VectorData> VectorDataSet;
}

#endif
#endif
