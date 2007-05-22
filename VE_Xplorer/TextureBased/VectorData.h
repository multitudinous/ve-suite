#ifndef JPG_VECTOR_DATA_H_
#define JPG_VECTOR_DATA_H_

#include <gmtl/Vec.h>
#include <map>
#include <string>
#include <vector>

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   // Convenience typedef.
   typedef std::map<std::string, gmtl::Vec3f> VectorMap;

   /**
    * Represents a data Vector of a texture data set.
    */
   class VE_TEXTURE_BASED_EXPORTS VectorDataSet
   {
   public:
      

      /**
       * Default Ctor
       */
      VectorDataSet()
      {}

      /**
       * Adds a set of vector values to this data set.  If a data set of the
       * same name already exists, it is replaced with the given one.
       *
       * @param   name     the name of the vector to set.
       * @param   values   the values of the vector.
       */
      void setVector(const std::string& name, 
                     const std::vector<gmtl::Vec3f>& values)
      {
         mVectorMap[name] = values;
      }

      /**
       * Sets the value of the specified vector at the given index.  If
       * the vector name does not exist and the index is not zero,
       * or the index is greater than 1 + the current maximum index, 
       * this will return false.
       *
       * @param   name     the name of the vector to set a value of.
       * @param   idx      the index of the vector to set the value at.
       * @param   value    the value to set the vector to.
       *
       * @return     true if successful, false otherwise.
       */
      bool setVector(const std::string& name, const size_t idx, 
                     const gmtl::Vec3f& value)
      {
         if ((mVectorMap.find(name) == mVectorMap.end() && idx != 0) ||
             idx > mVectorMap[name].size())
         {
            return false;
         }
         if (idx == mVectorMap[name].size())
         {
            // Since the index is one beyond the current maximum, we have
            // to actually add the value to the vector instead of setting it.
            mVectorMap[name].push_back(value);
         }
         else
         {
            mVectorMap[name][idx] = value;
         }
         return true;
      }

      /**
       * Sets the value of vectors in this VectorDataSet at the specified
       * index (timestep).
       *
       * @param   idx      the index to set the values of
       * @param   values   the values to set for the given index.
       *
       * @return     true if successful, false otherwise
       */
      bool setVector(const size_t idx, const VectorMap& values);

      /**
       * Gets the all the values of the specified vector.
       *
       * @param   name     the name of the vector to retrieve values of.
       *
       * @return     all the values of the given vector; if the name does not
       *             exist, an empty vector is returned.
       */
      std::vector<gmtl::Vec3f> getVector(const std::string& name) const
      {
         std::vector<gmtl::Vec3f> empty;
         if (mVectorMap.find(name) != mVectorMap.end())
         {
            return mVectorMap.find(name)->second;
         }
         return empty;
      }

      /**
       * Gets the value of the specified vector at the specified index.
       * If the vector or index do not exist, it will return a gmtl::Vec3f
       * with the value of NaN for all components.
       *
       * @param   name     the name of the vector to get the value of.
       * @param   idx      the index of the desired value.
       *
       * @return     the value of name at idx, or gmtl::Vec3f(NaN, NaN, NaN)
       */
      gmtl::Vec3f getVector(const std::string& name, const size_t idx) const
      {
         if (mVectorMap.find(name) != mVectorMap.end() &&
             idx < mVectorMap.find(name)->second.size())
         {
            return mVectorMap.find(name)->second[idx]; 
         }
         float NaN = std::numeric_limits<float>::quiet_NaN();
         return gmtl::Vec3f(NaN, NaN, NaN);
      }

      /**
       * Gets the value of all vectors at the specified timestep.
       * If the timestep does not exist, this will return an empty map.
       *
       * @param   idx      the timestep to retrieve the values of
       *
       * @return     the values of all vectors at timestep idx.
       */
      VectorMap getVector(const size_t idx) const;

      /**
       * Checks to see if this VectorDataSet contains a vector with the 
       * given name.
       *
       * @param   name     the name of the vector to lookup.
       *
       * @return     true if the vector is in this dataset, false otherwise.
       */
      bool hasVector(const std::string& name)
      {
         return mVectorMap.find(name) != mVectorMap.end();
      }

      /**
       * Returns the names of all vectors contained in this data set.
       *
       * @return     a vector of strings containing the names of each vector.
       */
      const std::vector<std::string> getVectorNames() const;

      /**
       * Returns the number of values for the specified vector.
       *
       * @param   name     the name of the vector to return the value count of.
       *
       * @return     the number of values associated with the named vector.
       */
      const size_t size(const std::string& name) const
      {
         std::map<std::string, std::vector<gmtl::Vec3f> >::const_iterator itr =
            mVectorMap.find(name);
         if (itr == mVectorMap.end())
         {
            return 0;
         }
         return itr->second.size();
      }

      /**
       * Returns the number of values for the first vector in the map.
       * 
       * @note    This should be changed to something else whenever a support
       *          for time steps without vector values is added.
       *
       * @return     The size of the first vector in the set.
       */
      const size_t size() const
      {
         return mVectorMap.begin()->second.size();
      }

   private:

      /// The map of vector names to their values.
      std::map<std::string, std::vector<gmtl::Vec3f> >         mVectorMap;

   };
}
#endif
