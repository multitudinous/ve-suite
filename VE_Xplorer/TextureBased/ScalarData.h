#ifndef JPG_SCALAR_DATA_H_
#define JPG_SCALAR_DATA_H_

#include <limits>
#include <map>
#include <string>
#include <vector>

#ifdef VE_PATENTED

#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   /// Convenience typedef of a single timesteps scalar data map.
   typedef std::map<std::string, float> ScalarDataMap;

   /**
    * Represents a set of scalars that is used in a texture data set.  This is
    * the group of scalars that will have a value for each timestep.
    */
   class VE_TEXTURE_BASED_EXPORTS ScalarDataSet
   {
   public:

      /// A pair of a name and a scalar.
      typedef std::pair<std::string, float>     ScalarData;

      ScalarDataSet()
      {}

      /**
       * Adds a set of scalar values to this data set.  If a data set of the
       * same name already exists, it is replaced with the given one.
       *
       * @param   name     the name of the scalar to add.
       * @param   values   the values of the scalar.
       */
      void setScalar(const std::string& name, const std::vector<float>& values)
      {
         mScalarMap[name] = values;
      }

      /**
       * Sets a value of a scalar dataset at a particular index.
       *
       * @param   name     the name of the scalar to set a value for.
       * @param   idx      the index of the value to set. 
       * @param   value    the value to set.
       *
       * @return     true if successful, false otherwise.
       */
      bool setScalar(const std::string& name, const size_t idx, 
                     const float value)
      {
         if ( (mScalarMap.find(name) == mScalarMap.end() && idx != 0) ||
             idx >= mScalarMap[name].size())
         {
            return false;
         }
         (mScalarMap[name])[idx] = value;
         return true;
      }

      /**
       * Gets the set of values associated with a scalar name.
       *
       * @param   name     the name of the scalar to retrieve values for.
       *
       * @return     a list of all scalar values contained for the specified
       *             scalar name.
       */
      const std::vector<float> getScalar(const std::string& name) const
      {
         const std::vector<float> empty;
         if (mScalarMap.find(name) != mScalarMap.end())
         {
            return mScalarMap.find(name)->second;
         }
         return empty;
      }

      /**
       * Gets the value of the given scalar data set at index idx.
       *
       * @param   name     the name of the scalar to retrieve a value of.
       * @param   idx      the index of the desired value.
       *
       * @return     the value of the given scalar at the desired index, or
       *             NaN if the name isn't in this data set, or the index is
       *             out of range.
       */
      float getScalar(const std::string& name, const size_t idx) const
      {
         if (mScalarMap.find(name) == mScalarMap.end() ||
             idx >= mScalarMap.find(name)->second.size())
         {
            return std::numeric_limits<float>::quiet_NaN();
         }
         return mScalarMap.find(name)->second[idx];
      }

      /**
       * Gets the scalar data map associated with all scalars at index idx.
       *
       * @param   idx      the index of the values to get.
       *
       * @return     a scalar data map of each scalars value at index idx.
       *
       * @note    No bounds checking is performed on idx.
       */
      ScalarDataMap getScalar(const size_t idx) const
      {
         ScalarDataMap results;
         std::map<std::string, std::vector<float> >::const_iterator itr;
         for (itr = mScalarMap.begin(); itr != mScalarMap.end(); ++itr)
         {
            results.insert(std::make_pair(itr->first, itr->second[idx]));
         }
         return results;
      }


      /**
       * Checks to see if this data set contains the specified scalar name.
       *
       * @param   name     the scalar name to look for.
       *
       * @return     true if the scalar is in this set, false otherwise.
       */
      bool hasScalar(const std::string& name) const
      {
         return mScalarMap.find(name) != mScalarMap.end();
      }

      /**
       * Retrieves a list of the names of scalars that are in this data set.
       *
       * @return     a list of scalar names that are in this data set.
       */
      const std::vector<std::string> getScalarNames() const
      {
         std::vector<std::string> names;
         std::map<std::string, std::vector<float> >::const_iterator itr;
         for (itr = mScalarMap.begin(); itr != mScalarMap.end(); ++itr)
         {
            names.push_back(itr->first);
         }
         return names;
      }

      /**
       * Gets the number of values associated with a particular scalar.
       */
      const size_t size(const std::string& name) const
      {
         std::map<std::string, std::vector<float> >::const_iterator itr =
            mScalarMap.find(name);
         if (itr != mScalarMap.end())
         {
            return itr->second.size();
         }
         return 0;
      }

   private:

      /// The map of scalar names to their values.
      std::map<std::string, std::vector<float> >            mScalarMap;
      
   };

}
#endif
#endif
