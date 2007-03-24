#ifndef JPG_TEXTURE_DATA_MANAGER_H_
#define JPG_TEXTURE_DATA_MANAGER_H_

#ifdef VE_PATENTED

#ifdef _WIN32
#include <windows.h>
#define WIN32_LEAN_AND_MEAN
#endif

#include "VE_Xplorer/TextureBased/ScalarData.h"
#include "VE_Xplorer/TextureBased/VectorData.h"
#include "VE_Xplorer/TextureBased/TimestepData.h"

#include <gmtl/Vec.h>
#include <loki/SmartPtr.h>

#include <map>
#include <string>
#include <vector>
#include "VE_Installer/include/VEConfig.h"

namespace VE_TextureBased
{
   /**
    * Represents a data set that is used to create a texture.  This data
    * set is composed of several timesteps each of which contain a vector,
    * and any number of scalars.
    */
   class VE_TEXTURE_BASED_EXPORTS TextureData
   {
   public:

      /**
       * Default Ctor
       */
      TextureData()
         : mOrigin(NULL)
      {}
      
      /**
       * Ctor
       * Takes the name of this texture data set and the data itself.
       *
       * @param   name     the name of this data set.
       * @param   data     the vector data of this data set.
       * @param   scalars  the scalars of this data set.
       */
      TextureData(const std::string& name, 
                  const VectorDataSet& data,
                  const ScalarDataSet& scalars)
         : mName(name), mVectorData(data), mScalarData(scalars), mOrigin(0)
      {}

      /**
       * Dtor
       * Cleans up the origin if necessary.
       */
      ~TextureData()
      {
         if (mOrigin)
         {
            delete mOrigin;
         }
      }

      /**
       * Sets the name of this data set.
       *
       * @param   name     the name of the data set.
       */
      void setName(const std::string& name)
      {
         mName = name;
      }

      /**
       * Gets the name of this data set.
       *
       * @return     the name of this data set.
       */
      const std::string getName() const
      {
         return mName;
      }

      /**
       * Gets the number of timesteps in this data set.
       *
       * @return     the number of timesteps in this data set.
       */
      const size_t getTimestepCount() const
      {
         return mVectorData.size();
      }

      /**
       * Returns the VectorData associated with the specified timestep.
       *
       * @param   idx      the index of the VectorData to retrieve.
       *
       * @return     The vector data associated with the given index.
       *
       * @note    No bounds checking is performed on idx.
       */
      VectorData getVectorData(const size_t idx) const
      {
         return mVectorData[idx]
      }

      /**
       * Returns the ScalarData associated with the specified timestep
       *
       * @param   idx      the index of the ScalarData to retrieve.
       *
       * @return     The ScalarData map associated with idx.
       *
       * @note    No bounds checking is performed on idx.
       */
      ScalarDataMap getScalarData(const size_t idx) const
      {
         return mScalarData.getScalar(idx);
      }

      /**
       * Returns the TimestepData associated with the 
       * specified timetep.
       *
       * @param   idx      the index of the data to retrieve.
       *
       * @return     The timestep data associated with idx. 
       *
       * @note    No bounds checking is performed on idx.
       */
      TimestepData getTimestepData(const size_t idx) const
      {
         return TimestepData(mVectorData[idx], mScalarData.getScalar(idx));
      }

   private:

      /// The name of this texture data.
      std::string                                     mName;

      /// The vector data for this texture. 
      VectorDataSet                                   mVectorData;

      /// The scalar data for this texture.
      ScalarDataSet                                   mScalarData;

      /// The origin of this data; this is a pointer since not every dataset
      /// has an origin.
      VectorData*                                     mOrigin;

      /// The spacing of this data.
      VectorData                                      mSpacing;
      
      
   };

   /// Typedef for a SmartPtr type for the TextureData.
   typedef Loki::SmartPtrDef<TextureData>::type TextureDataPtr;
}

#endif
#endif
