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
#ifndef JPG_TEXTURE_DATA_H_
#define JPG_TEXTURE_DATA_H_

#ifdef _WIN32
#include <windows.h>
#define WIN32_LEAN_AND_MEAN
#endif

#include <VE_Xplorer/TextureBased/ScalarData.h>
#include <VE_Xplorer/TextureBased/VectorData.h>
#include <VE_Xplorer/TextureBased/TimestepData.h>

#include <gmtl/Vec.h>
#include <loki/SmartPtr.h>

#include <map>
#include <string>
#include <vector>
#include <VE_Installer/include/VEConfig.h>

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
       * @param   spacing  the spacing of this data set.
       * @param   origin   the origin of this data set (if one exists)
       */
      TextureData(const std::string& name, 
                  const VectorDataSet& data,
                  const ScalarDataSet& scalars,
                  const gmtl::Vec3f& spacing,
                  gmtl::Vec3f* origin=NULL)
         : mName(name), mVectorData(data), mScalarData(scalars), 
           mSpacing(spacing), mOrigin(origin)
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
       * Checks to see if the texture data has an origin or not.
       *
       * @return     true if an origin is present, false otherwise.
       */
      bool hasOrigin() const
      {
         return mOrigin != NULL;
      }

      /**
       * Returns the origin if once exists; otherwise, this will return an
       * empty Vector.  Use hasOrigin() to see if one exists or not.
       *
       * @return     the origin, or an empty vector.
       */
      gmtl::Vec3f getOrigin() const
      {
         if (mOrigin)
         {
            return *mOrigin;
         }
         return gmtl::Vec3f();
      }

      /**
       * Sets the origin of this data set.
       *
       * @param   origin      the new origin of this data set.
       */
      void setOrigin(const gmtl::Vec3f& origin)
      {
         if (mOrigin)
         {
            delete mOrigin;
         }
         mOrigin = new gmtl::Vec3f(origin);
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
       * Returns the Vector Data associated with the specified timestep.
       *
       * @param   idx      the index of the Vector Data to retrieve.
       *
       * @return     The vector data associated with the given index.
       *
       * @note    No bounds checking is performed on idx.
       */
      VectorMap getVectorData(const size_t idx) const
      {
         return mVectorData.getVector(idx);
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
         return TimestepData(mVectorData.getVector(idx), 
                             mScalarData.getScalar(idx));
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
      gmtl::Vec3f*                                    mOrigin;

      /// The spacing of this data.
      gmtl::Vec3f                                     mSpacing;
      
      
   };

   /// Typedef for a SmartPtr type for the TextureData.
   typedef Loki::SmartPtrDef<TextureData>::type TextureDataPtr;
}
#endif
