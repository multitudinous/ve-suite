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
#ifndef JPG_DATA_H_
#define JPG_DATA_H_

#include <Variant.h>

#include <string>

namespace VE_TextureBased
{
   /**
    * Dummy class to represent NULL Database Values
    */
   struct DBNullValue
   {};

   /**
    * Raw BinaryData
    */
   class BinaryData
   {
   public:
      /**
       * Default Ctor
       * Does not allocate the array.
       */
      BinaryData()
         : mSize(0), mData(NULL)
      {}

      /**
       * Ctor
       * Sets the size and allocates the array.
       */
      BinaryData(const size_t size)
         : mSize(size), mData(new unsigned char[size])
      {}

      /**
       * Ctor
       * Sets the data from the given buffer.
       *
       * @param   size     the size of the data.
       * @param   data     the data to buffer.
       *
       * @pre  size MUST be the actual size of data!
       *
       * @post This BinaryData is set to the data buffer that was given;
       *       this class assumes ownership of the given data pointer!
       */
      BinaryData(const size_t size, unsigned char* data)
         : mSize(size), mData(data)
      {
      }

      /**
       * Copy Ctor
       * Duplicates another BinaryData buffer.
       */
      BinaryData(const BinaryData& src)
         : mSize(src.mSize), mData(new unsigned char[src.mSize])
      {
         memcpy(mData, src.mData, mSize);
      }

      /**
       * Destructor
       */
      ~BinaryData()
      {
         delete[] mData;
         mData = 0;
      }
      
      /**
       * Retrieves the size of the data.
       *
       * @return     the size of the data.
       */
      const size_t getSize() const
      {
         return mSize;
      }

      /**
       * Assignment Operator
       */
      BinaryData& operator=(const BinaryData& rhs)
      {
         if (this != &rhs)
         {
            if (mSize != rhs.mSize)
            {
               delete[] mData;
               mSize = rhs.mSize;
               mData = new unsigned char[mSize];
            }
            memcpy(mData, rhs.mData, mSize);
         }
         return *this;
      }

      /**
       * Array Operator
       * Returns a reference to the binary array.
       */
      const unsigned char& operator[](const size_t idx) const
      {
         return mData[idx];
      }

      /**
       * Array Operator
       * Returns a reference to the binary array.
       */
      unsigned char& operator[](const size_t idx)
      {
         return mData[idx];
      }

      /**
       * Comparison Operator
       * Primarily for use by unit tests.
       */
      bool operator==(const BinaryData& rhs)
      {
         return ( (&rhs == this) || 
                  ((mSize == rhs.mSize) &&
                   (memcmp(mData, rhs.mData, mSize) == 0)) );
      }

   private:

      /// The size of the data.
      size_t                                          mSize;

      /// The raw data.
      unsigned char *                                 mData;

   };
   /// Typedef for the type of results that can be retrieved.
   typedef Loki::Variant<LOKI_TYPELIST_5(DBNullValue, int64_t, double, std::string, BinaryData) > DBValue;
}
#endif
