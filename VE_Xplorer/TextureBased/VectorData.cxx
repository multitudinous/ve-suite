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
#include "VE_Xplorer/TextureBased/VectorData.h"

namespace VE_TextureBased
{
   bool
   VectorDataSet::setVector(const size_t idx, const VectorMap& values)
   {
      VectorMap::const_iterator itr;
      for (itr = values.begin(); itr != values.end(); ++itr)
      {
         if (idx > mVectorMap[itr->first].size())
         {
            return false;
         }
         else if (idx == mVectorMap[itr->first].size())
         {
            mVectorMap[itr->first].push_back(itr->second); 
         }
         else
         {
            mVectorMap[itr->first][idx] = itr->second;
         }
      }
      return true;
   }

   VectorMap 
   VectorDataSet::getVector(const size_t idx) const
   {
      VectorMap results;
      std::map<std::string, std::vector<gmtl::Vec3f> >::const_iterator itr;
      for (itr = mVectorMap.begin(); itr != mVectorMap.end(); ++itr)
      {
         if (idx < itr->second.size())
         {
            results[itr->first] = itr->second[idx];
         }
      }
      return results;
   }

   const std::vector<std::string>
   VectorDataSet::getVectorNames() const
   {
      std::vector<std::string> results;
      std::map<std::string, std::vector<gmtl::Vec3f> >::const_iterator itr;
      for (itr = mVectorMap.begin(); itr != mVectorMap.end(); ++itr)
      {
         results.push_back(itr->first);
      }
      return results;
   }


}
