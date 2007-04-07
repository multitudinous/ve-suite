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
