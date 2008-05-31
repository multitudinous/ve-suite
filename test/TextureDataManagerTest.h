#ifndef JPG_TEXTURE_DATA_MANAGER_TEST_H_
#define JPG_TEXTURE_DATA_MANAGER_TEST_H_

#include "ves/xplorer/volume/TextureDataManager.h"
#include "ves/xplorer/volume/TimestepData.h"

#include <cxxtest/TestSuite.h>

class TextureDataManagerTest : public CxxTest::TestSuite
{
public:

   void testGetTextureData()
   {
      VE_TextureBased::TextureData td1;
      
   }

   void testCreateTextureFromDatabase()
   {
      std::string db = "texture_test.db";
      std::string sim = "Test";
      TS_ASSERT(VE_TextureBased::TextureDataManager::Instance().
                createTextureFromDatabase(db, sim));
      VE_TextureBased::TextureDataPtr td = VE_TextureBased::TextureDataManager::Instance().getTextureData(sim);
   }
};
#endif
