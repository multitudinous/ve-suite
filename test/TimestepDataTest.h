#ifndef JPG_TIMESTEP_DATA_TEST_H_
#define JPG_TIMESTEP_DATA_TEST_H_

#include "VE_Xplorer/TextureBased/TimestepData.h"

#include <cxxtest/TestSuite.h>
#include <limits>

class TimestepDataTest : public CxxTest::TestSuite
{
public:

   void testArrayOperatorWithString()
   {
      VE_TextureBased::VectorData vd3(std::string("Foo"), 
                                      gmtl::Vec3f(4.0f, 5.0f, 6.0f));
      VE_TextureBased::ScalarDataSet sd;
      std::vector<float> v1;
      for (size_t i = 0; i < 9; ++i)
      {
         v1.push_back(static_cast<float>(i)); 
      }
      sd.addScalar("Temperature", v1);
      VE_TextureBased::TimestepData td(vd3.mData, sd.getScalar(1));

      TS_ASSERT_DELTA(td["Temperature"], 1.0f, 0.0000001);
      TS_ASSERT(td.mNaN != td.mNaN);
   }

   void testArrayOperatorWithIdx()
   {
      VE_TextureBased::VectorData vd3(std::string("Foo"), 
                                      gmtl::Vec3f(4.0f, 5.0f, 6.0f));
      VE_TextureBased::ScalarDataSet sd;
      std::vector<float> v1;
      for (size_t i = 0; i < 9; ++i)
      {
         v1.push_back(static_cast<float>(i)); 
      }
      sd.addScalar("Temperature", v1);
      VE_TextureBased::TimestepData td(vd3.mData, sd.getScalar(1));
      
      TS_ASSERT_DELTA(td[0], 4.0f, 0.0000001f);
      TS_ASSERT_DELTA(td[1], 5.0f, 0.0000001f);
      TS_ASSERT_DELTA(td[2], 6.0f, 0.0000001f);
      TS_ASSERT(td.mNaN != td.mNaN);
   }
};

#endif
