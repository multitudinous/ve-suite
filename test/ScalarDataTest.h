#ifndef JPG_SCALAR_DATA_TEST_H_
#define JPG_SCALAR_DATA_TEST_H_

#include "ves/xplorer/volume/ScalarData.h"

#include <cxxtest/TestSuite.h>
#include <limits>

class ScalarDataTest : public CxxTest::TestSuite
{
public:
   
   void testAddScalarValues()
   {
      VE_TextureBased::ScalarDataSet sd;
      std::vector<float> v1;
      for (size_t i = 0; i < 9; ++i)
      {
         v1.push_back(static_cast<float>(i)); 
      }
      sd.setScalar("Temperature", v1);
      TS_ASSERT(sd.hasScalar("Temperature"));
      TS_ASSERT_EQUALS(sd.size("Temperature"), 9);
      
      std::vector<float> r1 = sd.getScalar("Temperature");
      TS_ASSERT_EQUALS(r1.size(), 9);
      for (size_t i = 0; i < 9; ++i)
      {
         TS_ASSERT_DELTA(r1[i], static_cast<float>(i), 0.0000001);
      }
   }

   void testGetScalarValue()
   {
      VE_TextureBased::ScalarDataSet sd;
      std::vector<float> v1;
      for (size_t i = 0; i < 9; ++i)
      {
         v1.push_back(static_cast<float>(i)); 
      }
      sd.setScalar("Temperature", v1);
      TS_ASSERT(sd.hasScalar("Temperature"));
      TS_ASSERT_EQUALS(sd.size("Temperature"), 9);
      TS_ASSERT_EQUALS(sd.hasScalar("DoesNotExist"), false);
      TS_ASSERT(std::numeric_limits<float>::has_quiet_NaN);
      // The value NaN is special in that it is never equivalent to any
      // value, including itself, thus testing to see if x != x will verify
      // if a number is NaN.
      TS_ASSERT_DIFFERS(sd.getScalar("DoesNotExist", 0), 
                        sd.getScalar("DoesNotExist", 0));
      TS_ASSERT_DIFFERS(sd.getScalar("Temperature", 9),
                        sd.getScalar("Temperature", 9));
   }

};
#endif
