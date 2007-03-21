#ifndef JPG_VECTOR_DATA_TEST_H_
#define JPG_VECTOR_DATA_TEST_H_

#include "VE_Xplorer/TextureBased/VectorData.h"

#include <gmtl/Vec.h>
#include <cxxtest/TestSuite.h>

#include <string>

class VectorDataTest : public CxxTest::TestSuite
{
public:

   void testVectorConversion()
   {
      VE_TextureBased::VectorData vd1;
      VE_TextureBased::VectorData vd2(gmtl::Vec3f(1.0f, 2.0f, 3.0f));
      VE_TextureBased::VectorData vd3(std::string("Foo"), 
                                      gmtl::Vec3f(4.0f, 5.0f, 6.0f));
      gmtl::Vec3f v1(vd1);
      gmtl::Vec3f v2 = vd2;
      gmtl::Vec3f v3;
      v3 = vd3;
      TS_ASSERT_DELTA(v1[0], 0.0f, 0.0000001);
      TS_ASSERT_DELTA(v1[1], 0.0f, 0.0000001);
      TS_ASSERT_DELTA(v1[2], 0.0f, 0.0000001);
      
      TS_ASSERT_DELTA(v2[0], 1.0f, 0.0000001);
      TS_ASSERT_DELTA(v2[1], 2.0f, 0.0000001);
      TS_ASSERT_DELTA(v2[2], 3.0f, 0.0000001);

      TS_ASSERT_DELTA(v3[0], 4.0f, 0.0000001);
      TS_ASSERT_DELTA(v3[1], 5.0f, 0.0000001);
      TS_ASSERT_DELTA(v3[2], 6.0f, 0.0000001);
   }
};

#endif
