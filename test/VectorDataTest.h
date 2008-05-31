#ifndef JPG_VECTOR_DATA_TEST_H_
#define JPG_VECTOR_DATA_TEST_H_

#include "ves/xplorer/volume/VectorData.h"

#include <gmtl/Vec.h>
#include <gmtl/VecOps.h>
#include <cxxtest/TestSuite.h>

#include <string>

class VectorDataTest : public CxxTest::TestSuite
{
public:

   void testAddVectorValues()
   {
      VE_TextureBased::VectorDataSet vds;
      gmtl::Vec3f v1(1.0f, 2.0f, 3.0f);
      gmtl::Vec3f v2(4.0f, 5.0f, 6.0f);
      std::vector<gmtl::Vec3f> vd;
      vd.push_back(v1);
      vd.push_back(v2);
      vds.setVector("Foo", vd);
      TS_ASSERT(vds.hasVector("Foo"));
      TS_ASSERT_EQUALS(vds.size("Foo"), 2);
      std::vector<gmtl::Vec3f> r1 = vds.getVector("Foo");
      TS_ASSERT_EQUALS(r1.size(), 2);
      TS_ASSERT_EQUALS(r1[0], v1);
      TS_ASSERT_EQUALS(r1[1], v2);
   }

   void testSetVectorValue()
   {
      VE_TextureBased::VectorDataSet vds;
      gmtl::Vec3f v1(1.0f, 2.0f, 3.0f);
      gmtl::Vec3f v2(4.0f, 5.0f, 6.0f);
      gmtl::Vec3f v3(7.0f, 8.0f, 9.0f);
      std::vector<gmtl::Vec3f> vd;
      vd.push_back(v1);
      vd.push_back(v2);
      vds.setVector("Foo", vd);
      TS_ASSERT(vds.hasVector("Foo"));
      TS_ASSERT_EQUALS(vds.size("Foo"), 2);
      vds.setVector("Foo", 1, v3);
      TS_ASSERT_EQUALS(vds.getVector("Foo", 0), v1);
      TS_ASSERT_EQUALS(vds.getVector("Foo", 1), v3);
   }

   void testSetVectorTimestepValues()
   {
      VE_TextureBased::VectorDataSet vds;
      gmtl::Vec3f v1(1.0f, 2.0f, 3.0f);
      gmtl::Vec3f v2(4.0f, 5.0f, 6.0f);
      gmtl::Vec3f v3(7.0f, 8.0f, 9.0f);
      std::map<std::string, gmtl::Vec3f> tsd;
      tsd["Foo"] = v1;
      tsd["Bar"] = v2;
      tsd["Baz"] = v3;
      TS_ASSERT(vds.setVector(0, tsd));
      std::map<std::string, gmtl::Vec3f> tsd2;
      tsd2["Foo"] = v3;
      tsd2["Bar"] = v2;
      tsd2["Baz"] = v1;
      TS_ASSERT(vds.setVector(1, tsd2));
      TS_ASSERT(vds.hasVector("Foo"));
      TS_ASSERT(vds.hasVector("Bar"));
      TS_ASSERT(vds.hasVector("Baz"));
      std::vector<gmtl::Vec3f> r1 = vds.getVector("Foo");
      std::vector<gmtl::Vec3f> r2 = vds.getVector("Bar");
      std::vector<gmtl::Vec3f> r3 = vds.getVector("Baz");
      TS_ASSERT_EQUALS(r1.size(), 2);
      TS_ASSERT_EQUALS(r1[0], v1);
      TS_ASSERT_EQUALS(r1[1], v3);
      TS_ASSERT_EQUALS(r2.size(), 2);
      TS_ASSERT_EQUALS(r2[0], v2);
      TS_ASSERT_EQUALS(r2[1], v2);
      TS_ASSERT_EQUALS(r3.size(), 2);
      TS_ASSERT_EQUALS(r3[0], v3);
      TS_ASSERT_EQUALS(r3[1], v1);
   }

   void testGetVectorTimestepValues()
   {
      VE_TextureBased::VectorDataSet vds;
      gmtl::Vec3f v1(1.0f, 2.0f, 3.0f);
      gmtl::Vec3f v2(4.0f, 5.0f, 6.0f);
      gmtl::Vec3f v3(7.0f, 8.0f, 9.0f);
      std::map<std::string, gmtl::Vec3f> tsd;
      tsd["Foo"] = v1;
      tsd["Bar"] = v2;
      tsd["Baz"] = v3;
      TS_ASSERT(vds.setVector(0, tsd));
      std::map<std::string, gmtl::Vec3f> tsd2;
      tsd2["Foo"] = v3;
      tsd2["Bar"] = v2;
      tsd2["Baz"] = v1;
      TS_ASSERT(vds.setVector(1, tsd2));
      TS_ASSERT(vds.hasVector("Foo"));
      TS_ASSERT(vds.hasVector("Bar"));
      TS_ASSERT(vds.hasVector("Baz"));

   }

   void testGetNames()
   {
      VE_TextureBased::VectorDataSet vds;
      gmtl::Vec3f v1(1.0f, 2.0f, 3.0f);
      gmtl::Vec3f v2(4.0f, 5.0f, 6.0f);
      gmtl::Vec3f v3(7.0f, 8.0f, 9.0f);
      std::map<std::string, gmtl::Vec3f> tsd;
      tsd["Foo"] = v1;
      tsd["Bar"] = v2;
      tsd["Baz"] = v3;
      TS_ASSERT(vds.setVector(0, tsd));

      std::vector<std::string> names = vds.getVectorNames();
      TS_ASSERT_EQUALS(names[0], "Bar");
      TS_ASSERT_EQUALS(names[1], "Baz");
      TS_ASSERT_EQUALS(names[2], "Foo");

   }

};

#endif
