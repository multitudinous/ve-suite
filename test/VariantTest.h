#ifndef JPG_VARIANT_TEST_H_
#define JPG_VARIANT_TEST_H_

#include "ves/xplorer/volume/Variant.h"
#include "ves/xplorer/volume/Database.h"

#include <loki/Typelist.h>
#include <loki/Visitor.h>
#include <cxxtest/TestSuite.h>

#include <string>

// Test Variant Type
typedef Loki::Variant<LOKI_TYPELIST_4(long, float, VE_TextureBased::BinaryData*, std::string)> TestVariant;
/**
 * Visitor Implementation that handles each value type for the Variant test.
 */
class VariantTester : public TestVariant::StrictVisitor
{
public:
   VariantTester()
      : mLongData(0), mFloatData(0), mBinData(0)
   {}

   virtual void Visit(long& data)
   {
      mLongData = data; 
   }

   virtual void Visit(float& data)
   {
      mFloatData = data;
   }

   virtual void Visit(VE_TextureBased::BinaryData*& data)
   {
      mBinData = data;
   }

   virtual void Visit(std::string& data)
   {
      mStringData = data;
   }

   long                                   mLongData;
   float                                  mFloatData;
   VE_TextureBased::BinaryData*                       mBinData;
   std::string                            mStringData;
};

class VariantTest : public CxxTest::TestSuite
{
public:

   void testAccept()
   {
      long int r1 = 1000L;
      float r2 = 0.33;
      VE_TextureBased::BinaryData* r3 = new VE_TextureBased::BinaryData(8);
      std::string r4 = "FUBAR";
      TestVariant v1(r1);
      TestVariant v2(r2);
      TestVariant v3(r3);
      TestVariant v4(r4);
      VariantTester vt;
      // Visit Each Variant.
      v1.Accept(vt);
      v2.Accept(vt);
      v3.Accept(vt);
      v4.Accept(vt);

      TS_ASSERT_EQUALS(vt.mLongData, r1);
      TS_ASSERT_EQUALS(vt.mFloatData, r2);
      TS_ASSERT_EQUALS(vt.mBinData, r3);
      TS_ASSERT_EQUALS(vt.mStringData, r4);
   }

};

#endif
