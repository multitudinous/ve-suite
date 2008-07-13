
#ifndef VE_JULIUS_NETWORK_CLIENT_TEST_H_
#define VE_JULIUS_NETWORK_CLIENT_TEST_H_

#include "apps/voice/JuliusNetworkClient.h"

#include <cxxtest/TestSuite.h>

class JuliusNetworkClientTest : public CxxTest::TestSuite
{
public:

   void testConnect()
   {
      JuliusNetworkClient* jnc = new JuliusNetworkClient();
      TS_ASSERT(!jnc->isConnected());
      TS_ASSERT(jnc->connect(std::string("localhost")));
      TS_ASSERT(jnc->isConnected());
      delete jnc;
   }

   void testStartDataLoop()
   {
      JuliusNetworkClient* jnc = new JuliusNetworkClient();
      JuliusXMLParserPtr parser = new JuliusXMLParser();
      TS_ASSERT_EQUALS(jnc->startDataLoop(), false);
      TS_ASSERT(jnc->connect(std::string("localhost")));
      TS_ASSERT_EQUALS(jnc->startDataLoop(), false);
      jnc->disconnect();
      jnc->setParser(parser);
      TS_ASSERT_EQUALS(jnc->startDataLoop(), false);
      delete jnc;
   }

};

#endif
