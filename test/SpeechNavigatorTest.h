
#ifndef VE_SPEECH_NAVIGATOR_TEST_H_
#define VE_SPEECH_NAVIGATOR_TEST_H_

#include "apps/voice/SpeechNavigator.h"

#include <cxxtest/TestSuite.h>

class SpeechNavigatorTest : public CxxTest::TestSuite
{
public:

   void setUp()
   {
      mNavigator = new SpeechNavigator();
   }

   void tearDown()
   {
      delete mNavigator;
      mNavigator = NULL;
   }

   void testConnectToJulius()
   {
      TS_ASSERT(mNavigator->connectToJulius(std::string("localhost"))); 
   }

   void testConnectToXplorer()
   {
      char* argv[] = {"TestRunner", "-ORBInitRef", 
                      "NameService=corbaloc:iiop:localhost:1239/NameService"};
      int argc = 3;
      mNavigator->setArgcArgv(argc, argv);
      TS_ASSERT(mNavigator->connectToXplorer());
   }

private:

   SpeechNavigator*                                mNavigator;
};

#endif
